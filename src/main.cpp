#include <SDL2/SDL.h>
#include <cstring>
#include <sys/types.h>

#define BYTECODE_READER_IMPLEMENTATION
#include "reader.hpp"

#include <cassert>
#include <chrono>
#include <csignal>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <fcntl.h>
#include <thread>
#include <unistd.h>

const int SCREEN_WIDTH = 64;
const int SCREEN_HEIGHT = 32;
const int SCALE = 5;
static size_t RAM_SIZE = 0x1000;
const int TARGET_CYCLES_PER_SECOND = 500;
const int TARGET_MS_PER_CYCLE = 1000 / TARGET_CYCLES_PER_SECOND;
const int TARGET_MS_PER_FRAME = 1000 / 60;
const int BG_COLOR = 0x081820;
const int FG_COLOR = 0x88c070;

void draw(SDL_Renderer *renderer, SDL_Texture *texture,
          bool framebuffer[SCREEN_HEIGHT][SCREEN_WIDTH]) {

    printf("Drawing...\n");

    uint32_t pixels[SCREEN_WIDTH * SCREEN_HEIGHT];
    for (int i = 0; i < SCREEN_HEIGHT; i++) {
        for (int j = 0; j < SCREEN_WIDTH; j++) {
            pixels[i * SCREEN_WIDTH + j] =
                framebuffer[i][j] ? FG_COLOR : BG_COLOR;
        }
    }

    SDL_UpdateTexture(texture, nullptr, pixels,
                      SCREEN_WIDTH * sizeof(uint32_t));
    SDL_RenderClear(renderer);
    SDL_RenderCopy(renderer, texture, nullptr, nullptr);
    SDL_RenderPresent(renderer);
}

static uint8_t FONT[0x10][0x05] = {
    {0xF0, 0x90, 0x90, 0x90, 0xF0}, // 0
    {0x20, 0x60, 0x20, 0x20, 0x70}, // 1
    {0xF0, 0x10, 0xF0, 0x80, 0xF0}, // 2
    {0xF0, 0x10, 0xF0, 0x10, 0xF0}, // 3
    {0x90, 0x90, 0xF0, 0x10, 0x10}, // 4
    {0xF0, 0x80, 0xF0, 0x10, 0xF0}, // 5
    {0xF0, 0x80, 0xF0, 0x90, 0xF0}, // 6
    {0xF0, 0x10, 0x20, 0x40, 0x40}, // 7
    {0xF0, 0x90, 0xF0, 0x90, 0xF0}, // 8
    {0xF0, 0x90, 0xF0, 0x10, 0xF0}, // 9
    {0xF0, 0x90, 0xF0, 0x90, 0x90}, // A
    {0xE0, 0x90, 0xE0, 0x90, 0xE0}, // B
    {0xF0, 0x80, 0x80, 0x80, 0xF0}, // C
    {0xE0, 0x90, 0x90, 0x90, 0xE0}, // D
    {0xF0, 0x80, 0xF0, 0x80, 0xF0}, // E
    {0xF0, 0x80, 0xF0, 0x80, 0x80}, // F
};

class Chip8 {
  public:
    Chip8(char *rom_path) {
        int rom_fd = open(rom_path, O_RDONLY);
        if (rom_fd < 0) {
            printf("Failed to open file: %s\n", rom_path);
            exit(1);
        }

        pc = 0x200;
        ram = (uint8_t *)malloc(RAM_SIZE);
        if (ram == NULL) {
            printf("Failed to allocate ram!");
            exit(1);
        }

        memcpy(ram, FONT, sizeof(FONT));

        int file_size = lseek(rom_fd, 0, SEEK_END);
        (void)lseek(rom_fd, 0, SEEK_SET);

        printf("Reading file: %s for %d bytes\n", rom_path, file_size);

        int err = read(rom_fd, ram + 0x200, file_size);
        if (err < 0) {
            printf("Failed to read file: %s\n", rom_path);
            exit(1);
        }

        if (err != file_size) {
            printf("Failed to read file: %s\n", rom_path);
            exit(1);
        }

        close(rom_fd);

        stack = (uint16_t *)malloc(sizeof(uint16_t) * 16);
        if (stack == NULL) {
            printf("Failed to allocate stack!");
            exit(1);
        }
    }

    ~Chip8() {
        free(ram);
        free(stack);
    }

    int run();
    void view_ram();
    void dump_ram();

    int is_protected(size_t addr) { return addr < 0x200; }

    int read_mem(size_t addr) {
        if (is_protected(addr)) {
            printf("Attempted to read from protected address: 0x%04x\n",
                   (unsigned int)addr);
            dump_ram();
            exit(1);
        }
        return this->ram[addr];
    }

    void write_mem(size_t addr, uint8_t val) {
        if (is_protected(addr)) {
            printf("Attempted to write to protected address: 0x%04x\n",
                   (unsigned int)addr);
            dump_ram();
            exit(1);
        }
        this->ram[addr] = val;
    }

    void set_sound_timer(uint8_t val) {
        // enable buzzer
        this->sound_timer = val;
    }

    void set_pixel(int x, int y, uint8_t val) {
        assert(x >= 0 && x < SCREEN_WIDTH);
        assert(y >= 0 && y < SCREEN_HEIGHT);
        this->fb[y][x] = val;
    }

    uint8_t get_pixel(int x, int y) {
        assert(x >= 0 && x < SCREEN_WIDTH);
        assert(y >= 0 && y < SCREEN_HEIGHT);
        return this->fb[y][x];
    }

  private:
    uint8_t *ram;
    bool fb[SCREEN_HEIGHT][SCREEN_WIDTH];
    uint16_t pc;
    uint16_t *stack;
    uint8_t sp;
    uint8_t v[16];
    uint16_t i;
    uint8_t delay;
    uint8_t sound_timer;
    bool compat;
};

int Chip8::run() {
    using namespace std::chrono;

    int exit = 0;
    constexpr auto cycle_time = milliseconds(TARGET_MS_PER_CYCLE);
    constexpr auto timer_interval = milliseconds(TARGET_MS_PER_FRAME);
    auto last_timer_update = high_resolution_clock::now();

    if (SDL_Init(SDL_INIT_VIDEO) < 0) {
        printf("Failed to initialize SDL: %s\n", SDL_GetError());
        return 1;
    }

    SDL_Window *window = SDL_CreateWindow(
        "CHIP-8 Emulator", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
        SCREEN_WIDTH * SCALE, SCREEN_HEIGHT * SCALE, SDL_WINDOW_SHOWN);
    SDL_Renderer *renderer =
        SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED);
    SDL_Texture *texture = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_RGBA8888,
                                             SDL_TEXTUREACCESS_STREAMING,
                                             SCREEN_WIDTH, SCREEN_HEIGHT);

    bool running = true;
    SDL_Event event;

    while (running) {
        while (SDL_PollEvent(&event)) {
            if (event.type == SDL_QUIT) {
                running = false;
            }
        }

        auto start_time = high_resolution_clock::now();

        uint16_t op = (read_mem(pc) << 8) | read_mem(pc + 1);
        pc += 2;
        printf("PC: 0x%04x OP: 0x%04x\n", pc, op);
        Bytecode bytecode = parse(op);

        printf("OPCODE: 0x%04x INSTRUCTION_TYPE: %d\n", op,
               bytecode.instruction_type);

        switch (bytecode.instruction_type) {
        case EXIT: {
            // From Peter Miller's chip8run. Exit emulator with a return value
            // of N.
            exit = bytecode.operand.byte;
            running = false;
            break;
        }
        case SYS: {
            //             Jump to a machine code routine at nnn.
            // This instruction is only used on the old computers on which
            // Chip-8 was originally implemented. It is ignored by modern
            // interpreters.
            uint16_t addr = bytecode.operand.word & 0x0FFF;
            assert(addr < RAM_SIZE && addr % 0x2 == 0);

            pc = bytecode.operand.word & 0x0FFF;
            break;
        }
        case CLS: {
            //             Clear the screen.
            // fprintf(stderr, "CLS not implemented\n");
            memset(this->fb, 0, SCREEN_WIDTH * SCREEN_HEIGHT);
            break;
        }
        case RET: {
            //             Return from a subroutine.
            // The interpreter sets the program counter to the address at the
            // top of the stack, then subtracts 1 from the stack pointer.

            pc = stack[--sp];
            break;
        }
        case JP: {
            //             Jump to location nnn.
            // The interpreter sets the program counter to nnn.

            uint16_t addr = bytecode.operand.word & 0x0FFF;
            assert(addr < RAM_SIZE && addr % 0x2 == 0);

            pc = bytecode.operand.word & 0x0FFF;
            break;
        }
        case CALL: {
            //             Call subroutine at nnn.
            // The interpreter increments the stack pointer, then puts the
            // current PC on the top of the stack. The PC is then set to nnn.

            stack[++sp] = pc;
            pc = bytecode.operand.word & 0x0FFF;
            break;
        }
        case SKIP_INSTRUCTION_BYTE: {
            //             Skip next instruction if Vx = kk.
            // The interpreter compares register Vx to kk, and if they are
            // equal, increments the program counter by 2.
            if (this->v[bytecode.operand.byte_reg.reg] ==
                bytecode.operand.byte_reg.byte) {
                pc += 2;
            }

            break;
        }
        case SKIP_INSTRUCTION_NE_BYTE: {
            //             Skip next instruction if Vx != kk.
            // The interpreter compares register Vx to kk, and if they are not
            // equal, increments the program counter by 2.
            if (this->v[bytecode.operand.byte_reg.reg] !=
                bytecode.operand.byte_reg.byte) {
                pc += 2;
            }

            break;
        }
        case SKIP_INSTRUCTION_REG: {
            //             Skip next instruction if Vx = Vy.
            // The interpreter compares register Vx to register Vy, and if they
            // are equal, increments the program counter by 2.
            if (this->v[bytecode.operand.reg_reg.x] ==
                this->v[bytecode.operand.reg_reg.y]) {
                pc += 2;
            }
            break;
        }
        case SKIP_INSTRUCTION_NE_REG: {
            //             Skip next instruction if Vx != Vy.
            // The values of Vx and Vy are compared, and if they are not equal,
            // the program counter is increased by 2.
            if (this->v[bytecode.operand.reg_reg.x] !=
                this->v[bytecode.operand.reg_reg.y]) {
                pc += 2;
            }
            break;
        }
        case LOAD_BYTE: {
            //             Set Vx = kk.
            // The interpreter puts the value kk into register Vx.
            this->v[bytecode.operand.byte_reg.reg] =
                bytecode.operand.byte_reg.byte;
            break;
        }
        case ADD_BYTE: {
            //             Set Vx = Vx + kk.
            // Adds the value kk to the value of register Vx, then stores the
            // result in Vx.
            this->v[bytecode.operand.byte_reg.reg] +=
                bytecode.operand.byte_reg.byte;
            break;
        }
        case LOAD_REG: {
            //             Set Vx = Vy.
            // Stores the value of register Vy in register Vx.
            this->v[bytecode.operand.reg_reg.x] =
                this->v[bytecode.operand.reg_reg.y];
            break;
        }
        case ADD_REG: {
            //             Set Vx = Vx + Vy, set VF = carry.
            // The values of Vx and Vy are added together. If the result is
            // greater than 8 bits (i.e., > 255,) VF is set to 1, otherwise 0.
            // Only the lowest 8 bits of the result are kept, and stored in Vx.
            int result = this->v[bytecode.operand.reg_reg.x] +
                         this->v[bytecode.operand.reg_reg.y];
            this->v[bytecode.operand.reg_reg.x] = result & 0xFF;
            this->v[0xF] = result > 0xFF;

            break;
        }
        case SUB_REG: {
            //             Set Vx = Vx - Vy, set VF = NOT borrow.
            // If Vx > Vy, then VF is set to 1, otherwise 0. Then Vy is
            // subtracted from Vx, and the results stored in Vx.
            int result = this->v[bytecode.operand.reg_reg.x] -
                         this->v[bytecode.operand.reg_reg.y];
            this->v[bytecode.operand.reg_reg.x] = result & 0xFF;
            this->v[0xF] = result < 0;
            break;
        }
        case OR_REG: {
            //             Set Vx = Vx OR Vy.
            // Performs a bitwise OR on the values of Vx and Vy, then stores the
            // result in Vx. A bitwise OR compares the corrseponding bits from
            // two values, and if either bit is 1, then the same bit in the
            // result is also 1. Otherwise, it is 0.
            this->v[bytecode.operand.reg_reg.x] =
                this->v[bytecode.operand.reg_reg.x] |
                this->v[bytecode.operand.reg_reg.y];
            break;
        }
        case AND_REG: {
            //             Set Vx = Vx AND Vy.
            // Performs a bitwise AND on the values of Vx and Vy, then stores
            // the result in Vx. A bitwise AND compares the corrseponding bits
            // from two values, and if both bits are 1, then the same bit in the
            // result is also 1. Otherwise, it is 0.
            this->v[bytecode.operand.reg_reg.x] =
                this->v[bytecode.operand.reg_reg.x] &
                this->v[bytecode.operand.reg_reg.y];
            break;
        }
        case XOR_REG: {
            //             Set Vx = Vx XOR Vy.
            // Performs a bitwise exclusive OR on the values of Vx and Vy, then
            // stores the result in Vx. An exclusive OR compares the
            // corrseponding bits from two values, and if the bits are not both
            // the same, then the corresponding bit in the result is set to 1.
            // Otherwise, it is 0.
            this->v[bytecode.operand.reg_reg.x] =
                this->v[bytecode.operand.reg_reg.x] ^
                this->v[bytecode.operand.reg_reg.y];
            break;
        }
        case SHR_REG: {
            //             Set Vx = Vx SHR 1.
            // If the least-significant bit of Vx is 1, then VF is set to 1,
            // otherwise 0. Then Vx is divided by 2.
            this->v[0xF] = this->v[bytecode.operand.reg_reg.x] & 1;
            this->v[bytecode.operand.reg_reg.x] =
                this->v[bytecode.operand.reg_reg.x] >> 1;
            break;
        }
        case SUBN_REG: {
            //             Set Vx = Vy - Vx, set VF = NOT borrow.
            // If Vy > Vx, then VF is set to 1, otherwise 0. Then Vx is
            // subtracted from Vy, and the results stored in Vx.
            int result = this->v[bytecode.operand.reg_reg.y] -
                         this->v[bytecode.operand.reg_reg.x];
            this->v[bytecode.operand.reg_reg.x] = result & 0xFF;
            this->v[0xF] = result < 0;
            break;
        }
        case SHL_REG: {
            //             Set Vx = Vx SHL 1.
            // If the most-significant bit of Vx is 1, then VF is set to 1,
            // otherwise to 0. Then Vx is multiplied by 2.
            this->v[0xF] = this->v[bytecode.operand.reg_reg.x] >> 7;
            this->v[bytecode.operand.reg_reg.x] =
                this->v[bytecode.operand.reg_reg.x] << 1;
            break;
        }
        case LOAD_I_BYTE: {
            // Set I = nnn.
            // The value of register I is set to nnn.
            this->i = bytecode.operand.word & 0x0FFF;
            break;
        }
        case JP_V0_BYTE: {
            //             Jump to location nnn + V0.
            // The program counter is set to nnn plus the value of V0.
            pc = (bytecode.operand.word & 0x0FFF) + this->v[0];
            break;
        }
        case RND: {
            //             Set Vx = random byte AND kk.
            // The interpreter generates a random number from 0 to 255, which is
            // then ANDed with the value kk. The results are stored in Vx. See
            // instruction 8xy2 for more information on AND.
            this->v[bytecode.operand.byte_reg.reg] =
                static_cast<uint8_t>(rand()) & bytecode.operand.byte_reg.byte;
            break;
        }
        case DRW: {
            //             Display n-byte sprite starting at memory location I
            //             at (Vx, Vy), set VF = collision.
            // The interpreter reads n bytes from memory, starting at the
            // address stored in I. These bytes are then displayed as sprites on
            // screen at coordinates (Vx, Vy). Sprites are XORed onto the
            // existing screen. If this causes any pixels to be erased, VF is
            // set to 1, otherwise it is set to 0. If the sprite is positioned
            // so part of it is outside the coordinates of the display, it wraps
            // around to the opposite side of the screen. See instruction 8xy3
            // for more information on XOR, and section 2.4, Display, for more
            // information on the Chip-8 screen and sprites.
            // fprintf(stderr, "DRW not implemented\n");
            this->v[0x0F] = 0;

            for (int i = 0; i < bytecode.operand.reg_reg_nibble.nibble; i++) {
                uint8_t sprite = read_mem(this->i + i);

                for (int j = 0; j < 8; j++) {
                    bool source = (sprite >> (7 - j)) & 0x1;
                    int x = (this->v[bytecode.operand.reg_reg_nibble.x] + j) %
                            SCREEN_WIDTH;
                    int y = (this->v[bytecode.operand.reg_reg_nibble.y] + i) %
                            SCREEN_HEIGHT;

                    printf("Sprite %d, bit %d %d\n", i, j,
                           sprite & (0x80 >> j));
                    if (!source) {
                        continue;
                    }

                    if (get_pixel(x, y)) {
                        set_pixel(x, y, 0);
                        this->v[0x0F] = 1;
                    } else {
                        set_pixel(x, y, 1);
                    }
                }
            }
            break;
        }
        case SKIP_PRESSED_REG: {
            //             Skip next instruction if key with the value of Vx is
            //             pressed.
            // Checks the keyboard, and if the key corresponding to the value of
            // Vx is currently in the down position, PC is increased by 2.
            fprintf(stderr, "SKIP_PRESSED_REG not implemented\n");
            break;
        }
        case SKIP_NOT_PRESSED_REG: {
            //             Skip next instruction if key with the value of Vx is
            //             not pressed.
            // Checks the keyboard, and if the key corresponding to the value of
            // Vx is currently in the up position, PC is increased by 2.
            fprintf(stderr, "SKIP_NOT_PRESSED_REG not implemented\n");
            break;
        }
        case LD_REG_DT: {
            //             Set Vx = delay timer value.
            // The value of DT is placed into Vx.
            this->v[bytecode.operand.reg_reg.x] = this->delay;
            break;
        }
        case LD_REG_K: {
            //             Wait for a key press, store the value of the key in
            //             Vx.
            // All execution stops until a key is pressed, then the value of
            // that key is stored in Vx.
            fprintf(stderr, "LD_REG_K not implemented\n");
            break;
        }
        case LD_DT_REG: {
            //             Set delay timer = Vx.
            // DT is set equal to the value of Vx.
            this->delay = this->v[bytecode.operand.reg_reg.x];
            break;
        }
        case LD_ST_REG: {
            //             Set sound timer = Vx.
            // ST is set equal to the value of Vx.
            set_sound_timer(bytecode.operand.byte);
            break;
        }
        case ADD_I_REG: {
            //             Set I = I + Vx.
            // The values of I and Vx are added, and the results are stored in
            // I.
            this->i += this->v[bytecode.operand.byte];
            break;
        }
        case LD_F_REG: {
            //             Set I = location of sprite for digit Vx.
            // The value of I is set to the location for the hexadecimal sprite
            // corresponding to the value of Vx. See section 2.4, Display, for
            // more information on the Chip-8 hexadecimal font.

            //? This is the ONLY spot where the emulator is allowed to access
            //? 0x0000-0x01FF of the RAM. Since that area of RAM is reserved for
            //? the emulator's own use.
            this->i = (uint16_t)(bytecode.operand.byte * 5);
            break;
        }
        case LD_B_REG: {
            //             Store BCD representation of Vx in memory locations I,
            //             I+1, and I+2.
            // The interpreter takes the decimal value of Vx, and places the
            // hundreds digit in memory at location in I, the tens digit at
            // location I+1, and the ones digit at location I+2.
            // TODO: is this correct?
            this->ram[this->i] =
                (uint8_t)((this->v[bytecode.operand.reg_reg.x] / 100) & 0x0F);
            this->ram[this->i + 1] =
                (uint8_t)((this->v[bytecode.operand.reg_reg.x] % 100) / 10) &
                0x0F;
            this->ram[this->i + 2] =
                (uint8_t)(this->v[bytecode.operand.reg_reg.x] % 10) & 0x0F;
            break;
        }
        case LD_PTR_I_REG: {
            //             Store registers V0 through Vx in memory starting at
            //             location I.
            // The interpreter copies the values of registers V0 through Vx into
            // memory, starting at the address in I.
            for (int i = 0; i < bytecode.operand.reg_reg.x; i++) {
                this->ram[this->i + i] = this->v[i];
            }
            break;
        }
        case LD_REG_PTR_I: {
            //             Read registers V0 through Vx from memory starting at
            //             location I.
            // The interpreter reads values from memory starting at location I
            // into registers V0 through Vx.
            for (int i = 0; i < bytecode.operand.reg_reg.x; i++) {
                this->v[i] = this->ram[this->i + i];
            }
            break;
        }
        case UNKNOWN_INSTRUCTION: {
            fprintf(stderr, "Unknown instruction type: %d\n",
                    bytecode.instruction_type);
            exit = 1;
            running = false;
            break;
        }
        }

        printf("Emulating...\n");

        auto now = high_resolution_clock::now();
        if (duration_cast<milliseconds>(now - last_timer_update) >=
            timer_interval) {
            printf("Updating...\n");
            if (delay > 0)
                --delay;
            if (sound_timer > 0)
                --sound_timer;
            draw(renderer, texture, this->fb);
            last_timer_update = now;
        }

        auto elapsed_time = duration_cast<milliseconds>(
            high_resolution_clock::now() - start_time);
        if (elapsed_time < cycle_time) {
            std::this_thread::sleep_for(cycle_time - elapsed_time);
        }
    }

    SDL_DestroyTexture(texture);
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit();

    return exit;
}

void Chip8::view_ram() {
    printf("Hex dump:\n");
    for (size_t i = 0; i < RAM_SIZE / 16; i++) {
        size_t j = 0;
        for (; j < 16; j++) {
            printf("%02x ", this->ram[i * 16 + j]);
        }
        printf(" |");
        j = 0;
        for (; j < 16; j++) {
            if (this->ram[i * 16 + j] >= 32 && this->ram[i * 16 + j] <= 126) {
                printf("%c", this->ram[i * 16 + j]);
            } else {
                printf(".");
            }
        }
        printf("|\n");
    }
}

void Chip8::dump_ram() {
    (void)remove("ram.bin");

    FILE *fp = fopen("ram.bin", "wb");
    if (fp == NULL) {
        printf("Failed to open file\n");
        exit(1);
    }

    fwrite(this->ram, RAM_SIZE, 1, fp);
    fclose(fp);
}

int main(int argc, char **argv) {
    signal(SIGINT, exit);

    if (argc < 2) {
        printf("Usage: %s <file>\n", argv[0]);
        return 1;
    }

    Chip8 chip8 = Chip8(argv[1]);
    chip8.view_ram();
    int ret = chip8.run();

    return ret;
}
