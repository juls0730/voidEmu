#include <SDL2/SDL.h>
#include <SDL2/SDL_audio.h>
#include <SDL2/SDL_keycode.h>
#include <SDL2/SDL_render.h>
#include <SDL2/SDL_video.h>
#include <atomic>
#include <condition_variable>
#include <cstddef>
#include <cstring>
#include <mutex>
#include <sys/types.h>
#include <thread>

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

const size_t RAM_SIZE = 0x1000;
const int TARGET_CYCLES_PER_SECOND = 500;
const int TARGET_MS_PER_CYCLE = 1000 / TARGET_CYCLES_PER_SECOND;
const int TARGET_FRAMERATE = 60;
const int TARGET_MS_PER_FRAME = 1000 / TARGET_FRAMERATE;
const int TARGET_MS_PER_TICK = 1000 / 60;
static int SCREEN_WIDTH = 64;
static int SCREEN_HEIGHT = 32;
static int SCALE = 10;
static int BG_COLOR = 0x081820;
static int FG_COLOR = 0x88c070;

// Spcae Invaders by David Winter uses misaligned addresses, so we might not
// always want to align pc
#define ALIGN_PC true
// the SYS instruction technically shouldnt be used, so it can be compiled out.
#define SYS_INSTRUCTION false

const int SAMPLE_RATE = 44100;
const int FREQUENCY = 440; // A4
const int AMPLITUDE = 28000;
const int SAMPLES_PER_CYCLE = SAMPLE_RATE / FREQUENCY;

void audioCallback(void *userdata, Uint8 *stream, int len) {
    (void)userdata;
    static int phase = 0;
    Sint16 *buffer = (Sint16 *)stream;
    int length = len / 2;

    for (int i = 0; i < length; i++) {
        buffer[i] = (phase < SAMPLES_PER_CYCLE / 4) ? AMPLITUDE : -AMPLITUDE;
        phase = (phase + 1) % SAMPLES_PER_CYCLE;
    }
}

void initAudio() {
    SDL_AudioSpec want, have;
    SDL_zero(want);
    want.freq = SAMPLE_RATE;
    want.format = AUDIO_S16SYS;
    want.channels = 1;
    want.samples = 2048; // Buffer size
    want.callback = audioCallback;

    if (SDL_OpenAudio(&want, &have) < 0) {
        SDL_Log("Failed to open audio: %s", SDL_GetError());
    }
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

void clear_framebuffer(bool **fb, int width, int height) {
    // since the pixel array is just one big array, we can just memset the
    // first element of the array and that will clear the entire pixel array
    memset(fb[0], 0, width * height);
}

bool **allocate_framebuffer(int width, int height) {
    // allocate the frambuffer, which is a pointer to an array of booleans, the
    // pointer to the arrays should be height elements long. The boolean arrays
    // should be wdith elements long.
    bool **fb = (bool **)malloc(height * sizeof(bool *));
    if (fb == NULL) {
        return NULL;
    }

    bool *pixel_array = (bool *)malloc(width * height * sizeof(bool));
    if (pixel_array == NULL) {
        return NULL;
    }

    for (int y = 0; y < height; y++) {
        fb[y] = pixel_array + (y * width);
    }

    clear_framebuffer(fb, width, height);

    return fb;
}

void free_framebuffer(bool **fb) {
    // since the pixel array is just one big array, we can just free the
    // first element of the array and that will free the entire pixel array
    free(fb[0]);
    free(fb);
}

class Chip8 {
  public:
    Chip8(char *rom_path) {
        int rom_fd = open(rom_path, O_RDONLY);
        if (rom_fd < 0) {
            fprintf(stderr, "Failed to open file: %s\n", rom_path);
            exit(1);
        }

        ram = (uint8_t *)malloc(RAM_SIZE);
        if (ram == NULL) {
            fprintf(stderr, "Failed to allocate ram!");
            exit(1);
        }

        memcpy(ram, FONT, sizeof(FONT));

        int file_size = lseek(rom_fd, 0, SEEK_END);
        (void)lseek(rom_fd, 0, SEEK_SET);

        printf("Reading file: %s for %d bytes\n", rom_path, file_size);

        int err = read(rom_fd, ram + 0x200, file_size);
        if (err < 0) {
            fprintf(stderr, "Failed to read file: %s\n", rom_path);
            exit(1);
        }

        if (err != file_size) {
            fprintf(stderr, "Failed to read file: %s\n", rom_path);
            exit(1);
        }

        close(rom_fd);

        stack = (uint16_t *)malloc(sizeof(uint16_t) * 16);
        if (stack == NULL) {
            fprintf(stderr, "Failed to allocate stack!");
            exit(1);
        }

        fb = allocate_framebuffer(SCREEN_WIDTH, SCREEN_HEIGHT);
        fb_length = SCREEN_HEIGHT * SCREEN_WIDTH;

        if (fb == NULL) {
            fprintf(stderr, "Failed to allocate framebuffer!\n");
            exit(1);
        }
    }

    ~Chip8() {
        free(ram);
        free(stack);
        free_framebuffer(fb);
    }

    int run();
    void view_ram();
    void dump_ram();
    void view_stack();

    // only allow addresses in the program space to be executed, addresses
    // not protected, but in the reserved space, for example, the font,
    // should not be executable
    bool is_executable(size_t addr) { return addr > 0x1FF; }

    int read_mem(size_t addr) { return this->ram[addr]; }

    void write_mem(size_t addr, uint8_t val) { this->ram[addr] = val; }

    void set_sound_timer(uint8_t val) { this->sound_timer = val; }

    void set_pixel(int x, int y, uint8_t val) {
        assert(fb != NULL);
        assert(x >= 0 && x < SCREEN_WIDTH);
        assert(y >= 0 && y < SCREEN_HEIGHT);
        assert(this->fb[y] != NULL);
        this->fb[y][x] = val;
    }

    uint8_t get_pixel(int x, int y) {
        assert(fb != NULL);
        assert(x >= 0 && x < SCREEN_WIDTH);
        assert(y >= 0 && y < SCREEN_HEIGHT);
        assert(this->fb[y] != NULL);
        return this->fb[y][x];
    }

    size_t fb_length = 0;
    bool **fb = nullptr;
    std::atomic_uint8_t delay = 0;
    std::atomic_uint8_t sound_timer = 0;
    std::mutex key_mutex = {};
    std::condition_variable key_cv = {};
    bool key_pressed_map[0x10] = {};
    std::atomic<uint8_t> last_key = 0xFF;

  private:
    uint8_t *ram = nullptr;
    uint16_t pc = 0x200;
    uint16_t *stack = nullptr;
    uint8_t sp = 0;
    uint8_t v[16] = {0};
    uint16_t i = 0;
    // bool compat;
};

size_t pixels_length = 0;
uint32_t *pixels_array = nullptr;

void draw(SDL_Renderer *renderer, SDL_Texture *texture, Chip8 *chip8) {
    if (pixels_array == NULL ||
        pixels_length != (size_t)(SCREEN_HEIGHT * SCREEN_WIDTH)) {
        if (pixels_array != NULL) {
            free(pixels_array);
        }

        pixels_array = (uint32_t *)malloc(sizeof(uint32_t) *
                                          (SCREEN_HEIGHT * SCREEN_WIDTH));
        if (pixels_array == NULL) {
            fprintf(stderr, "Failed to allocated pixels buffer!");
            exit(1);
        }
        pixels_length = (size_t)(SCREEN_HEIGHT * SCREEN_WIDTH);
    }

    for (int i = 0; i < SCREEN_HEIGHT; i++) {
        for (int j = 0; j < SCREEN_WIDTH; j++) {
            pixels_array[i * SCREEN_WIDTH + j] =
                chip8->fb[i][j] ? FG_COLOR : BG_COLOR;
        }
    }

    SDL_UpdateTexture(texture, nullptr, pixels_array,
                      SCREEN_WIDTH * sizeof(uint32_t));
    SDL_RenderClear(renderer);
    SDL_RenderCopy(renderer, texture, nullptr, nullptr);
    SDL_RenderPresent(renderer);
}

void timer_thread(Chip8 *chip8) {
    using namespace std::chrono;

    initAudio();

    while (true) {
        std::this_thread::sleep_for(milliseconds(TARGET_MS_PER_TICK)); // ~60Hz
        if (chip8->delay > 0) {
            chip8->delay--;
        }
        if (chip8->sound_timer > 0) {
            SDL_PauseAudio(0);
            chip8->sound_timer--;
        }

        if (chip8->sound_timer == 0) {
            SDL_PauseAudio(1);
        }
    }
}

void render_thread(Chip8 *chip8) {
    SDL_Window *sdl_window = SDL_CreateWindow(
        "CHIP-8 Emulator", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
        SCREEN_WIDTH * SCALE, SCREEN_HEIGHT * SCALE, SDL_WINDOW_SHOWN);
    SDL_Renderer *sdl_renderer =
        SDL_CreateRenderer(sdl_window, -1, SDL_RENDERER_ACCELERATED);
    SDL_Texture *render_texture = SDL_CreateTexture(
        sdl_renderer, SDL_PIXELFORMAT_RGBA8888, SDL_TEXTUREACCESS_STREAMING,
        SCREEN_WIDTH, SCREEN_HEIGHT);

    int width, height;
    SDL_GetWindowSize(sdl_window, &width, &height);

    width /= SCALE;
    height /= SCALE;

    while (true) {
        // check if we need to resize the window
        if (width != SCREEN_WIDTH || height != SCREEN_HEIGHT) {
            // resize the framebuffer
            bool **new_fb = allocate_framebuffer(SCREEN_WIDTH, SCREEN_HEIGHT);

            if (new_fb == NULL) {
                fprintf(stderr, "Failed to allocate framebuffer!\n");
                exit(1);
            }

            // we need to copy the old framebuffer to the new one, but, it
            // is not guranteed that the old framebuffer is smaller than the
            // new framebuffer, so we need to make sure we only read at most
            // the smaller framebuffer.
            size_t smaller_fb_size = width * height;
            if (width > SCREEN_WIDTH || height > SCREEN_HEIGHT) {
                smaller_fb_size = SCREEN_WIDTH * SCREEN_HEIGHT;
            }
            memcpy(new_fb[0], chip8->fb[0], smaller_fb_size);

            free_framebuffer(chip8->fb);
            chip8->fb_length = SCREEN_HEIGHT * SCREEN_WIDTH;
            chip8->fb = new_fb;

            // resize the SDL window
            SDL_SetWindowSize(sdl_window, SCREEN_WIDTH * SCALE,
                              SCREEN_HEIGHT * SCALE);
            SDL_DestroyTexture(render_texture);
            render_texture = SDL_CreateTexture(
                sdl_renderer, SDL_PIXELFORMAT_RGBA8888,
                SDL_TEXTUREACCESS_STREAMING, SCREEN_WIDTH, SCREEN_HEIGHT);
            width = SCREEN_WIDTH;
            height = SCREEN_HEIGHT;
        }

        draw(sdl_renderer, render_texture, chip8);
        std::this_thread::sleep_for(
            std::chrono::milliseconds(TARGET_MS_PER_FRAME)); // 60Hz
    }
}

void input_thread(Chip8 *chip8, std::atomic_bool &running) {
    SDL_Event event;
    while (true) {
        while (SDL_PollEvent(&event)) {
            if (event.type == SDL_QUIT)
                running = false;
            if (event.type == SDL_KEYDOWN || event.type == SDL_KEYUP) {
                std::unique_lock<std::mutex> lock(chip8->key_mutex);

                // map the key to a CHIP-8 key in a numpad way, but not on
                // the numpad since I dont have a numpad
                uint8_t key;
                switch (event.key.keysym.sym) {
                case SDLK_1:
                    key = 0x01;
                    break;
                case SDLK_2:
                    key = 0x02;
                    break;
                case SDLK_3:
                    key = 0x03;
                    break;
                case SDLK_q:
                    key = 0x04;
                    break;
                case SDLK_w:
                    key = 0x05;
                    break;
                case SDLK_e:
                    key = 0x06;
                    break;
                case SDLK_a:
                    key = 0x07;
                    break;
                case SDLK_s:
                    key = 0x08;
                    break;
                case SDLK_d:
                    key = 0x09;
                    break;
                case SDLK_x:
                    key = 0x00;
                    break;
                case SDLK_z:
                    key = 0x0A;
                    break;
                case SDLK_c:
                    key = 0x0B;
                    break;
                case SDLK_4:
                    key = 0x0C;
                    break;
                case SDLK_r:
                    key = 0x0D;
                    break;
                case SDLK_f:
                    key = 0x0E;
                    break;
                case SDLK_v:
                    key = 0x0F;
                    break;
                default:
                    continue;
                }
                chip8->key_pressed_map[key] = event.type == SDL_KEYDOWN;
                chip8->last_key = key;
                chip8->key_cv.notify_one();
                lock.unlock();
            }
        }
        std::this_thread::sleep_for(std::chrono::milliseconds(1));
    }
}

int Chip8::run() {
    using namespace std::chrono;

    constexpr auto cycle_time = milliseconds(TARGET_MS_PER_CYCLE);

    if (SDL_Init(SDL_INIT_VIDEO) < 0) {
        fprintf(stderr, "Failed to initialize SDL: %s\n", SDL_GetError());
        return 1;
    }

    std::atomic_bool running = true;

    std::thread render(render_thread, this);
    std::this_thread::sleep_for(milliseconds(TARGET_MS_PER_FRAME));
    std::thread input(input_thread, this, std::ref(running));
    std::thread timer(timer_thread, this);

    int ret = 0;

    while (running) {
        auto start_time = high_resolution_clock::now();

        uint16_t op = (read_mem(pc) << 8) | read_mem(pc + 1);
        if (!is_executable(pc)) {
            fprintf(stderr, "Attempted to execute protected memory at 0x%04x\n",
                    pc);
            view_ram();
            return 1;
        }
        printf("PC: 0x%04x OP: 0x%04x\n", pc, op);
        pc += 2;
        Bytecode bytecode = parse(op);

        printf("OPCODE: 0x%04x INSTRUCTION_TYPE: %d\n", op,
               bytecode.instruction_type);

        switch (bytecode.instruction_type) {
        case EXIT: {
            // From Peter Miller's chip8run. Exit emulator with a return
            // value of N.
            ret = bytecode.operand.byte;
            running = false;
            break;
        }
        case SYS: {
//             Jump to a machine code routine at nnn.
// This instruction is only used on the old computers on which
// Chip-8 was originally implemented. It is ignored by modern
// interpreters.
#if SYS_INSTRUCTION
            uint16_t addr = bytecode.operand.word & 0x0FFF;
            assert(addr < RAM_SIZE);
#if ALIGN_PC
            assert(addr % 0x2 == 0);
#endif // ALIGN_PC

            pc = bytecode.operand.word & 0x0FFF;
#else
            (void)bytecode;
            fprintf(stderr, "SYS instruction not enabled\n");
            exit(1);
#endif // SYS_INSTRUCTION
            break;
        }
        case CLS: {
            //             Clear the screen.
            clear_framebuffer(this->fb, SCREEN_WIDTH, SCREEN_HEIGHT);
            break;
        }
        case RET: {
            //             Return from a subroutine.
            // The interpreter sets the program counter to the address at
            // the top of the stack, then subtracts 1 from the stack
            // pointer.

            // --sp subtracts 1 from the stack pointer and returns the value
            // after the subraction
            pc = stack[--sp];
            break;
        }
        case JP: {
            //             Jump to location nnn.
            // The interpreter sets the program counter to nnn.

            if ((pc - 2) == 0x200 && bytecode.operand.word == 0x260) {
                printf("Entering Hi-Res mode\n");
                // This is the Hi-Res enable opcode (0x1260), we need to
                // change the resolution to 64x64 and jump to 0x2C0 instead
                // of 0x260.
                bytecode.operand.word = 0x2C0;
                // resize_framebuffer(this, 64, 64);
                SCREEN_HEIGHT = 64;
            }

            uint16_t addr = bytecode.operand.word & 0x0FFF;
            assert(addr < RAM_SIZE);
#if ALIGN_PC
            assert(addr % 0x2 == 0);
#endif

            pc = bytecode.operand.word & 0x0FFF;
            break;
        }
        case CALL: {
            //             Call subroutine at nnn.
            // The interpreter increments the stack pointer, then puts the
            // current PC on the top of the stack. The PC is then set to
            // nnn.

            // sp++ increments the stack pointer and returns the value
            // before the addition
            stack[sp++] = pc;
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
            // The interpreter compares register Vx to kk, and if they are
            // not equal, increments the program counter by 2.
            if (this->v[bytecode.operand.byte_reg.reg] !=
                bytecode.operand.byte_reg.byte) {
                pc += 2;
            }

            break;
        }
        case SKIP_INSTRUCTION_REG: {
            //             Skip next instruction if Vx = Vy.
            // The interpreter compares register Vx to register Vy, and if
            // they are equal, increments the program counter by 2.
            if (this->v[bytecode.operand.reg_reg.x] ==
                this->v[bytecode.operand.reg_reg.y]) {
                pc += 2;
            }
            break;
        }
        case SKIP_INSTRUCTION_NE_REG: {
            //             Skip next instruction if Vx != Vy.
            // The values of Vx and Vy are compared, and if they are not
            // equal, the program counter is increased by 2.
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
            // Adds the value kk to the value of register Vx, then stores
            // the result in Vx.
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
            // greater than 8 bits (i.e., > 255,) VF is set to 1, otherwise
            // 0. Only the lowest 8 bits of the result are kept, and stored
            // in Vx.
            int result = this->v[bytecode.operand.reg_reg.x] +
                         this->v[bytecode.operand.reg_reg.y];
            this->v[bytecode.operand.reg_reg.x] = result & 0xFF;

            if (result > 0xFF) {
                printf("Overflowed!\n");
                this->v[0xF] = 1;
            } else {
                this->v[0xF] = 0;
            }

            break;
        }
        case SUB_REG: {
            //             Set Vx = Vx - Vy, set VF = NOT borrow.
            // If Vx > Vy, then VF is set to 1, otherwise 0. Then Vy is
            // subtracted from Vx, and the results stored in Vx.
            bool borrow = this->v[bytecode.operand.reg_reg.x] >=
                          this->v[bytecode.operand.reg_reg.y];

            this->v[bytecode.operand.reg_reg.x] =
                (this->v[bytecode.operand.reg_reg.x] -
                 this->v[bytecode.operand.reg_reg.y]) &
                0xFF;
            if (borrow) {
                this->v[0xF] = 1;
            } else {
                this->v[0xF] = 0;
            }

            if (borrow) {
                this->v[0xF] = 1;
            } else {
                this->v[0xF] = 0;
            }

            break;
        }
        case OR_REG: {
            //             Set Vx = Vx OR Vy.
            // Performs a bitwise OR on the values of Vx and Vy, then stores
            // the result in Vx. A bitwise OR compares the corrseponding
            // bits from two values, and if either bit is 1, then the same
            // bit in the result is also 1. Otherwise, it is 0.
            this->v[bytecode.operand.reg_reg.x] =
                this->v[bytecode.operand.reg_reg.x] |
                this->v[bytecode.operand.reg_reg.y];
            break;
        }
        case AND_REG: {
            //             Set Vx = Vx AND Vy.
            // Performs a bitwise AND on the values of Vx and Vy, then
            // stores the result in Vx. A bitwise AND compares the
            // corrseponding bits from two values, and if both bits are 1,
            // then the same bit in the result is also 1. Otherwise, it is
            // 0.
            this->v[bytecode.operand.reg_reg.x] =
                this->v[bytecode.operand.reg_reg.x] &
                this->v[bytecode.operand.reg_reg.y];
            break;
        }
        case XOR_REG: {
            //             Set Vx = Vx XOR Vy.
            // Performs a bitwise exclusive OR on the values of Vx and Vy,
            // then stores the result in Vx. An exclusive OR compares the
            // corrseponding bits from two values, and if the bits are not
            // both the same, then the corresponding bit in the result is
            // set to 1. Otherwise, it is 0.
            this->v[bytecode.operand.reg_reg.x] =
                this->v[bytecode.operand.reg_reg.x] ^
                this->v[bytecode.operand.reg_reg.y];
            break;
        }
        case SHR_REG: {
            //             Set Vx = Vx SHR 1.
            // If the least-significant bit of Vx is 1, then VF is set to 1,
            // otherwise 0. Then Vx is divided by 2.
            bool carry = this->v[bytecode.operand.reg_reg.x] & 0x01;
            this->v[bytecode.operand.reg_reg.x] >>= 1;
            if (carry) {
                this->v[0xF] = 1;
            } else {
                this->v[0xF] = 0;
            }
            break;
        }
        case SUBN_REG: {
            //             Set Vx = Vy - Vx, set VF = NOT borrow.
            // If Vy > Vx, then VF is set to 1, otherwise 0. Then Vx is
            // subtracted from Vy, and the results stored in Vx.
            bool borrow = this->v[bytecode.operand.reg_reg.y] >=
                          this->v[bytecode.operand.reg_reg.x];

            this->v[bytecode.operand.reg_reg.x] =
                (this->v[bytecode.operand.reg_reg.y] -
                 this->v[bytecode.operand.reg_reg.x]) &
                0xFF;
            if (borrow) {
                this->v[0xF] = 1;
            } else {
                this->v[0xF] = 0;
            }

            if (borrow) {
                this->v[0xF] = 1;
            } else {
                this->v[0xF] = 0;
            }

            break;
        }
        case SHL_REG: {
            //             Set Vx = Vx SHL 1.
            // If the most-significant bit of Vx is 1, then VF is set to 1,
            // otherwise to 0. Then Vx is multiplied by 2.
            bool carry = (this->v[bytecode.operand.reg_reg.x] >> 7) & 0x01;
            this->v[bytecode.operand.reg_reg.x] <<= 1;
            if (carry) {
                this->v[0xF] = 1;
            } else {
                this->v[0xF] = 0;
            }
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
            // The interpreter generates a random number from 0 to 255,
            // which is then ANDed with the value kk. The results are stored
            // in Vx. See instruction 8xy2 for more information on AND.
            this->v[bytecode.operand.byte_reg.reg] =
                static_cast<uint8_t>(rand()) & bytecode.operand.byte_reg.byte;
            break;
        }
        case DRW: {
            //             Display n-byte sprite starting at memory location
            //             I at (Vx, Vy), set VF = collision.
            // The interpreter reads n bytes from memory, starting at the
            // address stored in I. These bytes are then displayed as
            // sprites on screen at coordinates (Vx, Vy). Sprites are XORed
            // onto the existing screen. If this causes any pixels to be
            // erased, VF is set to 1, otherwise it is set to 0. If the
            // sprite is positioned so part of it is outside the coordinates
            // of the display, it wraps around to the opposite side of the
            // screen. See instruction 8xy3 for more information on XOR, and
            // section 2.4, Display, for more information on the Chip-8
            // screen and sprites.
            this->v[0x0F] = 0;

            for (int i = 0; i < bytecode.operand.reg_reg_nibble.nibble; i++) {
                uint8_t sprite = read_mem(this->i + i);

                for (int j = 0; j < 8; j++) {
                    bool source = (sprite >> (7 - j)) & 0x1;
                    int x = (this->v[bytecode.operand.reg_reg_nibble.x] + j) %
                            SCREEN_WIDTH;
                    int y = (this->v[bytecode.operand.reg_reg_nibble.y] + i) %
                            SCREEN_HEIGHT;
                    if (!source) {
                        continue;
                    }

                    if (this->get_pixel(x, y)) {
                        this->set_pixel(x, y, 0);
                        this->v[0x0F] = 1;
                    } else {
                        this->set_pixel(x, y, 1);
                    }
                }
            }
            break;
        }
        case SKIP_PRESSED_REG: {
            //             Skip next instruction if key with the value of Vx
            //             is pressed.
            // Checks the keyboard, and if the key corresponding to the
            // value of Vx is currently in the down position, PC is
            // increased by 2.
            uint8_t key = this->v[bytecode.operand.byte];

            if (this->key_pressed_map[key]) {
                pc += 2;
            }
            break;
        }
        case SKIP_NOT_PRESSED_REG: {
            //             Skip next instruction if key with the value of Vx
            //             is not pressed.
            // Checks the keyboard, and if the key corresponding to the
            // value of Vx is currently in the up position, PC is increased
            // by 2. fprintf(stderr, "SKIP_NOT_PRESSED_REG not
            // implemented\n");
            uint8_t key = this->v[bytecode.operand.byte];

            if (!this->key_pressed_map[key]) {
                pc += 2;
            }
            break;
        }
        case LD_REG_DT: {
            //             Set Vx = delay timer value.
            // The value of DT is placed into Vx.
            this->v[bytecode.operand.byte] = this->delay;
            break;
        }
        case LD_REG_K: {
            //             Wait for a key press, store the value of the key
            //             in Vx.
            // All execution stops until a key is pressed, then the value of
            // that key is stored in Vx.
            std::unique_lock<std::mutex> lock(this->key_mutex);

            this->key_cv.wait(lock, [&]() {
                this->v[bytecode.operand.byte] = this->last_key.load();
                return this->key_pressed_map[this->last_key.load()];
            });
            lock.unlock();
            while (this->key_pressed_map[this->last_key.load()]) {
                std::this_thread::sleep_for(std::chrono::milliseconds(1));
            }
            break;
        }
        case LD_DT_REG: {
            //             Set delay timer = Vx.
            // DT is set equal to the value of Vx.
            this->delay = this->v[bytecode.operand.byte];
            break;
        }
        case LD_ST_REG: {
            //             Set sound timer = Vx.
            // ST is set equal to the value of Vx.
            set_sound_timer(this->v[bytecode.operand.byte]);
            break;
        }
        case ADD_I_REG: {
            //             Set I = I + Vx.
            // The values of I and Vx are added, and the results are stored
            // in I.
            this->i += this->v[bytecode.operand.byte];
            break;
        }
        case LD_F_REG: {
            //             Set I = location of sprite for digit Vx.
            // The value of I is set to the location for the hexadecimal
            // sprite corresponding to the value of Vx. See section 2.4,
            // Display, for more information on the Chip-8 hexadecimal font.

            //? This is the ONLY spot in 0x0000-0x01FF of the RAM where the
            //? emulator is allowed to access. Since that area of RAM is
            //? where the font is stored.
            this->i = (uint16_t)(this->v[bytecode.operand.byte] * 5);
            break;
        }
        case LD_B_REG: {
            //             Store BCD representation of Vx in memory
            //             locations I, I+1, and I+2.
            // The interpreter takes the decimal value of Vx, and places the
            // hundreds digit in memory at location in I, the tens digit at
            // location I+1, and the ones digit at location I+2.
            write_mem(this->i,
                      (uint8_t)((this->v[bytecode.operand.byte] / 100) & 0x0F));
            write_mem(this->i + 1,
                      (uint8_t)((this->v[bytecode.operand.byte] % 100) / 10) &
                          0x0F);
            write_mem(this->i + 2,
                      (uint8_t)(this->v[bytecode.operand.byte] % 10) & 0x0F);
            break;
        }
        case LD_PTR_I_REG: {
            //             Store registers V0 through Vx in memory starting
            //             at location I.
            // The interpreter copies the values of registers V0 through Vx
            // into memory, starting at the address in I.
            for (int i = 0; i <= bytecode.operand.byte; i++) {
                write_mem(this->i + i, this->v[i]);
            }
            break;
        }
        case LD_REG_PTR_I: {
            //             Read registers V0 through Vx from memory starting
            //             at location I.
            // The interpreter reads values from memory starting at location
            // I into registers V0 through Vx.
            for (int i = 0; i <= bytecode.operand.byte; i++) {
                this->v[i] = read_mem(this->i + i);
            }
            break;
        }
        case UNKNOWN_INSTRUCTION: {
            fprintf(stderr, "Unknown instruction: %04x\n", op);
            exit(1);
        }
        }

        auto elapsed_time = duration_cast<milliseconds>(
            high_resolution_clock::now() - start_time);
        if (elapsed_time < cycle_time) {
            std::this_thread::sleep_for(cycle_time - elapsed_time);
        }
    }

    timer.detach();
    render.detach();
    input.detach();

    return ret;
}

void Chip8::view_ram() {
    printf("Hex dump:\n");
    for (size_t i = 0; i < RAM_SIZE / 16; i++) {
        printf("%04x: ", (unsigned int)(i * 16));

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

void Chip8::view_stack() {
    printf("Stack:\n");
    for (int i = 0; i < 16; i++) {
        printf("%04x ", stack[i]);
    }
    printf("\n");
}

void signal_handler(int signum) {
    (void)signum;
    exit(1);
}

int main(int argc, char **argv) {
    signal(SIGINT, signal_handler);
    if (argc < 2) {
        printf("Usage: %s <file> [options]\n", argv[0]);
        return 1;
    }

    char *file_name = NULL;

    // is this memory safe?
    // start from 1 to skip the first argument (the executable)
    for (int i = 1; i < argc; i++) {
        if (argc < i) {
            printf("exceeded argc\n");
        }

        if (strcmp(argv[i], "--scale") == 0) {

            if (argc < i + 1) {
                printf("Error: Missing scale value\n");
                return 1;
            }

            long scale = strtol(argv[i + 1], NULL, 10);
            if (scale < 1 || scale > 50) {
                printf("Error: Invalid scale value\n");
                return 1;
            }

            SCALE = scale;

            i++;
            continue;
        }

        if (strcmp(argv[i], "--bg") == 0) {
            if (argc < i + 1) {
                printf("Error: Missing bg value\n");
                return 1;
            }

            long bg = strtol(argv[i + 1], NULL, 16);
            if (bg < 0 || bg > 0xFFFFFF) {
                printf("Error: Invalid bg value\n");
                return 1;
            }

            // RSH by 8 to correct for the alpha channel
            BG_COLOR = (int)(bg << 8);
            i++;
            continue;
        }

        if (strcmp(argv[i], "--fg") == 0) {
            if (argc < i + 1) {
                printf("Error: Missing fg value\n");
                return 1;
            }

            long fg = strtol(argv[i + 1], NULL, 16);
            if (fg < 0 || fg > 0xFFFFFF) {
                printf("Error: Invalid fg value\n");
                return 1;
            }

            // RSH by 8 to correct for the alpha channel
            FG_COLOR = (int)(fg << 8);
            i++;
            continue;
        }

        // if the argument does not start with a dash, assume it is a file
        if (argv[i][0] != '-') {
            printf("Filename: %s\n", argv[i]);
            file_name = argv[i];
            continue;
        }
    }

    srand(time(0));

    Chip8 chip8 = Chip8(file_name);
    chip8.dump_ram();
    int ret = chip8.run();

    return ret;
}
