#include <bits/stdc++.h>
#include <cstddef>
#include <cstdint>
#include <fcntl.h>
#include <format>
#include <queue>
#include <string>
#include <sys/types.h>
#include <unistd.h>
#include <unordered_map>
#include <unordered_set>
#include <vector>
#define BYTECODE_READER_IMPLEMENTATION
#include "reader.hpp"

#include <cstdio>

struct label {
    enum type {
        type_instruction,
        type_byte,
    } type;
    uint16_t length;
    std::string name;
};

uint16_t *find_closest_label(const std::unordered_map<uint16_t, label> &map,
                             uint16_t target) {

    uint16_t *closest_label = nullptr;

    for (const auto &pair : map) {
        if (pair.first <= target) {
            if (closest_label != nullptr && pair.first < *closest_label) {
                continue;
            }
            if (closest_label == nullptr) {
                closest_label = (uint16_t *)calloc(1, sizeof(uint16_t));
                if (closest_label == NULL) {
                    throw std::runtime_error("Failed to allocate memory");
                }
            }
            *closest_label = pair.first;
        } else {
            break;
        }
    }

    return closest_label;
}

std::string to_hex(size_t number) { return std::format("{:04x}", number); }

std::string
get_label_for_byte(uint16_t byte,
                   std::unordered_map<uint16_t, struct label> labels,
                   enum label::type target_type) {
    std::string operand_str;
    operand_str.append("0x");
    operand_str.append(to_hex(byte));
    if (labels.find(byte - 0x200) != labels.end()) {
        operand_str.clear();
        operand_str = labels.at(byte - 0x200).name;
    } else {
        // try to see if the operand is offset from a label
        if (byte > 0x200) {
            uint16_t *closest_label = find_closest_label(labels, byte - 0x200);
            if (closest_label != nullptr &&
                labels.at(*closest_label).type == target_type) {
                uint16_t offset = (byte - 0x200) - (*closest_label);

                // discard our result if the offset is greater than the
                // length of the label
                if (offset > labels.at(*closest_label).length) {
                    return operand_str;
                }
                operand_str.clear();
                operand_str = labels.at(*closest_label).name;
                operand_str.append(" + 0x");
                operand_str.append(to_hex(offset));
                operand_str.append("");
            }
        }
    }

    return operand_str;
}

// I could emit something like omni assembly, nut that is significantly more
// complex than just emitting the assembly like this, so I am just going to
// emit the assembly like this for now
void print_instruction(Bytecode bytecode, uint16_t pc,
                       std::unordered_map<uint16_t, struct label> labels) {
    switch (bytecode.instruction_type) {
    case HLT:
        printf("halt\n");
        break;
    case EXIT:
        printf("exit 0x%02x\n", bytecode.operand.byte);
        break;
    case SYS:
        if (labels.find(bytecode.operand.word - 0x200) == labels.end()) {
            fprintf(stderr, "No label found for %04x\n",
                    bytecode.operand.word - 0x200);
            exit(1);
        }

        printf("sys %s\n",
               labels.find(bytecode.operand.word - 0x200)->second.name.c_str());
        break;
    case CLS:
        printf("cls\n");
        break;
    case RET:
        printf("ret\n");
        break;
    case JP:
        if (pc == 0 && bytecode.operand.word == 0x260) {
            printf("hires\n");
            break;
        }
        if (labels.find(bytecode.operand.word - 0x200) == labels.end()) {
            fprintf(stderr, "No label found for %04x\n",
                    bytecode.operand.word - 0x200);
            exit(1);
        }

        printf("jp %s\n",
               labels.find(bytecode.operand.word - 0x200)->second.name.c_str());
        break;
    case CALL:
        if (labels.find(bytecode.operand.word - 0x200) == labels.end()) {
            fprintf(stderr, "No label found for %04x\n",
                    bytecode.operand.word - 0x200);
            exit(1);
        }

        printf("call %s\n",
               labels.find(bytecode.operand.word - 0x200)->second.name.c_str());
        break;
    case SKIP_INSTRUCTION_BYTE:
        printf("se v%x, 0x%02x\n", bytecode.operand.byte_reg.reg,
               bytecode.operand.byte_reg.byte);
        break;
    case SKIP_INSTRUCTION_NE_BYTE:
        printf("sne v%x, 0x%02x\n", bytecode.operand.byte_reg.reg,
               bytecode.operand.byte_reg.byte);
        break;
    case SKIP_INSTRUCTION_REG:
        printf("se v%x, v%x\n", bytecode.operand.reg_reg.x,
               bytecode.operand.reg_reg.y);
        break;
    case SKIP_INSTRUCTION_NE_REG:
        printf("sne v%x, v%x\n", bytecode.operand.reg_reg.x,
               bytecode.operand.reg_reg.y);
        break;
    case LOAD_BYTE: {
        printf("ld v%x, 0x%02x\n", bytecode.operand.byte_reg.reg,
               bytecode.operand.byte_reg.byte);
        break;
    }
    case ADD_BYTE:
        printf("add v%x, 0x%02x\n", bytecode.operand.byte_reg.reg,
               bytecode.operand.byte_reg.byte);
        break;
    case LOAD_REG:
        printf("ld v%x, v%x\n", bytecode.operand.reg_reg.x,
               bytecode.operand.reg_reg.y);
        break;
    case ADD_REG:
        printf("add v%x, v%x\n", bytecode.operand.reg_reg.x,
               bytecode.operand.reg_reg.y);
        break;
    case OR_REG:
        printf("or v%x, v%x\n", bytecode.operand.reg_reg.x,
               bytecode.operand.reg_reg.y);
        break;
    case AND_REG:
        printf("and v%x, v%x\n", bytecode.operand.reg_reg.x,
               bytecode.operand.reg_reg.y);
        break;
    case XOR_REG:
        printf("xor v%x, v%x\n", bytecode.operand.reg_reg.x,
               bytecode.operand.reg_reg.y);
        break;
    case SHR_REG:
        printf("shr v%x\n", bytecode.operand.reg_reg.x);
        break;
    case SUB_REG:
        printf("sub v%x, v%x\n", bytecode.operand.reg_reg.x,
               bytecode.operand.reg_reg.y);
        break;
    case SUBN_REG:
        printf("subn v%x, v%x\n", bytecode.operand.reg_reg.x,
               bytecode.operand.reg_reg.y);
        break;
    case SHL_REG:
        printf("shl v%x\n", bytecode.operand.reg_reg.x);
        break;
    case LOAD_I_BYTE: {
        printf("ld I, %s\n", get_label_for_byte(bytecode.operand.word, labels,
                                                label::type_byte)
                                 .c_str());
        break;
    }
    case JP_V0_BYTE: {
        printf("jp v0, %s\n", get_label_for_byte(bytecode.operand.word, labels,
                                                 label::type_instruction)
                                  .c_str());
        break;
    }
    case RND:
        printf("rnd v%x, 0x%02x\n", bytecode.operand.byte_reg.reg,
               bytecode.operand.byte_reg.byte);
        break;
    case DRW:
        printf("drw v%x, v%x, 0x%x\n", bytecode.operand.reg_reg_nibble.x,
               bytecode.operand.reg_reg_nibble.y,
               bytecode.operand.reg_reg_nibble.nibble);
        break;
    case SKIP_PRESSED_REG:
        printf("skp v%x\n", bytecode.operand.byte);
        break;
    case SKIP_NOT_PRESSED_REG:
        printf("sknp v%x\n", bytecode.operand.byte);
        break;
    case LD_REG_DT:
        printf("ld v%x, dt\n", bytecode.operand.byte);
        break;
    case LD_REG_K:
        printf("ld v%x, k\n", bytecode.operand.byte);
        break;
    case LD_DT_REG:
        printf("ld dt, v%x\n", bytecode.operand.byte);
        break;
    case LD_ST_REG:
        printf("ld st, v%x\n", bytecode.operand.byte);
        break;
    case ADD_I_REG:
        printf("add I, v%x\n", bytecode.operand.byte);
        break;
    case LD_F_REG:
        printf("ld f, v%x\n", bytecode.operand.byte);
        break;
    case LD_B_REG:
        printf("ld b, v%x\n", bytecode.operand.byte);
        break;
    case LD_PTR_I_REG:
        printf("ld [I], v%x\n", bytecode.operand.byte);
        break;
    case LD_REG_PTR_I:
        printf("ld v%x, [I]\n", bytecode.operand.byte);
        break;
    case UNKNOWN_INSTRUCTION:
        printf("?\n");
    }
}

struct hole {
    uint16_t start;
    uint16_t end;
};

struct assembly_node {
    uint16_t address;
    enum type {
        type_byte,
        type_instruction,
    } type;
    union {
        uint8_t byte;
        Bytecode instruction;
    };
};

void write_assembly(std::vector<struct assembly_node> assembly,
                    std::unordered_map<uint16_t, struct label> labels) {
    std::sort(assembly.begin(), assembly.end(),
              [](const struct assembly_node &a, const struct assembly_node &b) {
                  return a.address < b.address;
              });
    uint16_t last_byte_address = 0x0000;

    for (auto &node : assembly) {
        if (node.type != assembly_node::type_byte && node.address != 0 &&
            node.address - 1 == last_byte_address) {
            printf("\n");
        }

        if (labels.find(node.address) != labels.end()) {
            if (node.address != 0x0000) {
                // add whitespacing between labels, but not for the _start label
                printf("\n");
            }

            printf("%s:\n", labels.at(node.address).name.c_str());
        }

        switch (node.type) {
        case assembly_node::type_byte:
            if (node.address != last_byte_address + 1) {
                printf("db ");
            } else {
                printf(",\n   ");
            }

            printf("0x%02x", node.byte);
            last_byte_address = node.address;
            break;
        case assembly_node::type_instruction:
            // if previous assembly was a byte, then we need to emit a new line
            print_instruction(node.instruction, node.address, labels);
            break;
        }
    }
}

void disassemble(uint8_t *rom, int rom_size) {
    // evaluate the bytecode, but dont actually execute it, just print it out,
    // and when we reach a branching instruction, follow it. Make sure that if
    // we enter an inifinite loop we dont just loop forever, so make sure we
    // keep track of what we have already visited
    std::unordered_set<uint16_t> addresses_visited;
    std::queue<uint16_t> work_queue;
    std::vector<struct hole> holes;
    std::unordered_map<uint16_t, struct label> labels;
    size_t label_idx = 0;
    uint16_t *stack = (uint16_t *)calloc(16, sizeof(uint16_t));
    if (stack == NULL) {
        fprintf(stderr, "Failed to allocate stack!");
        exit(1);
    }
    // holds the start of a label
    size_t stack_idx = 0;

    std::vector<struct assembly_node> assembly;

    // start at the beginning of the rom
    work_queue.push(0x0000);
    labels.emplace(
        0x0000,
        label{.type = label::type_instruction, .length = 0, .name = "_start"});

    while (!work_queue.empty()) {
        uint16_t pc = work_queue.front();
        work_queue.pop();

        if (pc >= (uint16_t)rom_size) {
            // if we are reading past the end of the rom, we are done
            break;
        }

        if (addresses_visited.find(pc) != addresses_visited.end())
            continue;
        addresses_visited.insert(pc);

        uint16_t opcode = (rom[pc] << 8) | rom[pc + 1];
        Bytecode bytecode = parse(opcode);

        assembly.push_back({.address = pc,
                            .type = assembly_node::type_instruction,
                            .instruction = bytecode});

        switch (bytecode.instruction_type) {
        case JP: {
            if (pc == 0 && bytecode.operand.word == 0x260) {
                work_queue.push(0x2C0);
                break;
            }

            if (!labels.contains(bytecode.operand.word - 0x200)) {
                labels.emplace(
                    bytecode.operand.word - 0x200,
                    label{.type = label::type_instruction,
                          .length = 0,
                          .name = "_" + std::to_string(label_idx++)});
            }
            work_queue.push(bytecode.operand.word - 0x200);
            break;
        }
        case CALL: {
            if (stack_idx == 16) {
                fprintf(stderr, "Stack overflow!\n");
                exit(1);
            }

            if (!labels.contains(bytecode.operand.word - 0x200)) {
                labels.emplace(
                    bytecode.operand.word - 0x200,
                    label{.type = label::type_instruction,
                          .length = 0,
                          .name = "_" + std::to_string(label_idx++)});
            }
            stack[stack_idx++] = pc + 2;

            work_queue.push(bytecode.operand.word - 0x200);
            break;
        }
        case SKIP_INSTRUCTION_BYTE:
        case SKIP_INSTRUCTION_NE_BYTE:
        case SKIP_INSTRUCTION_REG:
        case SKIP_INSTRUCTION_NE_REG:
        case SKIP_PRESSED_REG:
        case SKIP_NOT_PRESSED_REG: {
            work_queue.push(pc + 2);
            work_queue.push(pc + 4);
            break;
        }
        case RET: {
            if (stack_idx == 0) {
                fprintf(stderr, "Stack underflow!\n");
                exit(1);
            }

            uint16_t ret_pc = stack[--stack_idx];
            work_queue.push(ret_pc);
            break;
        }
        case HLT: { // Stop following
            break;
        }
        case UNKNOWN_INSTRUCTION: {
            fprintf(stderr, "Unknown instruction: %04x\n", opcode);
            // we failed at disassembling smartly
            break;
        }
        default:
            work_queue.push(pc + 2);
        }
    }

    bool skip = false;
    uint16_t *last_seen_byte_array = nullptr;
    uint16_t start_of_last_contiguous_block = 0x0000;
    for (uint16_t pc = 0x00; pc < rom_size; pc++) {
        if (skip) {
            skip = false;
            continue;
        }

        if (addresses_visited.find(pc) != addresses_visited.end()) {
            // when there is an instruction that we have already visited, we
            // want to skip this byte and the next byte, but we cant rely of the
            // instructions being aligned to 0x02 bytes, so instead we tell the
            // next run of the loop to skip
            skip = true;
            continue;
        }

        // this seems scary, but it's fine because the if block will jump down
        // if the first condition is met, so it will never dereference a null
        // pointer
        if (last_seen_byte_array == nullptr ||
            *last_seen_byte_array != pc - 1) {
            if (last_seen_byte_array == nullptr) {
                last_seen_byte_array = new uint16_t;
            }

            start_of_last_contiguous_block = pc;

            // we are not in a contiguous block of bytes, so we need to add a
            // label
            if (!labels.contains(pc)) {
                labels.emplace(
                    pc, label{.type = label::type_byte,
                              .length = 1,
                              .name = "_" + std::to_string(label_idx++)});
            }
        } else {
            // we are in a contiguous block of bytes, so we need to update the
            // label's length by one
            labels[start_of_last_contiguous_block].length++;
        }

        *last_seen_byte_array = pc;

        assembly.push_back(
            {.address = pc, .type = assembly_node::type_byte, .byte = rom[pc]});
    }

    for (auto &pair : labels) {
        uint16_t pc = pair.first;
        uint16_t label_length = 0;
        // while we havent reached the end of the rom, and we havent crossed
        // into a new label
        while (pc < rom_size) {
            label_length++;
            switch (pair.second.type) {
            case label::type_byte:
                pc++;
                break;
            case label::type_instruction:
                pc += 2;
                break;
            }

            if (labels.find(pc) != labels.end())
                break;
        }

        pair.second.length = label_length;
    }

    write_assembly(assembly, labels);
}

int main(int argc, char **argv) {
    if (argc < 2) {
        printf("Usage: %s <file>\n", argv[0]);
        return 1;
    }

    int rom_fd = open(argv[1], O_RDONLY);
    if (rom_fd < 0) {
        fprintf(stderr, "Failed to open file: %s\n", argv[1]);
        return 1;
    }

    int rom_size = lseek(rom_fd, 0, SEEK_END);
    (void)lseek(rom_fd, 0, SEEK_SET);

    uint8_t *rom = (uint8_t *)calloc(rom_size, sizeof(uint8_t));
    if (rom == NULL) {
        fprintf(stderr, "Failed to allocate memory!\n");
        return 1;
    }

    read(rom_fd, rom, rom_size);

    disassemble(rom, rom_size);

    return 0;
}