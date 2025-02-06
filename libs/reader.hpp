#include <cassert>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <sys/types.h>

struct byte_reg {
    uint8_t reg;
    uint16_t byte;
};

struct reg_reg {
    uint8_t x;
    uint8_t y;
};

struct reg_reg_nibble {
    uint8_t x;
    uint8_t y;
    uint8_t nibble;
};

// this definitely wont be confusing or difficult to implement at all, I am
// perfect at writing good code and documentation and I would never write
// something that is stupidly thought out or overly complicated
enum instruction {
    // operand is a pointer to a uint8_t
    EXIT = 0,
    // operand is a pointer to a uint16_t
    SYS,
    // no operand
    CLS,
    // no operand
    RET,
    // operand is a pointer to a uint16_t
    JP,
    // operand is a pointer to a uint16_t
    CALL,
    // operand is a pointer to byte_reg
    SKIP_INSTRUCTION_BYTE,
    // operand is a pointer to byte_reg
    SKIP_INSTRUCTION_NE_BYTE,
    // operand is a pointer to reg_reg
    SKIP_INSTRUCTION_REG,
    // operand is a pointer to reg_byte
    LOAD_BYTE,
    // operand is a pointer to reg_byte
    ADD_BYTE,
    // operand is a pointer to reg_reg
    LOAD_REG,
    // operand is a pointer to reg_reg
    OR_REG,
    // operand is a pointer to reg_reg
    AND_REG,
    // operand is a pointer to reg_reg
    XOR_REG,
    // operand is a pointer to reg_reg
    ADD_REG,
    // operand is a pointer to reg_reg
    SUB_REG,
    // operand is a pointer to reg_reg
    SHR_REG,
    // operand is a pointer to reg_reg
    SUBN_REG,
    // operand is a pointer to reg_reg
    SHL_REG,
    // operand is a pointer to reg_reg
    SKIP_INSTRUCTION_NE_REG,
    // operand is a pointer to a uint16_t
    LOAD_I_BYTE,
    // operand is a pointer to a uint16_t
    JP_V0_BYTE,
    // operand is a pointer to a reg_byte
    RND,
    // operand is a pointer to a reg_reg_nibble
    DRW,
    // operand is a pointer to a uint8_t
    SKIP_PRESSED_REG,
    // operand is a pointer to a uint8_t
    SKIP_NOT_PRESSED_REG,
    // operand is a pointer to a uint8_t
    LD_REG_DT,
    // operand is a pointer to a uint8_t
    LD_REG_K,
    // operand is a pointer to a uint8_t
    LD_DT_REG,
    // operand is a pointer to a uint8_t
    LD_ST_REG,
    // operand is a pointer to a uint8_t
    ADD_I_REG,
    // operand is a pointer to a uint8_t
    LD_F_REG,
    // operand is a pointer to a uint8_t
    LD_B_REG,
    // operand is a pointer to a uint8_t
    LD_PTR_I_REG,
    // operand is a pointer to a uint8_t
    LD_REG_PTR_I,
    UNKNOWN_INSTRUCTION,
};

class Bytecode {
  public:
    enum instruction instruction_type;
    // should be interpreted by a reader depending on the instruction type
    union {
        uint8_t byte;
        uint16_t word;
        struct byte_reg byte_reg;
        struct reg_reg reg_reg;
        struct reg_reg_nibble reg_reg_nibble;
    } operand;
};

inline Bytecode parse(uint16_t opcode) {
    struct Bytecode bytecode;

    switch (opcode & 0xF000) {
    case 0x0000: {
        if ((opcode & 0x00F0) == 0x0010) {
            // EXIT N 0x001N
            // Specific to emulators, not part of the original chip-8
            bytecode.instruction_type = EXIT;
            break;
        }

        switch (opcode & 0x00FF) {
        case 0x00E0: {
            // CLS 0x00E0
            // clears the screen
            bytecode.instruction_type = CLS;
            break;
        }
        case 0x00EE: {
            // RET 0x00EE
            bytecode.instruction_type = RET;
            break;
        }
        default:
            // SYS NNN
            //? NOTE: This is an outdated opcode, but it's still
            //? important for completeness. It's not clear what the
            //? difference is between it and the JMP NNN 0x1NNN opcode.
            bytecode.instruction_type = SYS;
            break;
        }

        break;
    }
    case 0x1000: {
        bytecode.instruction_type = JP;
        break;
    }
    case 0x2000: {
        bytecode.instruction_type = CALL;
        break;
    }
    case 0x3000: {
        bytecode.instruction_type = SKIP_INSTRUCTION_BYTE;
        break;
    }
    case 0x4000: {
        bytecode.instruction_type = SKIP_INSTRUCTION_NE_BYTE;
        break;
    }
    case 0x5000: {
        bytecode.instruction_type = SKIP_INSTRUCTION_REG;
        break;
    }
    case 0x6000: {
        bytecode.instruction_type = LOAD_BYTE;
        break;
    }
    case 0x700: {
        bytecode.instruction_type = ADD_BYTE;
        break;
    }
    case 0x8000: {
        switch (opcode & 0x000F) {
        case 0x0000: {
            bytecode.instruction_type = LOAD_REG;
            break;
        }
        case 0x0001: {
            bytecode.instruction_type = OR_REG;
            break;
        }
        case 0x0002: {
            bytecode.instruction_type = AND_REG;
            break;
        }
        case 0x0003: {
            bytecode.instruction_type = XOR_REG;
            break;
        }
        case 0x0004: {
            bytecode.instruction_type = ADD_REG;
            break;
        }
        case 0x0005: {
            bytecode.instruction_type = SUB_REG;
            break;
        }
        case 0x0006: {
            // Set VX equal to VX bitshifted right 1. VF is set to the least
            // significant bit of VX prior to the shift. Originally this opcode
            // meant set VX equal to VY bitshifted right 1 but emulators and
            // software seem to ignore VY now. Note: This instruction was
            // originally undocumented but functional due to how the 8XXX
            // instructions were implemented on teh COSMAC VIP.
            bytecode.instruction_type = SHR_REG;
            break;
        }
        case 0x0007: {
            bytecode.instruction_type = SUBN_REG;
            break;
        }
        case 0x000E: {
            bytecode.instruction_type = SHL_REG;
            break;
        }
        default: {
            bytecode.instruction_type = UNKNOWN_INSTRUCTION;
            break;
        }
        }
        break;
    }
    case 0x9000: {
        bytecode.instruction_type = SKIP_INSTRUCTION_NE_REG;
        break;
    }
    case 0xA000: {
        bytecode.instruction_type = LOAD_I_BYTE;
        break;
    }
    case 0xB000: {
        bytecode.instruction_type = JP_V0_BYTE;
        break;
    }
    case 0xC000: {
        bytecode.instruction_type = RND;
        break;
    }
    case 0xD000: {
        bytecode.instruction_type = DRW;
        break;
    }
    case 0xE000: {
        switch (opcode & 0x00FF) {
        case 0x009E: {
            bytecode.instruction_type = SKIP_PRESSED_REG;
            break;
        }
        case 0x00A1: {
            bytecode.instruction_type = SKIP_NOT_PRESSED_REG;
            break;
        }
        default: {
            bytecode.instruction_type = UNKNOWN_INSTRUCTION;
            break;
        }
        }
        break;
    }
    case 0xF000: {
        switch (opcode & 0x00FF) {
        case 0x0007: {
            bytecode.instruction_type = LD_REG_DT;
            break;
        }
        case 0x000A: {
            bytecode.instruction_type = LD_REG_K;
            break;
        }
        case 0x0015: {
            bytecode.instruction_type = LD_DT_REG;
            break;
        }
        case 0x0018: {
            bytecode.instruction_type = LD_ST_REG;
            break;
        }
        case 0x001E: {
            bytecode.instruction_type = ADD_I_REG;
            break;
        }
        case 0x0029: {
            bytecode.instruction_type = LD_F_REG;
            break;
        }
        case 0x0033: {
            bytecode.instruction_type = LD_B_REG;
            break;
        }
        case 0x0055: {
            bytecode.instruction_type = LD_PTR_I_REG;
            break;
        }
        case 0x0065: {
            bytecode.instruction_type = LD_REG_PTR_I;
            break;
        }
        default: {
            bytecode.instruction_type = UNKNOWN_INSTRUCTION;
            break;
        }
        }
        break;
    }
    default: {
        bytecode.instruction_type = UNKNOWN_INSTRUCTION;
        break;
    }
    }

    switch (bytecode.instruction_type) {
    case UNKNOWN_INSTRUCTION:
    case RET:
    case CLS: {
        // no operand
        break;
    }
    case EXIT: {
        bytecode.operand.byte = opcode & 0x000F;
        break;
    }
    case SKIP_PRESSED_REG:
    case SKIP_NOT_PRESSED_REG:
    case LD_REG_DT:
    case LD_REG_K:
    case LD_DT_REG:
    case LD_ST_REG:
    case ADD_I_REG:
    case LD_F_REG:
    case LD_B_REG:
    case LD_PTR_I_REG:
    case LD_REG_PTR_I: {
        bytecode.operand.byte = (uint8_t)((opcode & 0x0F00) >> 8);
        break;
    }
    case SYS:
    case JP:
    case CALL:
    case LOAD_I_BYTE:
    case JP_V0_BYTE: {
        bytecode.operand.word = (uint16_t)(opcode & 0x0FFF);
        break;
    }
    case SKIP_INSTRUCTION_BYTE:
    case SKIP_INSTRUCTION_NE_BYTE:
    case LOAD_BYTE:
    case ADD_BYTE:
    case RND: {
        bytecode.operand.byte_reg = (struct byte_reg){
            .reg = (uint8_t)((opcode & 0x0F00) >> 8),
            .byte = (uint16_t)(opcode & 0x00FF),
        };
        break;
    }
    case SKIP_INSTRUCTION_REG:
    case LOAD_REG:
    case OR_REG:
    case AND_REG:
    case XOR_REG:
    case ADD_REG:
    case SUB_REG:
    case SHR_REG:
    case SUBN_REG:
    case SHL_REG:
    case SKIP_INSTRUCTION_NE_REG: {
        bytecode.operand.reg_reg = (struct reg_reg){
            .x = (uint8_t)((opcode & 0x0F00) >> 8),
            .y = (uint8_t)((opcode & 0x00F0) >> 4),
        };
        break;
    }
    case DRW: {
        bytecode.operand.reg_reg_nibble = (struct reg_reg_nibble){
            .x = (uint8_t)((opcode & 0x0F00) >> 8),
            .y = (uint8_t)((opcode & 0x00F0) >> 4),
            .nibble = (uint8_t)(opcode & 0x000F),
        };
        break;
    }
    }

    return bytecode;
}
