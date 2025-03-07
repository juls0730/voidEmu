#include <algorithm>
#include <cctype>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fcntl.h>
#include <map>
#include <optional>
#include <set>
#include <sstream>
#include <string>
#include <unistd.h>
#include <unordered_map>
#include <unordered_set>
#include <variant>
#include <vector>

#define BYTECODE_READER_IMPLEMENTATION
#include "reader.hpp"

bool ishex(char c) {
    return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') ||
           (c >= 'A' && c <= 'F');
}

bool is_control(std::string &str) {
    // pointer I
    if (str.length() == 3 && str.starts_with("[") && str.ends_with("]")) {
        if (tolower(str[1]) == 'i') {
            return true;
        }
    }

    std::set<std::string> special_registers = {"i", "k", "b", "f", "dt", "st"};
    if ((special_registers.contains(str))) {
        return true;
    }

    return false;
}

bool is_register(std::string &str) {
    std::transform(str.begin(), str.end(), str.begin(), ::tolower);
    if ((str.starts_with("v") || str.starts_with("V")) && str.length() == 2 &&
        ishex(str[1])) {
        return true;
    }

    return is_control(str);
}

bool is_base16(std::string &str) {
    if (str.starts_with("0x")) {
        for (char c : str.substr(2)) {
            if (!ishex(c)) {
                return false;
            }
        }
        return true;
    }

    return false;
}

bool is_base2(std::string &str) {
    if (str.starts_with("0b")) {
        for (char c : str.substr(2)) {
            if (c != '0' && c != '1') {
                return false;
            }
        }
        return true;
    }

    return false;
}

bool is_base10(std::string &str) {
    for (char c : str) {
        if (!isdigit(c)) {
            return false;
        }
        return true;
    }

    return false;
}

bool is_numeric(std::string &str) {
    return is_base16(str) || is_base2(str) || is_base10(str);
}

bool is_arithmetic_operator(std::string &str) {
    return str == "+" || str == "-" || str == "*" || str == "/";
}

size_t get_numeric(std::string &str) {
    if (is_base16(str)) {
        return std::stoi(&str[2], nullptr, 16);
    }

    if (is_base2(str)) {
        return std::stoi(&str[2], nullptr, 2);
    }

    if (is_base10(str)) {
        return std::stoi(str, nullptr, 10);
    }

    fprintf(stderr, "Invalid numeric literal: %s\n", str.c_str());
    exit(1);
}

uint8_t get_reg(std::string &str) {
    if (str.length() != 2) {
        fprintf(stderr, "Invalid register: %s\n", str.c_str());
        exit(1);
    }

    if (tolower(str[0]) != 'v') {
        fprintf(stderr, "Invalid register: %s\n", str.c_str());
        exit(1);
    }

    return std::stoi(&str[1], nullptr, 16);
}

// as with all hashing functions, there is a chance of collision, but we can
// pretend it's not an issue by means of mathematical improbability
constexpr uint32_t hash(const std::string data) noexcept {
    uint32_t hash = 5381;

    for (const char &c : data) {
        hash = ((hash << 5) + hash) + (uint8_t)c;
    }

    return hash;
}

bool is_directive(std::string &str) {
    switch (hash(str)) {
    case hash("text"):
    case hash("offset"):
    case hash("include"):
    case hash("define"):
    case hash("db"):
    case hash("dw"):
        return true;
    default:
        return false;
    }
}

bool is_reserved(std::string &str) {
    return is_register(str) || is_directive(str) || is_numeric(str);
}

class OperandNode;
class InstructionNode;
class LabelNode;
class RootNode;

enum class ValueType { REGISTER, IMMEDIATE, STRING, ATIHMETIC, LABEL };

class OperandNode {
  public:
    ValueType type;
    std::string value;
    OperandNode(ValueType type, std::string value) : type(type), value(value) {}
};

enum class DirectiveType { DEFINE, TEXT, DB, DW, OFFSET, INCLUDE };
class DirectiveNode {
  public:
    DirectiveType directive;
    std::vector<OperandNode> operands;

    DirectiveNode(const DirectiveType directive,
                  const std::vector<OperandNode> &operands = {})
        : directive(directive), operands(operands) {}
};

class InstructionNode {
  public:
    std::string opcode;
    std::vector<OperandNode> operands;

    InstructionNode(const std::string &opcode,
                    const std::vector<OperandNode> &operands = {})
        : opcode(opcode), operands(operands) {}
};

struct SectionElement {
    enum class Type { Instruction, Directive };
    std::variant<InstructionNode, DirectiveNode> element;
    Type type;

    SectionElement(Type t, InstructionNode instr) : element(instr), type(t) {}

    SectionElement(Type t, DirectiveNode dir) : element(dir), type(t) {}
};

class SectionNode {
  public:
    std::optional<std::string> label_name;
    std::vector<SectionElement> elements;

    SectionNode(const std::string &name) : label_name(name) {}
    SectionNode(std::optional<std::string> name,
                const std::vector<SectionElement> &elements = {})
        : label_name(name), elements(elements) {}
};

class RootNode {
  public:
    std::vector<SectionNode> sections;
    // label name -> section index
    std::unordered_map<std::string, size_t> labels;

    RootNode(const std::vector<SectionNode> &sections = {})
        : sections(sections) {}
};

enum class TokenType {
    Label,
    LabelDecleration,
    Instruction,
    Directive,
    Register,
    Immediate,
    ArithmeticOperator,
    String,
    Comment,
    EOFToken
};

bool is_operand(TokenType type) {
    switch (type) {
    case TokenType::Label:
    case TokenType::Register:
    case TokenType::Immediate:
    case TokenType::String:
    case TokenType::ArithmeticOperator:
    case TokenType::Comment:
        return true;
    default:
        return false;
    }
}

// Token structure
struct Token {
    TokenType type;
    std::string value;
};

class Lexer {
  public:
    Lexer(const std::string &input) : input(input), pos(0) {}

    Token get_next_token() {
        std::string accum = "";
        bool is_operand = next_operand;
        next_operand = false;

        // strip whitespace from the beginning of the line, but dont use the
        // is_whitespace function, since that will strip away newlines,
        // which are very important in terms of context for us
        while (pos < input.size() && isspace(input[pos])) {
            pos++;
        }

        while (pos < input.size()) {
            char current_char = input[pos++];

            // if we find a comment, we make sure we havent reached the end of
            // our current token, and if we havent, collect the commend and
            // return the comment token
            if (current_char == ';') {
                if (!accum.empty()) {
                    // rewind so we dont consume the ; character
                    pos--;
                    break;
                }

                next_operand = false;

                while (current_char != '\n' && pos < input.size()) {
                    accum += current_char;
                    current_char = input[pos++];
                }

                return Token{TokenType::Comment, accum};
            }

            if (current_char == ':') {
                if (accum.empty()) {
                    fprintf(stderr,
                            "Expected label before ':' but got nothing\n");
                    exit(1);
                }

                if (is_reserved(accum)) {
                    fprintf(stderr, "Use of reserved token: %s\n",
                            accum.c_str());
                    exit(1);
                }

                next_operand = false;

                return Token{TokenType::LabelDecleration, accum};
            }

            if (current_char == '"') {
                if (!accum.empty()) {
                    fprintf(stderr, "Expected string before '\"'\n");
                    exit(1);
                }

                current_char = input[pos++];
                while (current_char != '"' && pos < input.size()) {
                    if (current_char == '\\') {
                        current_char = input[pos++];
                        switch (current_char) {
                        case 'a':
                            accum += '\a';
                            break;
                        case 'b':
                            accum += '\b';
                            break;
                        case 'e':
                            accum += 0x1b;
                            break;
                        case 'f':
                            accum += '\f';
                            break;
                        case 'n':
                            accum += '\n';
                            break;
                        case 'r':
                            accum += '\r';
                            break;
                        case 't':
                            accum += '\t';
                            break;
                        case 'v':
                            accum += '\v';
                            break;
                        // TODO: hex escapes
                        default:
                            accum += current_char;
                            break;
                        }

                        current_char = input[pos++];

                        continue;
                    }

                    accum += current_char;
                    current_char = input[pos++];
                }

                return Token{TokenType::String, accum};
            }

            if (current_char == '\n') {
                // we hit end of line, we are onto the next instruction
                next_operand = false;
                break;
            }

            // if we find a delimiting character, we have reached the end of
            // our current token
            if (isspace(current_char) || current_char == ',') {
                // next token is an operand
                next_operand = true;
                break;
            }

            accum += current_char;
        }

        if (pos >= input.size() && accum.empty()) {
            return Token{TokenType::EOFToken, ""};
        }

        if (accum.empty()) {
            return get_next_token();
        }

        if (is_arithmetic_operator(accum)) {
            return Token{TokenType::ArithmeticOperator, accum};
        }

        if (is_directive(accum)) {
            return Token{TokenType::Directive, accum};
        }

        if (is_numeric(accum)) {
            return Token{TokenType::Immediate, accum};
        }

        if (is_register(accum)) {
            return Token{TokenType::Register, accum};
        }

        if (is_operand) {
            return Token{TokenType::Label, accum};
        }

        return Token{TokenType::Instruction, accum};
    }

  private:
    std::string input;
    size_t pos;
    bool next_operand;
};

class Parser {
  public:
    Parser(Lexer &lexer)
        : lexer(lexer), current_token(lexer.get_next_token()) {}

    RootNode parse() {
        RootNode root;
        std::optional<SectionNode *> cur_section = std::nullopt;
        while (true) {
            if (current_token.type == TokenType::EOFToken) {
                break;
            }

            if (current_token.type == TokenType::LabelDecleration) {
                SectionNode new_section(current_token.value);
                if (root.labels.contains(current_token.value)) {
                    fprintf(stderr, "Label %s already exists\n",
                            current_token.value.c_str());
                    exit(1);
                }
                root.sections.push_back(new_section);
                root.sections.back().label_name = current_token.value;
                root.labels[current_token.value] = root.sections.size() - 1;
                cur_section =
                    std::optional<SectionNode *>(&root.sections.back());
                current_token = lexer.get_next_token();
                continue;
            }

            if (!cur_section.has_value()) {
                root.sections.push_back(SectionNode(std::nullopt));
                cur_section =
                    std::optional<SectionNode *>(&root.sections.back());
            }

            if (current_token.type == TokenType::Instruction) {
                // this will set the current token to the next token once it
                // finds a token that is not an operand
                cur_section.value()->elements.push_back(
                    {SectionElement::Type::Instruction, parse_instruction()});
            } else if (current_token.type == TokenType::Directive) {
                // this will set the current token to the next token once it
                // finds a token that is not an operand
                // cur_section->directives.push_back(parse_directive(&pc));
                cur_section.value()->elements.push_back(
                    {SectionElement::Type::Directive, parse_directive()});
            } else {
                current_token = lexer.get_next_token();
            }
        }
        return root;
    }

  private:
    Lexer &lexer;
    Token current_token;

    InstructionNode parse_instruction() {
        InstructionNode instruction = InstructionNode(current_token.value);
        instruction.opcode = current_token.value;
        current_token = lexer.get_next_token();

        while (is_operand(current_token.type)) {
            if (current_token.type == TokenType::Comment) {
                current_token = lexer.get_next_token();
                continue;
            }
            // the token is either a label, a number, or a register
            instruction.operands.push_back(parse_operand());
            current_token = lexer.get_next_token();
        }

        return instruction;
    }

    DirectiveNode parse_directive() {
        DirectiveType directive_type;
        switch (hash(current_token.value)) {
        case hash("define"): {
            directive_type = DirectiveType::DEFINE;
            break;
        }
        case hash("text"): {
            directive_type = DirectiveType::TEXT;
            break;
        }
        case hash("db"): {
            directive_type = DirectiveType::DB;
            break;
        }
        case hash("dw"): {
            directive_type = DirectiveType::DW;
            break;
        }
        case hash("offset"): {
            directive_type = DirectiveType::OFFSET;
            break;
        }
        case hash("include"): {
            directive_type = DirectiveType::INCLUDE;
            break;
        }
        }
        DirectiveNode directive = DirectiveNode(directive_type);
        current_token = lexer.get_next_token();

        // directives require an operand
        if (!is_operand(current_token.type)) {
            fprintf(stderr, "Expected operand, found %s",
                    current_token.value.c_str());
            exit(1);
        }

        while (is_operand(current_token.type)) {
            if (current_token.type == TokenType::Comment) {
                current_token = lexer.get_next_token();
                continue;
            }
            // the token is either a label, a number, or a register
            directive.operands.push_back(parse_operand());
            current_token = lexer.get_next_token();
        }

        return directive;
    }

    OperandNode parse_operand() {
        switch (current_token.type) {
        case TokenType::Label:
            return OperandNode(ValueType::LABEL, current_token.value);
        case TokenType::Register:
            return OperandNode(ValueType::REGISTER, current_token.value);
        case TokenType::Immediate:
            return OperandNode(ValueType::IMMEDIATE, current_token.value);
        case TokenType::String:
            return OperandNode(ValueType::STRING, current_token.value);
        case TokenType::ArithmeticOperator:
            return OperandNode(ValueType::ATIHMETIC, current_token.value);
        default:
            fprintf(stderr, "Unexpected operand type: %d\n",
                    static_cast<int>(current_token.type));
            exit(1);
        }
    }
};

struct Defines {
    enum ValueType type;
    std::string value;
};

class Assembler {
  public:
    uint8_t *assembler(RootNode root, int rom_fd) {
        printf("Assembling...\n");
        std::map<std::string, Defines> defines;

        // rom space labels
        std::unordered_map<std::string, size_t> labels;

        size_t pc = 0;
        for (auto &section : root.sections) {
            if (section.label_name.has_value()) {
                labels[section.label_name.value()] = pc;
            }

            for (auto &element : section.elements) {
                switch (element.type) {
                case SectionElement::Type::Instruction: {
                    pc += 2;
                    break;
                }

                case SectionElement::Type::Directive: {
                    DirectiveNode directive_node =
                        std::get<DirectiveNode>(element.element);
                    for (auto &operand : directive_node.operands) {
                        if (operand.type == ValueType::LABEL) {
                            // check if the label is really a reference
                            // to a constant (ie a value defined by a
                            // defines directive)
                            if (defines.contains(operand.value)) {
                                operand.type = defines[operand.value].type;
                                operand.value = defines[operand.value].value;
                            }
                        }
                    }
                    switch (directive_node.directive) {
                    case DirectiveType::DEFINE: {
                        if (directive_node.operands.size() != 2) {
                            fprintf(stderr, "Expected 2 operands for define\n");
                            exit(1);
                        }

                        if (root.labels.contains(
                                directive_node.operands[0].value)) {
                            fprintf(stderr,
                                    "Redecleration of label %s as "
                                    "constant\n",
                                    directive_node.operands[0].value.c_str());
                            exit(1);
                        }

                        if (defines.contains(
                                directive_node.operands[0].value)) {
                            // TODO: should we allow redeclaration of
                            // constants?
                            fprintf(stderr, "Redecleration of constant %s\n",
                                    directive_node.operands[0].value.c_str());
                            exit(1);
                        }

                        if (directive_node.operands[1].type ==
                            ValueType::LABEL) {
                            // "label" may be a constant
                            if (defines.contains(
                                    directive_node.operands[0].value)) {
                                defines[directive_node.operands[0].value] =
                                    defines.at(
                                        directive_node.operands[0].value);
                                break;
                            }
                        }

                        defines[directive_node.operands[0].value] = {
                            .type = directive_node.operands[1].type,
                            .value = directive_node.operands[1].value};
                        break;
                    }
                    case DirectiveType::TEXT: {
                        if (directive_node.operands.size() != 1) {
                            fprintf(stderr, "Expected 1 operand for text\n");
                            exit(1);
                        }

                        pc += directive_node.operands[0].value.size() + 1;
                        break;
                    }
                    case DirectiveType::DB: {
                        if (directive_node.operands.size() == 0) {
                            fprintf(stderr, "Expected operands for db\n");
                            exit(1);
                        }

                        pc += directive_node.operands.size();
                        break;
                    }
                    case DirectiveType::DW: {
                        if (directive_node.operands.size() == 0) {
                            fprintf(stderr, "Expected operands for dw\n");
                            exit(1);
                        }

                        pc += directive_node.operands.size() * 2;
                        break;
                    }
                    case DirectiveType::OFFSET: {
                        if (directive_node.operands.size() != 1) {
                            fprintf(stderr, "Expected 1 operand for offset\n");
                            exit(1);
                        }

                        size_t offset =
                            get_numeric(directive_node.operands[0].value);
                        if (directive_node.operands[0].type !=
                            ValueType::IMMEDIATE) {
                            fprintf(stderr, "Expected immediate "
                                            "operand for offset\n");
                            exit(1);
                        }

                        pc += offset;
                        break;
                    }
                    case DirectiveType::INCLUDE: {
                        fprintf(stderr, "extraneous include directive! This is "
                                        "likely a compiler bug\n");
                        exit(1);
                    }
                    }
                }
                }
            }
        }

        uint8_t *rom_buf = NULL;
        rom_buf = (uint8_t *)calloc(pc, sizeof(uint8_t));
        if (rom_buf == NULL) {
            fprintf(stderr, "Failed to allocate memory!\n");
            exit(1);
        }

        pc = 0;
        for (auto &section : root.sections) {
            for (auto &element : section.elements) {
                switch (element.type) {
                case SectionElement::Type::Instruction: {
                    InstructionNode instruction_node =
                        std::get<InstructionNode>(element.element);
                    std::vector<OperandNode> flattened_operands;
                    // "flatten" opperands, aka, take things like labels and
                    // defines, and replace them with their real values, so that
                    // instructon dont need knowledge of these concepts
                    for (size_t i = 0; i < instruction_node.operands.size();
                         i++) {
                        OperandNode &operand = instruction_node.operands[i];
                        if (operand.type == ValueType::LABEL) {
                            // check if the label is really a reference
                            // to a constant (ie a value defined by a
                            // defines directive)
                            if (defines.contains(operand.value)) {
                                operand.type = defines[operand.value].type;
                                operand.value = defines[operand.value].value;
                            }

                            // might seem redundant, but if we transformed the
                            // "label" into a label via defines, we need to
                            // transform the label into the position of the
                            // label, but if the defines is NOT a label, we dont
                            // want to crash out
                            if (operand.type == ValueType::LABEL) {
                                if (labels.contains(operand.value)) {
                                    // the label points to a label we have
                                    // already discovered, so, we can
                                    // transform the label into a memory
                                    // space address, ready for the
                                    // instruction to use
                                    operand.type = ValueType::IMMEDIATE;
                                    operand.value = std::to_string(
                                        labels[operand.value] + 0x200);
                                } else {
                                    fprintf(stderr, "Label %s not found\n",
                                            operand.value.c_str());
                                    exit(1);
                                }
                            }
                        }

                        if (operand.type == ValueType::ATIHMETIC) {
                            // collect previous operand and next operand, and
                            // flatten the arithmetic
                            if (i == 0) {
                                fprintf(stderr, "expected immediate before "
                                                "arithmetic operator\n");
                                exit(1);
                            }

                            if (i + 1 >= instruction_node.operands.size()) {
                                fprintf(stderr,
                                        "expected 2 operands for arithmetic "
                                        "operator, but got only %lu\n",
                                        instruction_node.operands.size());
                                exit(1);
                            }

                            OperandNode prev_operand =
                                flattened_operands.back();
                            flattened_operands.pop_back();
                            OperandNode next_operand =
                                instruction_node.operands[i + 1];

                            if (prev_operand.type != ValueType::IMMEDIATE ||
                                prev_operand.type != ValueType::IMMEDIATE) {
                                fprintf(stderr,
                                        "Arithmetic can only be performed on "
                                        "immediate operands.\n");
                                exit(1);
                            }

                            switch (hash(operand.value)) {
                            case hash("+"):
                                operand = OperandNode(
                                    ValueType::IMMEDIATE,
                                    std::to_string(
                                        get_numeric(prev_operand.value) +
                                        get_numeric(next_operand.value)));
                                break;
                            case hash("-"):
                                operand = OperandNode(
                                    ValueType::IMMEDIATE,
                                    std::to_string(
                                        get_numeric(prev_operand.value) -
                                        get_numeric(next_operand.value)));
                                break;
                            case hash("*"):
                                operand = OperandNode(
                                    ValueType::IMMEDIATE,
                                    std::to_string(
                                        get_numeric(prev_operand.value) *
                                        get_numeric(next_operand.value)));
                                break;
                            case hash("/"):
                                operand = OperandNode(
                                    ValueType::IMMEDIATE,
                                    std::to_string(
                                        get_numeric(prev_operand.value) /
                                        get_numeric(next_operand.value)));
                                break;
                            default:
                                fprintf(stderr,
                                        "unexpected arithmetic operator: %s\n",
                                        operand.value.c_str());
                            }

                            i += 1;
                            flattened_operands.push_back(operand);
                            continue;
                        }

                        flattened_operands.push_back(operand);
                    }

                    instruction_node.operands = flattened_operands;

                    uint16_t instruction = 0;
                    switch (hash(instruction_node.opcode)) {
                    case hash("exit"): {
                        if (instruction_node.operands.size() != 1) {
                            fprintf(stderr, "Expected 1 operand for exit\n");
                            exit(1);
                        }

                        if (instruction_node.operands[0].type !=
                            ValueType::IMMEDIATE) {
                            fprintf(stderr, "Expected immediate "
                                            "operand for exit\n");
                            exit(1);
                        }

                        uint8_t byte =
                            get_numeric(instruction_node.operands[0].value);
                        if (byte > 0x0F) {
                            fprintf(stderr,
                                    "Invalid exit code: %d, must be a "
                                    "value "
                                    "less that 0x10\n",
                                    byte);
                            exit(1);
                        }

                        instruction = 0x0010 | byte;
                        break;
                    }
                    case hash("cls"): {
                        instruction = 0x00E0;
                        break;
                    }
                    case hash("ret"): {
                        instruction = 0x00EE;
                        break;
                    }
                    case hash("sys"): {
                        if (instruction_node.operands.size() != 1) {
                            fprintf(stderr,
                                    "Expected 1 operand for sys got %lu\n",
                                    instruction_node.operands.size());
                            exit(1);
                        }

                        uint16_t address =
                            get_numeric(instruction_node.operands[0].value) &
                            0xFFF;
                        instruction = 0x0000 | address;
                        break;
                    }
                    case hash("jp"): {
                        if (instruction_node.operands.size() == 2) {
                            if (instruction_node.operands[0].type !=
                                    ValueType::REGISTER ||
                                get_reg(instruction_node.operands[0].value) !=
                                    0x0) {
                                fprintf(
                                    stderr,
                                    "V0 expected for jp with two arguments\n");
                                exit(1);
                            }

                            if (instruction_node.operands[1].type !=
                                ValueType::IMMEDIATE) {
                                fprintf(stderr,
                                        "Expected immediate operand for jp\n");
                                exit(1);
                            }

                            uint16_t address =
                                get_numeric(
                                    instruction_node.operands[1].value) &
                                0xFFF;

                            instruction = 0xB000 | address;
                            break;
                        }

                        if (instruction_node.operands.size() != 1) {
                            fprintf(stderr,
                                    "Expected 1 operand for jp got %lu\n",
                                    instruction_node.operands.size());
                            exit(1);
                        }

                        if (instruction_node.operands[0].type !=
                            ValueType::IMMEDIATE) {
                            fprintf(
                                stderr,
                                "Expected immediate operand for jp got %s\n",
                                instruction_node.operands[1].value.c_str());
                            exit(1);
                        }

                        uint16_t address =
                            get_numeric(instruction_node.operands[0].value) &
                            0xFFF;
                        instruction = 0x1000 | address;
                        break;
                    }
                    case hash("call"): {
                        if (instruction_node.operands.size() != 1) {
                            fprintf(stderr,
                                    "Expected 1 operand for call got %lu\n",
                                    instruction_node.operands.size());
                            exit(1);
                        }

                        uint16_t address =
                            get_numeric(instruction_node.operands[0].value) &
                            0xFFF;
                        instruction = 0x2000 | address;
                        break;
                    }
                    case hash("se"): {
                        if (instruction_node.operands.size() != 2) {
                            fprintf(stderr,
                                    "Expected 2 operands for se got %lu\n",
                                    instruction_node.operands.size());
                            exit(1);
                        }

                        if (instruction_node.operands[0].type !=
                            ValueType::REGISTER) {
                            fprintf(stderr,
                                    "Expected register operand for se\n");
                            exit(1);
                        }

                        if (is_control(instruction_node.operands[0].value)) {
                            fprintf(stderr, "Expected register operand for "
                                            "se\n");
                            exit(1);
                        }

                        if (instruction_node.operands[1].type ==
                            ValueType::IMMEDIATE) {
                            // se reg, nnn
                            instruction =
                                0x3000 |
                                (get_reg(instruction_node.operands[0].value)
                                 << 8) |
                                get_numeric(instruction_node.operands[1].value);
                            break;
                        }
                        if (instruction_node.operands[1].type ==
                            ValueType::REGISTER) {
                            // se reg, reg
                            if (is_control(
                                    instruction_node.operands[1].value)) {
                                fprintf(stderr, "Expected register operand for "
                                                "se\n");
                                exit(1);
                            }
                            instruction =
                                0x5000 |
                                (get_reg(instruction_node.operands[0].value)
                                 << 8) |
                                (get_reg(instruction_node.operands[1].value)
                                 << 4);
                            break;
                        }
                        fprintf(stderr, "Expected register or immediate "
                                        "operand for se\n");
                        exit(1);
                    }
                    case hash("sne"): {
                        if (instruction_node.operands.size() != 2) {
                            fprintf(stderr,
                                    "Expected 2 operands for sne got %lu\n",
                                    instruction_node.operands.size());
                            exit(1);
                        }

                        if (instruction_node.operands[0].type !=
                            ValueType::REGISTER) {
                            fprintf(stderr,
                                    "Expected register operand for sne\n");
                            exit(1);
                        }

                        if (is_control(instruction_node.operands[0].value)) {
                            fprintf(stderr, "Expected register operand for "
                                            "se\n");
                            exit(1);
                        }

                        if (instruction_node.operands[1].type ==
                            ValueType::IMMEDIATE) {
                            // sne reg, nnn
                            instruction =
                                0x4000 |
                                (get_reg(instruction_node.operands[0].value)
                                 << 8) |
                                get_numeric(instruction_node.operands[1].value);
                            break;
                        } else if (instruction_node.operands[1].type ==
                                   ValueType::REGISTER) {
                            if (is_control(
                                    instruction_node.operands[1].value)) {
                                fprintf(stderr, "Expected register operand for "
                                                "se\n");
                                exit(1);
                            }

                            // sne reg, reg
                            instruction =
                                0x9000 |
                                (get_reg(instruction_node.operands[0].value)
                                 << 8) |
                                (get_reg(instruction_node.operands[1].value)
                                 << 4);
                            break;
                        }
                        fprintf(stderr, "Expected register or immediate "
                                        "operand for se\n");
                        exit(1);
                    }
                    case hash("ld"): {
                        if (instruction_node.operands.size() != 2) {
                            fprintf(stderr,
                                    "Expected 2 operands for ld got %lu\n",
                                    instruction_node.operands.size());
                            exit(1);
                        }

                        if (instruction_node.operands[0].type !=
                            ValueType::REGISTER) {
                            fprintf(stderr,
                                    "Expected register operand for ld\n");
                            exit(1);
                        }

                        std::string reg0 = instruction_node.operands[0].value;

                        if (is_control(reg0)) {
                            std::transform(reg0.begin(), reg0.end(),
                                           reg0.begin(), ::tolower);
                            switch (hash(reg0)) {
                            case hash("i"): {
                                instruction =
                                    0xA000 |
                                    (get_numeric(
                                         instruction_node.operands[1].value) &
                                     0xFFF);
                                break;
                            }
                            case hash("k"): {
                                instruction =
                                    0xF00A |
                                    (get_reg(instruction_node.operands[1].value)
                                     << 8);
                                break;
                            }
                            case hash("f"): {
                                instruction =
                                    0xF029 |
                                    (get_reg(instruction_node.operands[1].value)
                                     << 8);
                                break;
                            }
                            case hash("b"): {
                                instruction =
                                    0xF033 |
                                    (get_reg(instruction_node.operands[1].value)
                                     << 8);
                                break;
                            }
                            case hash("dt"): {
                                instruction =
                                    0xF015 |
                                    (get_reg(instruction_node.operands[1].value)
                                     << 8);
                                break;
                            }
                            case hash("st"): {
                                instruction =
                                    0xF018 |
                                    (get_reg(instruction_node.operands[1].value)
                                     << 8);
                                break;
                            }
                            case hash("[i]"): {
                                instruction =
                                    0xF055 |
                                    (get_reg(instruction_node.operands[1].value)
                                     << 8);
                                break;
                            }
                            default: {
                                fprintf(stderr,
                                        "Unknown control register: %s\n",
                                        reg0.c_str());
                                exit(1);
                            }
                            }
                            break;
                        }

                        if (instruction_node.operands[1].type ==
                            ValueType::IMMEDIATE) {
                            // ld reg, nnn
                            uint16_t val =
                                get_numeric(instruction_node.operands[1].value);
                            if (val > 0xFF) {
                                fprintf(stderr,
                                        "Invalid immediate value for ld\n");
                                exit(1);
                            }
                            instruction =
                                0x6000 |
                                (get_reg(instruction_node.operands[0].value)
                                 << 8) |
                                val;
                            break;
                        }

                        // ld reg, control
                        if (instruction_node.operands[1].type ==
                            ValueType::REGISTER) {
                            std::string reg1 =
                                instruction_node.operands[1].value;
                            if (is_control(reg1)) {
                                std::transform(reg1.begin(), reg1.end(),
                                               reg1.begin(), ::tolower);
                                switch (
                                    hash(instruction_node.operands[1].value)) {
                                case hash("dt"): {
                                    instruction =
                                        0xF007 |
                                        (get_reg(
                                             instruction_node.operands[0].value)
                                         << 8);
                                    break;
                                }
                                case hash("k"): {
                                    instruction =
                                        0xF00A |
                                        (get_reg(
                                             instruction_node.operands[0].value)
                                         << 8);
                                    break;
                                }
                                case hash("[i]"): {
                                    instruction =
                                        0xF065 |
                                        (get_reg(
                                             instruction_node.operands[0].value)
                                         << 8);
                                    break;
                                }
                                }
                                break;
                            }

                            // ld reg, reg
                            instruction =
                                0x8000 |
                                (get_reg(instruction_node.operands[0].value)
                                 << 8) |
                                (get_reg(instruction_node.operands[1].value)
                                 << 4);
                            break;
                        }

                        fprintf(stderr, "Expected register or immediate "
                                        "operand for se\n");
                        exit(1);
                        break;
                    }
                    case hash("add"): {
                        if (instruction_node.operands.size() != 2) {
                            fprintf(stderr, "Expected 2 operands for add\n");
                            exit(1);
                        }
                        if (instruction_node.operands[0].type !=
                            ValueType::REGISTER) {
                            fprintf(stderr,
                                    "Expected register operand for add\n");
                            exit(1);
                        }

                        if (is_control(instruction_node.operands[0].value)) {
                            // add I, reg
                            instruction =
                                0xF01E |
                                (get_reg(instruction_node.operands[1].value)
                                 << 8);
                            break;
                        }

                        // add reg, nn
                        if (instruction_node.operands[1].type ==
                            ValueType::REGISTER) {
                            instruction =
                                0x8004 |
                                (get_reg(instruction_node.operands[0].value)
                                 << 8) |
                                (get_reg(instruction_node.operands[1].value)
                                 << 4);
                            break;
                        }

                        instruction =
                            0x7000 |
                            (get_reg(instruction_node.operands[0].value) << 8) |
                            get_numeric(instruction_node.operands[1].value);
                        break;
                    }
                    case hash("or"): {
                        if (instruction_node.operands.size() != 2) {
                            fprintf(stderr,
                                    "Expected 2 operands for or, got %lu",
                                    instruction_node.operands.size());
                            exit(1);
                        }

                        if (instruction_node.operands[0].type !=
                                ValueType::REGISTER ||
                            instruction_node.operands[1].type !=
                                ValueType::REGISTER) {
                            fprintf(stderr, "Expected two registers for or "
                                            "instruction\n");
                            exit(1);
                        }

                        instruction =
                            0x8001 |
                            (get_reg(instruction_node.operands[0].value) << 8) |
                            (get_reg(instruction_node.operands[1].value) << 4);
                        break;
                    }
                    case hash("and"): {
                        if (instruction_node.operands.size() != 2) {
                            fprintf(
                                stderr,
                                "Expected two registers for and instruction\n");
                            exit(1);
                        }

                        if (instruction_node.operands[0].type !=
                                ValueType::REGISTER ||
                            instruction_node.operands[1].type !=
                                ValueType::REGISTER) {
                            fprintf(
                                stderr,
                                "Expected two registers for and instruction\n");
                            exit(1);
                        }

                        instruction =
                            0x8002 |
                            (get_reg(instruction_node.operands[0].value) << 8) |
                            (get_reg(instruction_node.operands[1].value) << 4);
                        break;
                    }
                    case hash("xor"): {
                        if (instruction_node.operands.size() != 2) {
                            fprintf(
                                stderr,
                                "Expected two registers for xor instruction\n");
                            exit(1);
                        }

                        if (instruction_node.operands[0].type !=
                                ValueType::REGISTER ||
                            instruction_node.operands[1].type !=
                                ValueType::REGISTER) {
                            fprintf(
                                stderr,
                                "Expected two registers for xor instruction\n");
                            exit(1);
                        }

                        instruction =
                            0x8003 |
                            (get_reg(instruction_node.operands[0].value) << 8) |
                            (get_reg(instruction_node.operands[1].value) << 4);
                        break;
                    }
                    case hash("sub"): {
                        if (instruction_node.operands.size() != 2) {
                            fprintf(
                                stderr,
                                "Expected two registers for sub instruction\n");
                            exit(1);
                        }

                        if (instruction_node.operands[0].type !=
                                ValueType::REGISTER ||
                            instruction_node.operands[1].type !=
                                ValueType::REGISTER) {
                            fprintf(
                                stderr,
                                "Expected two registers for sub instruction\n");
                            exit(1);
                        }

                        instruction =
                            0x8005 |
                            (get_reg(instruction_node.operands[0].value) << 8) |
                            (get_reg(instruction_node.operands[1].value) << 4);
                        break;
                    }
                    case hash("shr"): {
                        if (instruction_node.operands.size() == 1) {
                            if (instruction_node.operands[0].type !=
                                ValueType::REGISTER) {
                                fprintf(stderr,
                                        "Expected register operand for shr\n");
                                exit(1);
                            }

                            instruction =
                                0x8006 |
                                (get_reg(instruction_node.operands[0].value)
                                 << 8) |
                                (get_reg(instruction_node.operands[0].value)
                                 << 4);
                            break;
                        }

                        if (instruction_node.operands.size() != 2) {
                            fprintf(
                                stderr,
                                "Expected two registers for shr instruction\n");
                            exit(1);
                        }

                        if (instruction_node.operands[0].type !=
                                ValueType::REGISTER ||
                            instruction_node.operands[1].type !=
                                ValueType::REGISTER) {
                            fprintf(
                                stderr,
                                "Expected two registers for shr instruction\n");
                            exit(1);
                        }

                        instruction =
                            0x8006 |
                            (get_reg(instruction_node.operands[0].value) << 8) |
                            (get_reg(instruction_node.operands[1].value) << 4);
                        break;
                    }
                    case hash("subn"): {
                        if (instruction_node.operands.size() != 2) {
                            fprintf(stderr, "Expected two registers for subn "
                                            "instruction\n");
                            exit(1);
                        }

                        if (instruction_node.operands[0].type !=
                                ValueType::REGISTER ||
                            instruction_node.operands[1].type !=
                                ValueType::REGISTER) {
                            fprintf(stderr, "Expected two registers for subn "
                                            "instruction\n");
                            exit(1);
                        }

                        instruction =
                            0x8007 |
                            (get_reg(instruction_node.operands[0].value) << 8) |
                            (get_reg(instruction_node.operands[1].value) << 4);
                        break;
                    }
                    case hash("shl"): {
                        if (instruction_node.operands.size() == 1) {
                            if (instruction_node.operands[0].type !=
                                ValueType::REGISTER) {
                                fprintf(stderr,
                                        "Expected register operand for shl\n");
                                exit(1);
                            }

                            instruction =
                                0x800E |
                                (get_reg(instruction_node.operands[0].value)
                                 << 8) |
                                (get_reg(instruction_node.operands[0].value)
                                 << 4);
                            break;
                        }

                        if (instruction_node.operands.size() != 2) {
                            fprintf(
                                stderr,
                                "Expected two registers for shl instruction\n");
                            exit(1);
                        }

                        if (instruction_node.operands[0].type !=
                                ValueType::REGISTER ||
                            instruction_node.operands[1].type !=
                                ValueType::REGISTER) {
                            fprintf(
                                stderr,
                                "Expected two registers for shl instruction\n");
                            exit(1);
                        }

                        instruction =
                            0x800E |
                            (get_reg(instruction_node.operands[0].value) << 8) |
                            (get_reg(instruction_node.operands[1].value) << 4);
                        break;
                    }
                    case hash("rnd"): {
                        if (instruction_node.operands.size() != 2) {
                            fprintf(stderr, "Expected 2 operands for rnd\n");
                            exit(1);
                        }

                        if (instruction_node.operands[0].type !=
                            ValueType::REGISTER) {
                            fprintf(stderr,
                                    "Expected register operand for rnd\n");
                            exit(1);
                        }

                        if (instruction_node.operands[1].type !=
                            ValueType::IMMEDIATE) {
                            fprintf(stderr,
                                    "Expected immediate operand for rnd\n");
                            exit(1);
                        }

                        if (get_numeric(instruction_node.operands[1].value) >
                            0xFF) {
                            fprintf(stderr,
                                    "Invalid immediate value for rnd\n");
                            exit(1);
                        }

                        instruction =
                            0xC000 |
                            (get_reg(instruction_node.operands[0].value) << 8) |
                            (get_numeric(instruction_node.operands[1].value) &
                             0xFF);
                        break;
                    }
                    case hash("drw"): {
                        if (instruction_node.operands.size() != 3) {
                            fprintf(stderr, "Expected 3 operands for drw\n");
                            exit(1);
                        }

                        if (instruction_node.operands[0].type !=
                            ValueType::REGISTER) {
                            fprintf(stderr,
                                    "Expected register operand for drw\n");
                            exit(1);
                        }

                        if (instruction_node.operands[1].type !=
                            ValueType::REGISTER) {
                            fprintf(stderr,
                                    "Expected register operand for drw\n");
                            exit(1);
                        }

                        if (instruction_node.operands[2].type !=
                            ValueType::IMMEDIATE) {
                            fprintf(stderr,
                                    "Expected immediate operand for drw\n");
                            exit(1);
                        }

                        if (get_numeric(instruction_node.operands[2].value) >
                            0xF) {
                            fprintf(stderr,
                                    "Invalid immediate value for drw\n");
                            exit(1);
                        }

                        instruction =
                            0xD000 |
                            (get_reg(instruction_node.operands[0].value) << 8) |
                            (get_reg(instruction_node.operands[1].value) << 4) |
                            (get_numeric(instruction_node.operands[2].value) &
                             0xF);
                        break;
                    }
                    case hash("skp"): {
                        if (instruction_node.operands.size() != 1) {
                            fprintf(stderr, "Expected 1 operand for skp\n");
                            exit(1);
                        }

                        if (instruction_node.operands[0].type !=
                            ValueType::REGISTER) {
                            fprintf(stderr,
                                    "Expected register operand for skp\n");
                            exit(1);
                        }

                        instruction =
                            0xE09E |
                            (get_reg(instruction_node.operands[0].value) << 8);
                        break;
                    }
                    case hash("sknp"): {
                        if (instruction_node.operands.size() != 1) {
                            fprintf(stderr, "Expected 1 operand for sknp\n");
                            exit(1);
                        }

                        if (instruction_node.operands[0].type !=
                            ValueType::REGISTER) {
                            fprintf(stderr,
                                    "Expected register operand for sknp\n");
                            exit(1);
                        }

                        instruction =
                            0xE0A1 |
                            (get_reg(instruction_node.operands[0].value) << 8);
                        break;
                    }
                    case hash("hires"): {
                        // jp to 260
                        instruction = 0x1000 | 0x260;
                        break;
                    }
                    default: {
                        fprintf(stderr, "Unhandled instruction %s!\n",
                                instruction_node.opcode.c_str());
                        break;
                    }
                    }

                    // le to be
                    rom_buf[pc] = (instruction >> 8) & 0xFF;
                    rom_buf[pc + 1] = instruction & 0xFF;

                    pc += 2;
                    break;
                }
                case SectionElement::Type::Directive: {
                    DirectiveNode directive_node =
                        std::get<DirectiveNode>(element.element);
                    for (auto &operand : directive_node.operands) {
                        if (operand.type == ValueType::LABEL) {
                            // check if the label is really a reference
                            // to a constant (ie a value defined by a
                            // defines directive)
                            if (defines.contains(operand.value)) {
                                operand.type = defines[operand.value].type;
                                operand.value = defines[operand.value].value;
                            }
                        }
                    }

                    switch (directive_node.directive) {
                    case DirectiveType::DEFINE: {
                        // defines are preprocessed, so we can ignore them
                        break;
                    }
                    case DirectiveType::TEXT: {
                        if (directive_node.operands.size() != 1) {
                            fprintf(stderr, "Expected 1 operand for text\n");
                            exit(1);
                        }

                        if (directive_node.operands[0].type ==
                            ValueType::LABEL) {
                            if (defines.contains(
                                    directive_node.operands[0].value)) {
                            }
                        }

                        if (directive_node.operands[0].type !=
                            ValueType::STRING) {
                            fprintf(stderr,
                                    "Expected string operand for text\n");
                            exit(1);
                        }

                        size_t len =
                            directive_node.operands[0].value.size() + 1;

                        pc += len;

                        // write stirng to rom including null terminator
                        memcpy(rom_buf + pc,
                               directive_node.operands[0].value.c_str(), len);
                        break;
                    }
                    case DirectiveType::DB: {
                        if (directive_node.operands.size() == 0) {
                            fprintf(stderr, "Expected operands for db\n");
                            exit(1);
                        }

                        for (auto &operand : directive_node.operands) {
                            uint8_t byte = get_numeric(operand.value);
                            if (operand.type != ValueType::IMMEDIATE) {
                                fprintf(stderr, "Expected immediate "
                                                "operand for db\n");
                                exit(1);
                            }

                            rom_buf[pc] = byte;
                            pc += 1;
                        }
                        break;
                    }
                    case DirectiveType::DW: {
                        if (directive_node.operands.size() == 0) {
                            fprintf(stderr, "Expected operands for dw\n");
                            exit(1);
                        }

                        for (auto &operand : directive_node.operands) {
                            uint16_t word = get_numeric(operand.value);
                            if (operand.type != ValueType::IMMEDIATE) {
                                fprintf(stderr, "Expected immediate "
                                                "operand for dw\n");
                                exit(1);
                            }

                            rom_buf[pc] = word & 0xFF;
                            rom_buf[pc + 1] = word >> 8;
                            pc += 2;
                        }
                        break;
                    }
                    case DirectiveType::OFFSET: {
                        if (directive_node.operands.size() != 1) {
                            fprintf(stderr, "Expected 1 operand for offset\n");
                            exit(1);
                        }

                        size_t offset =
                            get_numeric(directive_node.operands[0].value);
                        if (directive_node.operands[0].type !=
                            ValueType::IMMEDIATE) {
                            fprintf(stderr, "Expected immediate "
                                            "operand for offset\n");
                            exit(1);
                        }

                        pc += offset;
                        break;
                    }
                    case DirectiveType::INCLUDE: {
                        fprintf(stderr, "extraneous include directive! This is "
                                        "likely a compiler bug\n");
                        exit(1);
                    }
                    }

                    break;
                }
                }
            }
        }

        write(rom_fd, rom_buf, pc);

        return nullptr;
    }

  private:
};

std::optional<std::string>
process_includes(char *data, size_t size, std::string include_dir,
                 std::unordered_set<std::string> &seen_includes) {
    std::string asm_str = std::string(data, size);
    size_t str_idx = 0;
    // find `include "%s"` and replace with the contents of the
    // file iterate over each line in the file
    std::stringstream lss(asm_str);
    std::string line;
    while (std::getline(lss, line)) {
        str_idx += line.size() + 1;

        if (line.starts_with("include")) {
            size_t pos = line.find_first_of("\"") + 1;
            size_t n = line.find_first_of("\"", pos) - pos;
            std::string file_name = line.substr(pos, n);
            std::string path = include_dir + file_name;

            if (seen_includes.contains(path)) {
                fprintf(stderr, "Recursive include detected: %s\n",
                        path.c_str());
                return std::nullopt;
            }

            seen_includes.insert(path);

            // open the file
            int include_fd = open(path.c_str(), O_RDONLY);
            if (include_fd < 0) {
                fprintf(stderr, "Failed to open include file: %s\n",
                        path.c_str());
                return std::nullopt;
            }

            // read the file
            size_t include_size = lseek(include_fd, 0, SEEK_END);
            lseek(include_fd, 0, SEEK_SET);
            char *include_data = (char *)calloc(include_size, sizeof(char));
            if (include_data == nullptr) {
                fprintf(stderr,
                        "Failed to allocate memory for include "
                        "file: %s\n",
                        path.c_str());
                return std::nullopt;
            }

            ssize_t read_size = read(include_fd, include_data, include_size);
            if (read_size < 0) {
                fprintf(stderr, "Failed to read include file: %s\n",
                        path.c_str());
                return std::nullopt;
            }
            close(include_fd);

            // recursively process the included file
            std::optional<std::string> new_data = process_includes(
                include_data, include_size, include_dir, seen_includes);
            if (!new_data.has_value()) {
                return std::nullopt;
            }
            free(include_data);

            // replace the include line with the included file
            asm_str.reserve(asm_str.size() + new_data.value().size());
            asm_str.replace(str_idx - (line.size() + 1), line.size(),
                            new_data.value());
        }
    }

    return asm_str;
}

int main(int argc, char **argv) {
    if (argc < 3) {
        printf("Usage: %s <input> <output>\n", argv[0]);
        return 1;
    }

    int asm_fd = open(argv[1], O_RDONLY);
    if (asm_fd < 0) {
        fprintf(stderr, "Failed to open file: %s\n", argv[1]);
        return 1;
    }

    int rom_fd = open(argv[2], O_RDWR | O_CREAT, 0644);
    if (rom_fd < 0) {
        fprintf(stderr, "Failed to open file: %s\n", argv[2]);
        return 1;
    }

    ftruncate(rom_fd, 0);

    size_t asm_size = lseek(asm_fd, 0, SEEK_END);
    (void)lseek(asm_fd, 0, SEEK_SET);

    void *asm_buf = calloc(asm_size, sizeof(char));
    if (asm_buf == NULL) {
        fprintf(stderr, "Failed to allocate memory!\n");
        return 1;
    }
    read(asm_fd, asm_buf, asm_size);

    std::string file_name = std::string(argv[1]);
    std::string includes_dir =
        file_name.substr(0, file_name.find_last_of('/')) + "/";

    std::unordered_set<std::string> seen_includes = {file_name};
    std::optional<std::string> asm_str = process_includes(
        static_cast<char *>(asm_buf), asm_size, includes_dir, seen_includes);
    if (!asm_str.has_value()) {
        return 1;
    }

    Lexer lexer = Lexer(asm_str.value());
    Parser parser = Parser(lexer);
    close(asm_fd);

    Assembler assembler = Assembler();
    assembler.assembler(parser.parse(), rom_fd);

    return 0;
}