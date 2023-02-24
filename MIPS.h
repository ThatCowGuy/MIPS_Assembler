#pragma once
#include <map>
#include <string>
#include <list>

class MIPS
{
    public:

        // some offset counters
        static unsigned int RAM_Start;
        static unsigned int Current_PC;

        // some constants
        static const unsigned int COP1_HEAD = 0b010001;

        // Parsing Constants
        static std::map<std::string, unsigned int> Constants;
        static void add_constant(std::string name, unsigned int value);
        static unsigned int parse_constant(std::string input);
        // Parsing Labels
        static std::map<std::string, unsigned int> Labels;
        static void add_label(std::string name, unsigned int value);
        static unsigned int parse_label_absolute(std::string input);
        static unsigned int parse_label_relative(std::string input);
        static unsigned int reshape_target(unsigned int target, unsigned int max_bits);
        // Parsing Numerals
        static unsigned int parse_numeral(std::string input);
        static unsigned int parse_numeral_upper(std::string input);
        static unsigned int parse_numeral_lower(std::string input);

        // unsigned integer Registers
        static std::map<std::string, unsigned int> Registers;
        // unsigned integer Instructions
        static std::map<std::string, unsigned int> OpCodes;
        static std::map<std::string, unsigned int> FunctionCodes;

        // Float Registers
        static std::map<std::string, unsigned int> FP_Registers;
        // Float Instructions
        static std::map<std::string, unsigned int> FP_OpCodes;
        static std::map<std::string, unsigned int> FP_FunctionCodes;
        // Format Specifiers
        static std::map<std::string, unsigned int> FMT_Specifiers;
        static std::map<std::string, unsigned int> FP_TRANSER_Specifiers;

        // Other Specifiers
        static std::map<std::string, unsigned int> Data_Specifiers;
        static std::map<std::string, unsigned int> Injection_Start_Specifiers;
        static std::map<std::string, unsigned int> RAM_Start_Specifiers;

        // Parsers
        static unsigned int parse_register(std::string);
        static unsigned int parse_opcode(std::string);
        static unsigned int parse_functioncode(std::string);
        // FP Parsers
        static unsigned int parse_fp_register(std::string);
        static unsigned int parse_fp_opcode(std::string);
        static unsigned int parse_fp_functioncode(std::string);
        static unsigned int parse_fmt(std::string);
        static unsigned int parse_fp_transfer_specifier(std::string);

        // Encoding Families
        //========================================================
        //  f     = Function Code        o     = OpCode
        //  d     = Destination          fd    = FP-Destination
        //  s/S   = Source               fs/fS = FP-Source
        //  r     = Register             F     = FP-Register
        //  a     = Address              m     = FMT-Specifier
        //  imm   = Immediate            t     = Transfer Type
        //========================================================
        static std::list<std::string> HILO_MOVE;    // INSTR   d              // 0000 0000 0000 0000 dddd d000 00ff ffff
        static std::list<std::string> SPECIAL_DSS;  // INSTR   d, s, S        // 0000 00ss sssS SSSS dddd d000 00ff ffff
        static std::list<std::string> SPECIAL_DSI;  // INSTR   d, s, imm      // oooo ooss sssd dddd iiii iiii iiii iiii
        static std::list<std::string> SHIFT;        // INSTR   d, s, imm      // 0000 0000 000s ssss dddd diii iiff ffff
        static std::list<std::string> JUMP;         // INSTR   imm            // oooo ooii iiii iiii iiii iiii iiii iiii - (Immediate >> 4)
        static std::list<std::string> BRANCH_SS;    // INSTR   s, S, imm      // oooo ooss sssS SSSS iiii iiii iiii iiii - (Immediate >> 4)
        static std::list<std::string> LOADSTORE;    // INSTR   r, imm(a)      // oooo ooaa aaar rrrr iiii iiii iiii iiii
        // FP-Encoding Families
        static std::list<std::string> FP_LOADSTORE; // INSTR   F, imm(a)      // oooo ooaa aaaF FFFF iiii iiii iiii iiii
        static std::list<std::string> FP_TRANSFER;  // INSTR   r, F           // 0100 01tt tttr rrrr FFFF F000 0000 0000
        static std::list<std::string> FP_DSS;       // INSTR.m fd, fs, fS     // 0100 01mm mmmS SSSS ssss sddd ddff ffff
        static std::list<std::string> FP_DS;        // INSTR.m fd, fs         // 0100 01mm mmm0 0000 ssss sddd ddff ffff

        // and the SUPER_SPECIAL_DSS for MULT, MULTU, DIV, DIVU...
        static std::list<std::string> SUPER_SPECIAL_DSS;
        // AND the pseudo branches that translate into 2 instructions
        static std::list<std::string> PSEUDO_BRANCH_SS;

        // NOP
        // 0000 0000 0000 0000 0000 0000 0000 0000
        // JR s
        // 0000 00ss sss0 0000 0000 0000 0000 1000
        // LUI d, i
        // 0011 1100 000d dddd iiii iiii iiii iiii
};
