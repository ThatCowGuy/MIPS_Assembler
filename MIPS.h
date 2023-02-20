#pragma once
#include <map>
#include <string>
#include <list>

class MIPS
{
    public:

        // Parsed Constants
        static std::map<std::string, int> Constants;
        static void add_constant(std::string name, int value);
        static int parse_constant(std::string input);
        // Parsed Labels
        static std::map<std::string, int> Labels;

        // Integer Registers
        static std::map<std::string, int> Registers;
        // Integer Instructions
        static std::map<std::string, int> OpCodes;
        static std::map<std::string, int> FunctionCodes;

        // Float Registers
        static std::map<std::string, int> FP_Registers;
        // Float Instructions
        static std::map<std::string, int> FP_OpCodes;
        static std::map<std::string, int> FP_FunctionCodes;
        // Format Specifiers
        static std::map<std::string, int> FMT_Specifiers;

        // Other Specifiers
        static std::map<std::string, int> Data_Specifiers;
        static std::map<std::string, int> Injection_Start_Specifiers;
        static std::map<std::string, int> RAM_Start_Specifiers;

        // Parsers
        static int parse_register(std::string);
        static int parse_opcode(std::string);
        static int parse_functioncode(std::string);
        // FP Parsers
        static int parse_fp_register(std::string);
        static int parse_fp_opcode(std::string);
        static int parse_fp_functioncode(std::string);
        static int parse_fmt(std::string);

        // Encoding Families
        //========================================================
        //  f     = Function Code        o     = OpCode
        //  d     = Destination          fd    = FP-Destination
        //  s/S   = Source               fs/fS = FP-Source
        //  r     = Register             F     = FP-Register
        //  a     = Address              m     = FMT-Specifier
        //  imm   = Immediate
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
        static std::list<std::string> FP_TRANSFER;  // INSTR   r, F           // 0100 01oo ooor rrrr FFFF F000 0000 0000
        static std::list<std::string> FP_DSS;       // INSTR.m fd, fs, fS     // 0100 01mm mmmS SSSS ssss sddd ddff ffff
        static std::list<std::string> FP_DS;        // INSTR.m fd, fs         // 0100 01mm mmm0 0000 ssss sddd ddff ffff

        // NOP
        // 0000 0000 0000 0000 0000 0000 0000 0000
        // JR s
        // 0000 00ss sss0 0000 0000 0000 0000 1000
        // LUI d, i
        // 0011 1100 000d dddd iiii iiii iiii iiii

        // (PSEUDO) B i
        // --- BEQ R0, R0, i
        //     0001 0000 0000 0000 iiii iiii iiii iiii
        // (PSEUDO) MOVE d, s
        // --- ADD d, R0, s
        //     0000 0000 000S SSSS dddd d000 0010 0000
        // (PSEUDO) LI d, i
        // --- LUI d, i
        // --- ADDIU d, d, i
        //     0011 1100 000d dddd iiii iiii iiii iiii
        //     0010 01DD DDDd dddd iiii iiii iiii iiii
        // (PSEUDO) LA d, i
        //     virtually the same as LI with the way I
        //     handle consts + labels. Replace LA => LI
};
