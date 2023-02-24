#include "MIPS.h"

#include <iostream>
#include <map>
#include <format>
#include <string>
#include <Windows.h>



#include "Functions.h"

// init the Constants and Labels Map as empty
std::map <std::string, unsigned int> MIPS::Constants = {};
std::map <std::string, unsigned int> MIPS::Labels = {};
unsigned int MIPS::RAM_Start = 0;
unsigned int MIPS::Current_PC = 0;

void MIPS::add_constant(std::string name, unsigned int value)
{
    unsigned int test = Functions::try_key_on_dict(name, MIPS::Constants);
    if (test != Functions::ERROR_CODE)
    {
        Functions::print_error("Encountered duplicate Constant !", name);
        return;
    }
    MIPS::Constants.insert( std::pair <std::string, unsigned int> (name, value));
    Functions::print_in_color(std::format("(Constant) \"{0}\" = {1:#010X} added.\n", name, value), Functions::COL_CYN);
}
unsigned int MIPS::parse_constant(std::string input)
{
    // this will never be negative => no 2's Complement
    unsigned int result = Functions::try_key_on_dict(input, MIPS::Constants);
    if (result == Functions::ERROR_CODE)
    {
        Functions::print_error("Encountered unknown Constant !", input);
        return Functions::ERROR_CODE;
    }
    return result;
}

void MIPS::add_label(std::string name, unsigned int value)
{
    unsigned int test = Functions::try_key_on_dict(name, MIPS::Labels);
    if (test != Functions::ERROR_CODE)
    {
        Functions::print_error("Encountered duplicate Label !", name);
        return;
    }
    MIPS::Labels.insert(std::pair <std::string, unsigned int>(name, value));
    Functions::print_in_color(std::format("(Label) \"{0}\" = {1:#010X} added.\n", name, value), Functions::COL_GRN);
}
unsigned int MIPS::parse_label_absolute(std::string input)
{
    // the Label will never be negative => no 2's Complement
    unsigned int result = Functions::try_key_on_dict(input, MIPS::Labels);
    if (result == Functions::ERROR_CODE)
    {
        Functions::print_error("Encountered unknown Label !", input);
        return Functions::ERROR_CODE;
    }
    return (result + MIPS::RAM_Start);
}
unsigned int MIPS::parse_label_relative(std::string input)
{
    // the Label will never be negative => no 2's Complement
    unsigned int result = Functions::try_key_on_dict(input, MIPS::Labels);
    if (result == Functions::ERROR_CODE)
    {
        Functions::print_error("Encountered unknown Label !", input);
        return Functions::ERROR_CODE;
    }
    return (result - (MIPS::Current_PC + 4));
}
unsigned int MIPS::reshape_target(unsigned int target, unsigned int max_bits)
{
    // check target alignment
    if (target % 0x4 != 0)
        return Functions::ERROR_CODE;

    // check target distance
    if (std::abs((int)target) >= std::pow(2, (max_bits + 2)))
        return Functions::ERROR_CODE;

    // shift bits because lowest 2 dont matter
    target = (target >> 2);
    // & with the maximum possible target, to cut off
    // access bits in sign extended, negative targets
    target = (target & (unsigned int)(std::pow(2, max_bits) - 1));

    return target;
}

unsigned int MIPS::parse_numeral(std::string input)
{
    // Constant Input if input starts with *@'
    if (input.find("@") == 0)
        return MIPS::parse_constant(input.substr(1));
    // Label Input if input starts with *:'
    if (input.find(":") == 0)
        return MIPS::parse_label_absolute(input.substr(1));

    // Literal Hex Input if X at position 2
    if (input.find("X") == 1)
        return std::stoul(input.substr(2), nullptr, 16);
    // Literal Binary Input if B at position 2
    if (input.find("B") == 1)
        return std::stoul(input.substr(2), nullptr, 2);

    // else, Literal Decimal Input
    return std::stoul(input, nullptr, 10);
}
unsigned int MIPS::parse_numeral_upper(std::string input)
{
    unsigned int result = MIPS::parse_numeral(input);
    return (result >> 16);
}
unsigned int MIPS::parse_numeral_lower(std::string input)
{
    unsigned int result = MIPS::parse_numeral(input);
    return (result & 0xFFFF);
}


unsigned int MIPS::parse_register(std::string input)
{
    // this will never be negative => no 2's Complement
    unsigned int result = Functions::try_key_on_dict(input, MIPS::Registers);
    if (result == Functions::ERROR_CODE)
    {
        Functions::print_error("Register can't be parsed !", input);
        return Functions::ERROR_CODE;
    }
    return result;
}
unsigned int MIPS::parse_opcode(std::string input)
{
    // this will never be negative => no 2's Complement
    unsigned int result = Functions::try_key_on_dict(input, MIPS::OpCodes);
    if (result == Functions::ERROR_CODE)
    {
        Functions::print_error("OpCode can't be parsed !", input);
        return Functions::ERROR_CODE;
    }
    return result;
}
unsigned int MIPS::parse_functioncode(std::string input)
{
    // this will never be negative => no 2's Complement
    unsigned int result = Functions::try_key_on_dict(input, MIPS::FunctionCodes);
    if (result == Functions::ERROR_CODE)
    {
        Functions::print_error("FunctionCode can't be parsed !", input);
        return Functions::ERROR_CODE;
    }
    return result;
}

unsigned int MIPS::parse_fp_register(std::string input)
{
    // this will never be negative => no 2's Complement
    unsigned int result = Functions::try_key_on_dict(input, MIPS::FP_Registers);
    if (result == Functions::ERROR_CODE)
    {
        Functions::print_error("FP_Register can't be parsed !", input);
        return Functions::ERROR_CODE;
    }
    return result;
}
unsigned int MIPS::parse_fp_opcode(std::string input)
{
    // this will never be negative => no 2's Complement
    unsigned int result = Functions::try_key_on_dict(input, MIPS::FP_OpCodes);
    if (result == Functions::ERROR_CODE)
    {
        Functions::print_error("FP_OpCode can't be parsed !", input);
        return Functions::ERROR_CODE;
    }
    return result;
}
unsigned int MIPS::parse_fp_functioncode(std::string input)
{
    // this will never be negative => no 2's Complement
    unsigned int result = Functions::try_key_on_dict(input, MIPS::FP_FunctionCodes);
    if (result == Functions::ERROR_CODE)
    {
        Functions::print_error("FP_FunctionCode can't be parsed !", input);
        return Functions::ERROR_CODE;
    }
    return result;
}
unsigned int MIPS::parse_fmt(std::string input)
{
    // this will never be negative => no 2's Complement
    unsigned int result = Functions::try_key_on_dict(input, MIPS::FMT_Specifiers);
    if (result == Functions::ERROR_CODE)
    {
        Functions::print_error("FMT_Specifier can't be parsed !", input);
        return Functions::ERROR_CODE;
    }
    return result;
}

unsigned int MIPS::parse_fp_transfer_specifier(std::string input)
{
    // trim away the "C1" from the FP_TRANSFER instruction
    input = Functions::remove_char(input, 'C');
    input = Functions::remove_char(input, '1');

    // this will never be negative => no 2's Complement
    unsigned int result = Functions::try_key_on_dict(input, MIPS::FP_TRANSER_Specifiers);
    if (result == Functions::ERROR_CODE)
    {
        Functions::print_error("FP_TRANSER_Specifiers can't be parsed !", input);
        return Functions::ERROR_CODE;
    }
    return result;
}



std::map<std::string, unsigned int> MIPS::Registers =
{
    { "R0", 0b00000 }, { "00", 0b00000 }, { "0", 0b00000 }, { "ZERO", 0b00000 },
    { "AT", 0b00001 }, { "01", 0b00001 }, { "1", 0b00001 },
    { "V0", 0b00010 }, { "02", 0b00010 }, { "2", 0b00010 },
    { "V1", 0b00011 }, { "03", 0b00011 }, { "3", 0b00011 },
    { "A0", 0b00100 }, { "04", 0b00100 }, { "4", 0b00100 },
    { "A1", 0b00101 }, { "05", 0b00101 }, { "5", 0b00101 },
    { "A2", 0b00110 }, { "06", 0b00110 }, { "6", 0b00110 },
    { "A3", 0b00111 }, { "07", 0b00111 }, { "7", 0b00111 },
    { "T0", 0b01000 }, { "08", 0b01000 }, { "8", 0b01000 },
    { "T1", 0b01001 }, { "09", 0b01001 }, { "9", 0b01001 },
    { "T2", 0b01010 }, { "10", 0b01010 },
    { "T3", 0b01011 }, { "11", 0b01011 },
    { "T4", 0b01100 }, { "12", 0b01100 },
    { "T5", 0b01101 }, { "13", 0b01101 },
    { "T6", 0b01110 }, { "14", 0b01110 },
    { "T7", 0b01111 }, { "15", 0b01111 },
    { "S0", 0b10000 }, { "16", 0b10000 },
    { "S1", 0b10001 }, { "17", 0b10001 },
    { "S2", 0b10010 }, { "18", 0b10010 },
    { "S3", 0b10011 }, { "19", 0b10011 },
    { "S4", 0b10100 }, { "20", 0b10100 },
    { "S5", 0b10101 }, { "21", 0b10101 },
    { "S6", 0b10110 }, { "22", 0b10110 },
    { "S7", 0b10111 }, { "23", 0b10111 },
    { "T8", 0b11000 }, { "24", 0b11000 },
    { "T9", 0b11001 }, { "25", 0b11001 },
    { "K0", 0b11010 }, { "26", 0b11010 },
    { "K1", 0b11011 }, { "27", 0b11011 },
    { "GP", 0b11100 }, { "28", 0b11100 },
    { "SP", 0b11101 }, { "29", 0b11101 }, { "STACK", 0b11101 },
    { "FP", 0b11110 }, { "30", 0b11110 },
    { "RA", 0b11111 }, { "31", 0b11111 }, { "R", 0b11111 },
};

std::map<std::string, unsigned int> MIPS::OpCodes =
{
    // 000 000
    { "NOP", 0b000000 }, { "SLL", 0b000000 }, { "SRL", 0b000000 }, { "SRA", 0b000000 }, { "SLLV", 0b000000 },
    { "SRLV", 0b000000 }, { "SRAV", 0b000000 }, { "JR", 0b000000 }, { "JALR", 0b000000 }, { "SYSCALL", 0b000000 },
    { "BREAK", 0b000000 }, { "SYNC", 0b000000 }, { "MFHI", 0b000000 }, { "MTHI", 0b000000 }, { "MFLO", 0b000000 },
    { "MTLO", 0b000000 }, { "DSLLV", 0b000000 }, { "DSRLV", 0b000000 }, { "DSRAV", 0b000000 }, { "MULT", 0b000000 },
    { "MULTU", 0b000000 }, { "DIV", 0b000000 }, { "DIVU", 0b000000 }, { "DMULT", 0b000000 }, { "DMULTU", 0b000000 },
    { "DDIV", 0b000000 }, { "DDIVU", 0b000000 }, { "ADD", 0b000000 }, { "ADDU", 0b000000 }, { "SUB", 0b000000 },
    { "SUBU", 0b000000 }, { "AND", 0b000000 }, { "OR", 0b000000 }, { "XOR", 0b000000 }, { "NOR", 0b000000 },
    { "SLT", 0b000000 }, { "SLTU", 0b000000 }, { "DADD", 0b000000 }, { "DADDU", 0b000000 }, { "DSUB", 0b000000 },
    { "DSUBU", 0b000000 }, { "TGE", 0b000000 }, { "TGEU", 0b000000 }, { "TLT", 0b000000 }, { "TLTU", 0b000000 },
    { "TEQ", 0b000000 }, { "TNE", 0b000000 }, { "DSLL", 0b000000 }, { "DSRL", 0b000000 }, { "DSRA", 0b000000 },
    { "DSLL32", 0b000000 }, { "DSRL32", 0b000000 }, { "DSRA32", 0b000000 },
    // 010 001
    { "MUL.S", 0b010001 }, { "DIV.S", 0b010001 }, { "ABS.S", 0b010001 }, { "ADD.S", 0b010001 }, { "SUB.S", 0b010001 },
    { "C.EQ.S", 0b010001 }, { "C.LE.S", 0b010001 }, { "C.LT.S", 0b010001 }, { "CVT.S.W", 0b010001 }, { "CVT.W.S", 0b010001 },
    { "L.S", 0b010001 }, { "MOV.S", 0b010001 }, { "NEG.S", 0b010001 }, { "S.S", 0b010001 }, { "MTC1", 0b010001 },
    { "MFC1", 0b010001 },
    // unique
    { "BLTZ",   0b000001 },
    { "BLTZL",  0b000001 },
    { "J",      0b000010 },
    { "JAL",    0b000011 },
    { "BEQ",    0b000100 },
    { "BNE",    0b000101 },
    { "BLEZ",   0b000110 },
    { "BGTZ",   0b000111 },
    { "BGEZ",   0b000001 },
    { "BGEZL",  0b000001 },
    { "ADDI",   0b001000 },
    { "ADDIU",  0b001001 },
    { "SLTI",   0b001010 },
    { "SLTIU",  0b001011 },
    { "ANDI",   0b001100 },
    { "ORI",    0b001101 },
    { "XORI",   0b001110 },
    { "LUI",    0b001111 },
    { "COP0",   0b010000 },
    { "COP1",   0b010001 },
    { "COP2",   0b010010 },
    { "MULT.S", 0b010001 },
    { "BEQL",   0b010100 },
    { "BNEL",   0b010101 },
    { "BLEZL",  0b010110 },
    { "BGTZL",  0b010111 },
    { "DADDI",  0b011000 },
    { "DADDIU", 0b011001 },
    { "LDL",    0b011010 },
    { "LDR",    0b011011 },
    { "LB",     0b100000 },
    { "LH",     0b100001 },
    { "LWL",    0b100010 },
    { "LW",     0b100011 },
    { "LBU",    0b100100 },
    { "LHU",    0b100101 },
    { "LWR",    0b100110 },
    { "LWU",    0b100111 },
    { "SB",     0b101000 },
    { "SH",     0b101001 },
    { "SWL",    0b101010 },
    { "SW",     0b101011 },
    { "SBU",    0b101100 },
    { "SHU",    0b101101 },
    { "SWR",    0b101110 },
    { "P",      0b101111 },
    { "LL",     0b110000 },
    { "LWC1",   0b110001 }, // unsure if this is technically correct        caje 110001|01 - 010001|01
    { "LLD",    0b110100 },
    { "LD",     0b110111 },
    { "SC",     0b111000 },
    { "SWC1",   0b111001 }, // unsure if this is technically correct
    { "SLD",    0b111100 },
    { "SD",     0b111111 },
};

std::map<std::string, unsigned int> MIPS::FunctionCodes =
{
    { "SLL",    0b000000 },
    { "SRL",    0b000010 },
    { "SRA",    0b000011 },
    { "SLLV",   0b000100 },
    { "SRLV",   0b000110 },
    { "SRAV",   0b000111 },
    { "MFHI",   0b010000 },
    { "MTHI",   0b010001 },
    { "MFLO",   0b010010 },
    { "MTLO",   0b010011 },
    { "MULT",   0b011000 },
    { "MULTU",  0b011001 },
    { "DIV",    0b011010 },
    { "DIVU",   0b011011 },
    { "ADD",    0b100000 },
    { "ADDU",   0b100001 },
    { "SUB",    0b100010 },
    { "SUBU",   0b100011 },
    { "AND",    0b100100 },
    { "OR",     0b100101 },
    { "XOR",    0b100110 },
    { "NOR",    0b100111 },
    { "SLT",    0b101010 },
    { "SLTU",   0b101011 },
};

std::map<std::string, unsigned int> MIPS::FP_Registers =
{
    { "F00", 0b00000 }, { "F0", 0b00000 },
    { "F01", 0b00001 }, { "F1", 0b00001 },
    { "F02", 0b00010 }, { "F2", 0b00010 },
    { "F03", 0b00011 }, { "F3", 0b00011 },
    { "F04", 0b00100 }, { "F4", 0b00100 },
    { "F05", 0b00101 }, { "F5", 0b00101 },
    { "F06", 0b00110 }, { "F6", 0b00110 },
    { "F07", 0b00111 }, { "F7", 0b00111 },
    { "F08", 0b01000 }, { "F8", 0b01000 },
    { "F09", 0b01001 }, { "F9", 0b01001 },
    { "F10", 0b01010 },
    { "F11", 0b01011 },
    { "F12", 0b01100 },
    { "F13", 0b01101 },
    { "F14", 0b01110 },
    { "F15", 0b01111 },
    { "F16", 0b10000 },
    { "F17", 0b10001 },
    { "F18", 0b10010 },
    { "F19", 0b10011 },
    { "F20", 0b10100 },
    { "F21", 0b10101 },
    { "F22", 0b10110 },
    { "F23", 0b10111 },
    { "F24", 0b11000 },
    { "F25", 0b11001 },
    { "F26", 0b11010 },
    { "F27", 0b11011 },
    { "F28", 0b11100 },
    { "F29", 0b11101 },
    { "F30", 0b11110 },
    { "F31", 0b11111 },
};

std::map<std::string, unsigned int> MIPS::FP_OpCodes =
{
    { "MFC1",       0b00000 }, // MFC1 d, fs - 0100 01oo oood dddd ssss s000 0000 0000
    { "DMFC1",      0b00001 }, // double

    { "MTC1",       0b00100 },
    { "DMTC1",      0b00101 }, // double
};

std::map<std::string, unsigned int> MIPS::FP_FunctionCodes =
{
    { "ADD",      0b000000 }, // ADD.FMT D, s, S - 0100 01mm mmmS SSSS ssss sDDD DDff ffff - m = Format, NOTE: s and S are swapped
    { "SUB",      0b000001 }, // .
    { "MUL",      0b000010 }, // .
    { "DIV",      0b000011 }, // .

    { "SQRT",     0b000100 }, // SQRT.FMT D, s - 0100 01mm mmm0 0000 ssss sDDD DDff ffff
    { "ABS",      0b000101 }, // .
    { "MOV",      0b000110 }, // .
    { "NEG",      0b000111 }, // .

    { "ROUND.W",  0b001100 }, // .
    { "TRUNC.W",  0b001101 }, // .
    { "CEIL.W",   0b001110 }, // .
    { "FLOOR.W",  0b001111 }, // .

    { "CVT.S",    0b100000 }, // .
    { "CVT.W",    0b100100 }, // .
};

std::map<std::string, unsigned int> MIPS::FMT_Specifiers = 
{
    // Note: These are the FMT encoding, NOT the FMT-3 ones...
    { "S", 0b10000 },
    { "D", 0b10001 },

    { "W", 0b10100 },
    { "L", 0b10101 },
};

std::map<std::string, unsigned int> MIPS::FP_TRANSER_Specifiers =
{
    { "MF",  0b00000 },
    { "DMF", 0b00001 },

    { "MT",  0b00100 },
    { "DMT", 0b00101 },
};

std::map<std::string, unsigned int> MIPS::Data_Specifiers =
{
    { ".WORD", 0x4 },
    { ".HALF", 0x2 }, { ".HALFWORD",  0x2 },
    { ".BYTE", 0x1 },
};
std::map<std::string, unsigned int> MIPS::Injection_Start_Specifiers =
{
    { ".ORG",       0x4 },
    { ".ROMORG",    0x4 },
    { ".ROM_ORG",   0x4 },
    { ".FILEORG",   0x4 },
    { ".FILE_ORG",  0x4 },
};
std::map<std::string, unsigned int> MIPS::RAM_Start_Specifiers =
{
    { ".RAMORG",    0x4 },
    { ".RAM_ORG",   0x4 },
    { ".RAMSTART",  0x4 },
    { ".RAM_START", 0x4 },
};


// Encoding Families
std::list<std::string> MIPS::HILO_MOVE =
{
    // INSTR d
    // 0000 0000 0000 0000 dddd d000 00ff ffff
    "MFLO", "MTLO", "MFHI", "MTHI", 
};
std::list<std::string> MIPS::SPECIAL_DSS =
{
    // INSTR d, s, S
    // 0000 00ss sssS SSSS dddd d000 00ff ffff
    "MULT", "MULTU", "DIV", "DIVU", "ADD", "ADDU",
    "SUB", "SUBU", "AND", "OR", "XOR", "NOR",
    "SLT",
};
std::list<std::string> MIPS::SPECIAL_DSI =
{
    // INSTR d, s, imm
    // oooo ooss sssd dddd iiii iiii iiii iiii
    "ADDI", "ADDIU", "DADDI", "DADDIU", "SLTI", "SLTIU",
    "ANDI", "ORI", "XORI",
};
std::list<std::string> MIPS::SHIFT =
{
    // INSTR d, s, imm
    // 0000 0000 000s ssss dddd diii iiff ffff
    "SLL", "SRL", "SRA", "SLLV", "SRLV", "SRAV",        // < there is no SLA
};
std::list<std::string> MIPS::JUMP =
{
    // INSTR imm - (Immediate >> 2)
    // oooo ooii iiii iiii iiii iiii iiii iiii
    "JAL", "J",
};
std::list<std::string> MIPS::BRANCH_SS =
{
    // INSTR s, S, imm - (Immediate >> 2)
    // oooo ooss sssS SSSS iiii iiii iiii iiii
    "BEQ", "BNE", "BEQL", "BNEL",
};
std::list<std::string> MIPS::LOADSTORE =
{
    // INSTR r, imm(a)
    // oooo ooaa aaar rrrr iiii iiii iiii iiii
    "LB", "LH", "LW", "LBU", "LHU", "LWU", 
    "SB", "SH", "SW", "SBU", "SHU",                      // < there is no SWU
    "LWL", "LWR", "SWL", "SWR",
};
std::list<std::string> MIPS::FP_LOADSTORE =
{
    // INSTR F, imm(a)
    // oooo ooaa aaaF FFFF iiii iiii iiii iiii
    "SWC1", "LWC1",
};
std::list<std::string> MIPS::FP_TRANSFER =
{
    // FP_TRANSFER r, F
    // 0100 01tt tttr rrrr FFFF F000 0000 0000
    "MFC1", "DMFC1", "MTC1", "DMTC1",
};
std::list<std::string> MIPS::FP_DSS =
{
    // INSTR.m fd, fs, fS - (sources are swapped)
    // 0100 01mm mmmS SSSS ssss sddd ddff ffff
    "ADD", "SUB", "MUL", "DIV",
};
std::list<std::string> MIPS::FP_DS =
{
    // INSTR.m fd, fs
    // 0100 01mm mmm0 0000 ssss sddd ddff ffff
    "SQRT", "ABS", "MOV", "NEG", "ROUND.W", "TRUNC.W",
    "CEIL.W", "FLOOR.W", "CVT.S", "CVT.W",
};

std::list<std::string> MIPS::SUPER_SPECIAL_DSS =
{
    // the Instructions MULT, DIV, MULTI and DIVU are special YET AGAIN...
    // User could input "DIV d, s, S" which is PSEUDO-Instruction for
    //    (DIV s, S) + (MFLO d)
    // or User could input "DIV s, S" (implying they know to use MFLO after)
    // this can be reshaped into a SPECIAL_DSS Instruction regardless:
    //    "DIV s, S" == "DIV R0, s, S"
    "MULT", "MULTU", "DIV", "DIVU",
};
std::list<std::string> MIPS::PSEUDO_BRANCH_SS =
{
    // PSEUDOs that translate into an SLT+BEQ/BNE combi
    // INSTR s, S, imm - (Immediate >> 2)
    "BLT", "BGT", "BLE", "BGE",
};