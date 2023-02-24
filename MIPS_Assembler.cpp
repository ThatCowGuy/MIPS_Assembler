#include <iostream>
#include <map>
#include <string>
#include <format>
#include <Windows.h>
#include <fstream>
#include <filesystem>

#include "MIPS.h"
#include "Functions.h"

int main(int argc, char* argv[])
{
    std::string ASM_filename;
    std::string BIN_filename;
    if (argc == 3)
    {
        // grabbing some filenames
        ASM_filename = argv[1];
        BIN_filename = argv[2];
        // Check if the Files exist before proceeding
        if (ASM_filename.find(".asm") == std::string::npos)
        {
            Functions::print_in_color(std::format("ASM File \"{}\" doesn't seem to be a .asm File...\n", ASM_filename), Functions::COL_WHI_ON_RED);
            std::cin.get();
            return -1;
        }
        if (std::filesystem::exists(ASM_filename) == false)
        {
            Functions::print_in_color(std::format("ASM File \"{}\" doesn't exist / inaccessable.\n", ASM_filename), Functions::COL_WHI_ON_RED);
            std::cin.get();
            return -1;
        }
        if (std::filesystem::exists(BIN_filename) == false)
        {
            Functions::print_in_color(std::format("BIN File \"{}\" doesn't exist / inaccessable.\n", BIN_filename), Functions::COL_WHI_ON_RED);
            std::cin.get();
            return -1;
        }
    }
    else
    {
        // defining some default filenames
        ASM_filename = "testing.asm";
        BIN_filename = "blank.bin";
    }
    // generating some filenames based on the given ones
    std::string LOG_filename = ASM_filename.substr(0, ASM_filename.find(".")) + "_Log.txt";
    std::string BackUp_filename = BIN_filename.substr(0, BIN_filename.find(".")) + "_BackUp.bin";


    // open the ASM file which we want to assemble
    std::ifstream ASM_File(ASM_filename);
    // create a list of strings, which will contain all the
    // FILTERED and IMPORTANT lines of the ASM file
    std::list <std::string> ASM_lines;
    std::string current_line;

    MIPS::Current_PC = 0;
    ////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////
    ////////       PASS-THROUGH #01 - FILTERING AND GRABBING SYMBOLS      //////////
    ////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////
    while (std::getline(ASM_File, current_line))
    {
        //-------------------------------------------------------
        // CATCH + PRESERVE .STRING DATA
        //-------------------------------------------------------
        if (Functions::convert_to_upper(current_line).find(".STRING") != std::string::npos)
        {
            // ---------------------------------------------------------------------
            // .STRING "CONTENT"
            // ---------------------------------------------------------------------
            // split line into 3 substrings, sub[1] will contain the actual CONTENT.
            // in this pass, we only want to get the size of CONTENT for PC control
            std::vector<std::string> string_split = Functions::split_string_at(current_line, '\"');
            // arg control
            if (string_split.size() != 3)
            {
                Functions::print_error(".STRING data entry seems to be malformatted !", current_line);
                continue;
            }
            // alignment control
            while (MIPS::Current_PC % 4 > 0)
                MIPS::Current_PC++;
            // add the bytesize of the parsed string +1 for '\0' to PC
            MIPS::Current_PC += (string_split[1].size() + 1);

            // also add the un-editted line to our collection
            ASM_lines.push_back(current_line);
            continue;
        }
        //-------------------------------------------------------
        // REMOVE CAPITALIZATION
        //-------------------------------------------------------
        // first convert everything to upper, so capitalization doesnt matter
        current_line = Functions::convert_to_upper(current_line);

        //-------------------------------------------------------
        // REMOVE COMMENTS
        //-------------------------------------------------------
        // trim away all comments (couple different comment formats)
        current_line = current_line.substr(0, current_line.find("//"));
        current_line = current_line.substr(0, current_line.find("#"));
        current_line = current_line.substr(0, current_line.find(";"));
        if (current_line.size() == 0) continue;
        //-------------------------------------------------------
        // REMOVE INDENTATION + TRAILS
        //-------------------------------------------------------
        // this is kinda of slick in C++ actually
        current_line = Functions::trim_whitespace(current_line);
        if (current_line.size() == 0) continue;

        //-------------------------------------------------------
        // REMOVE SPECIAL CHARS + MULTI-SPACES
        //-------------------------------------------------------
        // remove all commas
        current_line = Functions::remove_char(current_line, ',');
        // remove all dollars ("$T0" == "T0" etc.)
        current_line = Functions::remove_char(current_line, '$');
        // remove all closing brackets (in LOADSTORE "0x40(T0)" no need for the closing bracket)
        current_line = Functions::remove_char(current_line, ')');
        // and replace all opening brackes with spaces, to sepperate the address
        current_line = Functions::replace_char(current_line, '(', ' ');
        // and filter out all double-spaces
        current_line = Functions::remove_double_spaces(current_line);

        //-------------------------------------------------------
        // SPLIT INTO ARGS
        //-------------------------------------------------------
        std::vector<std::string> parsed_args = Functions::split_string_at(current_line, ' ');
        //-------------------------------------------------------
        // CONSTANT DEFINTIONS
        //-------------------------------------------------------
        if (parsed_args.size() == 2 && parsed_args[0].find("[") != std::string::npos)
        {
            // grab the name of the constant
            parsed_args[0] = Functions::remove_char(parsed_args[0], ':');
            parsed_args[0] = Functions::remove_char(parsed_args[0], '[');
            parsed_args[0] = Functions::remove_char(parsed_args[0], ']');
            // parse the value, and add it to the collection
            int value = MIPS::parse_numeral(parsed_args[1]);
            MIPS::add_constant(parsed_args[0], value);

            // discard line for Passthrough #02
            continue;
        }
        //-------------------------------------------------------
        // LABEL DEFINITIONS
        //-------------------------------------------------------
        if (parsed_args.size() == 1 && parsed_args[0].find(":") != std::string::npos)
        {
            // grab the name of the label
            parsed_args[0] = Functions::remove_char(parsed_args[0], ':');
            // and add it (with the Current-PC) to the collection
            MIPS::add_label(parsed_args[0], MIPS::Current_PC);

            // discard line for Passthrough #02
            continue;
        }
        //-------------------------------------------------------
        // INJECTION-POINTS + DATA INSERTS
        //-------------------------------------------------------
        else if (parsed_args[0][0] == '.')
        {
            if (Functions::try_key_on_dict(parsed_args[0], MIPS::RAM_Start_Specifiers) != Functions::ERROR_CODE)
            {
                // set the RAM start to the parsed value
                MIPS::RAM_Start = MIPS::parse_numeral(parsed_args[1]);
                Functions::print_in_color(std::format("(RAM_Start) {0:#010X}\n", MIPS::RAM_Start), Functions::COL_MAG);

                // discard line for Passthrough #02
                continue;
            }
            if (Functions::try_key_on_dict(parsed_args[0], MIPS::Injection_Start_Specifiers) != Functions::ERROR_CODE)
            {
                // set the Current_PC to the injection point
                MIPS::Current_PC = MIPS::parse_numeral(parsed_args[1]);
                Functions::print_in_color(std::format("(File_Offset) {0:#010X}\n", MIPS::Current_PC), Functions::COL_MAGL);

                // add this line for passthrough #02
                ASM_lines.push_back(current_line);
                continue;
            }
            if (Functions::try_key_on_dict(parsed_args[0], MIPS::Data_Specifiers) != Functions::ERROR_CODE)
            {
                // NOTE: on passthrough #01 we only care about the size of these

                // get the desired datasize
                unsigned int data_size = Functions::try_key_on_dict(parsed_args[0], MIPS::Data_Specifiers);
                // alignment control
                while (MIPS::Current_PC % data_size > 0)
                {
                    // here we would pad on pass #2, but we only increment
                    // the Current_PC on pass #1 and pretend we padded
                    MIPS::Current_PC++;
                }
                // increment PC for every input recieved
                for (unsigned int i = 1; i < parsed_args.size(); i++)
                {
                    MIPS::Current_PC += data_size;
                }
                // add this line for passthrough #02
                ASM_lines.push_back(current_line);
                continue;
            }
        }
        //-------------------------------------------------------
        // PSEUDO INSTRUCTIONS
        //-------------------------------------------------------
        // alignment control
        while (MIPS::Current_PC % 4 > 0)
        {
            // here we would pad on pass #2, but we only increment
            // the Current_PC on pass #1 and pretend we padded
            MIPS::Current_PC++;
        }
        // if we got up to here, we are instructions.
        // we dont care about parsing those on pass #1, but we have
        // to check if the instruction is a MULTI-LINE one, aka.
        // a PSEUDO that gets unrolled into multiple Instructions.
        if (parsed_args[0] == "LI" || parsed_args[0] == "LA")
        {
            // arg control
            if (parsed_args.size() != 3)
            {
                Functions::print_error("LI or LA PSEUDO-Instruction has wrong amount of Arguments !", current_line);
                continue;
            }
            // (PSEUDO) LI d, i ==>
            //     LUI d, i
            //     ADDIU d, d, i
            ASM_lines.push_back(std::format("LUI {} {}", parsed_args[1], parsed_args[2]));
            ASM_lines.push_back(std::format("ADDIU {} {} {}", parsed_args[1], parsed_args[1], parsed_args[2]));
            MIPS::Current_PC += 8;
            continue;
        }
        if (parsed_args[0] == "B")
        {
            // arg control
            if (parsed_args.size() != 2)
            {
                Functions::print_error("B PSEUDO-Instruction has wrong amount of Arguments !", current_line);
                continue;
            }
            // (PSEUDO) B imm ==> BEQ, R0, R0, imm
            ASM_lines.push_back(std::format("BEQ R0 R0 {}", parsed_args[1]));
            MIPS::Current_PC += 4;
            continue;
        }
        if (parsed_args[0] == "MOVE")
        {
            // arg control
            if (parsed_args.size() != 3)
            {
                Functions::print_error("MOVE PSEUDO-Instruction has wrong amount of Arguments !", current_line);
                continue;
            }
            // (PSEUDO) MOVE d, s ==> ADDU d, R0, s
            ASM_lines.push_back(std::format("ADDU {} R0 {}", parsed_args[1], parsed_args[2]));
            MIPS::Current_PC += 4;
            continue;
        }
        if (parsed_args[0] == "SUBI")
        {
            // arg control
            if (parsed_args.size() != 4)
            {
                Functions::print_error("SUBI PSEUDO-Instruction has wrong amount of Arguments !", current_line);
                continue;
            }
            // (PSEUDO) SUBI d, s, imm ==> ADDI d, s, -imm
            // first, we get the negative rep of the immediate
            unsigned int immediate = MIPS::parse_numeral_lower(parsed_args[3]);
            std::string negative_imm = std::format("0X{0:04X}", Functions::twos_complement(immediate));
            // then we can reassemble the function into a real one
            ASM_lines.push_back(std::format("ADDI {} {} {}", parsed_args[1], parsed_args[2], negative_imm));
            MIPS::Current_PC += 4;
            continue;
        }
        if (Functions::list_contains(MIPS::PSEUDO_BRANCH_SS, parsed_args[0]) == true)
        {
            // arg control
            if (parsed_args.size() != 4)
            {
                Functions::print_error("BRANCH_SS PSEUDO-Instruction has wrong amount of Arguments !", current_line);
                continue;
            }
            // (PSEUDO) INSTR s, S, imm - (Immediate >> 2)

            // BLT ==> SLT AT, s, S
            // BGT ==> SLT AT, S, s  -  sources are swapped
            // BLE ==> SLT AT, S, s  -  sources are swapped
            // BGE ==> SLT AT, s, S
            if (parsed_args[0] == "BLT" || parsed_args[0] == "BGE")
            {
                ASM_lines.push_back(std::format("SLT AT {} {}", parsed_args[1], parsed_args[2]));
                MIPS::Current_PC += 4;
            }
            if (parsed_args[0] == "BGT" || parsed_args[0] == "BLE")
            {
                ASM_lines.push_back(std::format("SLT AT {} {}", parsed_args[2], parsed_args[1]));
                MIPS::Current_PC += 4;
            }
            // BLT ==> BNE AT, R0, imm
            // BGT ==> BNE AT, R0, imm
            // BLE ==> BEQ AT, R0, imm
            // BGE ==> BEQ AT, R0, imm
            if (parsed_args[0] == "BLT" || parsed_args[0] == "BGT")
            {
                ASM_lines.push_back(std::format("BNE AT R0 {}", parsed_args[3]));
                MIPS::Current_PC += 4;
            }
            if (parsed_args[0] == "BLE" || parsed_args[0] == "BGE")
            {
                ASM_lines.push_back(std::format("BEQ AT R0 {}", parsed_args[3]));
                MIPS::Current_PC += 4;
            }
            continue;
        }
        //-------------------------------------------------------
        // INSTRUCTIONS
        //-------------------------------------------------------
        // the Instructions MULT, DIV, MULTI and DIVU are special YET AGAIN...
        // User could input "DIV d, s, S" which is PSEUDO-Instruction for
        //    (DIV s, S) + (MFLO d)
        // or User could input "DIV s, S" (implying they know to use MFLO after)
        // this can be reshaped into a SPECIAL_DSS Instruction regardless:
        //    "DIV s, S" == "DIV R0, s, S"
        if (Functions::list_contains(MIPS::SUPER_SPECIAL_DSS, parsed_args[0]) == true)
        {
            // Version #1 - "INSTR s, S"
            //          ==> "INSTR R0, s, S"
            if (parsed_args.size() == 3)
            {
                ASM_lines.push_back(std::format("{} R0 {} {}", parsed_args[0], parsed_args[1], parsed_args[2]));
                MIPS::Current_PC += 4;
                continue;
            }
            // Version #2 - "INSTR d, s, S"
            //          ==> "INSTR R0, s, S"
            //          ==> "MFLO d"
            if (parsed_args.size() == 4)
            {
                ASM_lines.push_back(std::format("{} R0 {} {}", parsed_args[0], parsed_args[2], parsed_args[3]));
                ASM_lines.push_back(std::format("MFLO {}", parsed_args[1]));
                MIPS::Current_PC += 8;
                continue;
            }
        }
        // if we got up to here, we are facing a REAL instruction.
        // these all have a size of exactly 4, and that's all we
        // care about for this passthrough...
        MIPS::Current_PC += 4;
        // and also, we need to add the line for passthrough #02...
        ASM_lines.push_back(current_line);
    }
    // we are done with the ASM file now
    ASM_File.close();




    // create a backup of the editted binary File before doing anything
    std::filesystem::copy(BIN_filename, BackUp_filename, std::filesystem::copy_options::overwrite_existing);

    // now open the to-be-editted binary file and get going
    std::fstream BIN_File(BIN_filename, std::iostream::in | std::iostream::out | std::iostream::binary);
    // and open the LOG file
    std::fstream LOG_File(LOG_filename, std::iostream::out | std::iostream::trunc);

    MIPS::Current_PC = 0;
    ////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////
    ////////      PASS-THROUGH #02 - ACTUAL ASSEMBLY OF INSTRUCTIONS      //////////
    ////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////
    // we can now assume that there is no "garbage" in any of the lines anymore
    // also, we can assume that there are no Constant- or Label-Definitions
    // also, we can assume that (MULT, MULTU, DIV, DIVU) were translated into
    // normal SPECIAL_DSS instructions... so no need to check for that anymore
    for (std::list<std::string>::iterator line = ASM_lines.begin(); line != ASM_lines.end(); line++)
    {
        // deref the line iterator
        current_line = *line;

        //-------------------------------------------------------
        // INSERT .STRING DATA
        //-------------------------------------------------------
        if (Functions::convert_to_upper(current_line).find(".STRING") != std::string::npos)
        {
            // ---------------------------------------------------------------------
            // .STRING "CONTENT"
            // ---------------------------------------------------------------------
            // split line into 3 substrings, sub[1] will contain the actual CONTENT.
            std::vector<std::string> string_split = Functions::split_string_at(current_line, '\"');
            // arg control
            if (string_split.size() != 3)
            {
                Functions::print_error(".STRING data entry seems to be malformatted !", current_line);
                continue;
            }
            // alignment control
            while (MIPS::Current_PC % 4 > 0)
            {
                // pad file and increment PC
                Functions::write_to_file(BIN_File, 0x00, 1);
                Functions::write_to_txt_file(LOG_File, std::format("{0:08X} 0x00 Padding\n", MIPS::Current_PC));
                MIPS::Current_PC++;
            }
            // write the string content to the file and increment PC
            Functions::write_to_file(BIN_File, string_split[1]);
            MIPS::Current_PC += (string_split[1].size() + 1);
        }
        //-------------------------------------------------------
        // SPLIT INTO ARGS
        //-------------------------------------------------------
        std::vector<std::string> parsed_args = Functions::split_string_at(current_line, ' ');
        //-------------------------------------------------------
        // INJECTION-POINTS + DATA INSERTS
        //-------------------------------------------------------
        if (parsed_args[0][0] == '.')
        {
            // Ram_Start is handled in passthrough #01

            // .ORG - INJECTION-POINTS
            if (Functions::try_key_on_dict(parsed_args[0], MIPS::Injection_Start_Specifiers) != Functions::ERROR_CODE)
            {
                // set the Current_PC to the injection point
                unsigned int offset = MIPS::parse_numeral(parsed_args[1]);
                MIPS::Current_PC = offset;
                BIN_File.seekg(offset);
                Functions::write_to_txt_file(LOG_File, std::format("\n.ORG = {0:08X}\n", MIPS::Current_PC));
                continue;
            }
            // .DATA - DATA INSERTS
            if (Functions::try_key_on_dict(parsed_args[0], MIPS::Data_Specifiers) != Functions::ERROR_CODE)
            {
                // get the desired datasize and the corresponding value
                unsigned int data_size = Functions::try_key_on_dict(parsed_args[0], MIPS::Data_Specifiers);

                // alignment control
                while (MIPS::Current_PC % data_size > 0)
                {
                    // pad file and increment PC
                    Functions::write_to_file(BIN_File, 0x00, 1);
                    Functions::write_to_txt_file(LOG_File, std::format("{0:08X} 0x00 Padding\n", MIPS::Current_PC));
                    MIPS::Current_PC++;
                }
                // write data to the file and increment PC for every input recieved
                for (int i = 1; i < parsed_args.size(); i++)
                {
                    unsigned int value = MIPS::parse_numeral(parsed_args[i]);
                    Functions::write_to_file(BIN_File, value, data_size);
                    switch (data_size)
                    {
                        case (0x1):
                            Functions::write_to_txt_file(LOG_File, std::format("{0:08X} [x] -       {1:02X} - {2}\n", MIPS::Current_PC, value, current_line));
                            break;
                        case (0x2):
                            Functions::write_to_txt_file(LOG_File, std::format("{0:08X} [x] -     {1:04X} - {2}\n", MIPS::Current_PC, value, current_line));
                            break;
                        case (0x4):
                            Functions::write_to_txt_file(LOG_File, std::format("{0:08X} [x] - {1:08X} - {2}\n", MIPS::Current_PC, value, current_line));
                            break;
                    }
                    MIPS::Current_PC += data_size;
                }
                continue;
            }
        }
        //-------------------------------------------------------
        // INSTRUCTIONS
        //-------------------------------------------------------
        // if we get to here, we know that some instruction has to come next,
        // so we definetly need to do alignment control
        while (MIPS::Current_PC % 4 > 0)
        {
            // pad file and increment PC
            Functions::write_to_file(BIN_File, 0x00, 1);
            Functions::write_to_txt_file(LOG_File, std::format("{0:08X} 0x00 Padding\n", MIPS::Current_PC));
            MIPS::Current_PC++;
        }
        // we also replaced every PSEUDO-Instruction with their actual
        // MIPS lines, so we can assume that no more PSEUDOs will come
        
        // Instruction NOP
        if (parsed_args[0] == "NOP")
        {
            // NOP
            // 0000 0000 0000 0000 0000 0000 0000 0000
            unsigned int instruction = 0x00;

            Functions::write_to_file(BIN_File, instruction, 4);
            Functions::write_to_txt_file(LOG_File, std::format("{0:08X} [x] - {1:08X} - {2}\n", MIPS::Current_PC, instruction, current_line));
            MIPS::Current_PC += 4;
            continue;
        }
        // Instruction JR
        if (parsed_args[0] == "JR")
        {
            // arg control
            if (parsed_args.size() != 2)
            {
                Functions::print_error("JR Instruction has wrong amount of Arguments !", current_line);
                continue;
            }
            // JR s
            // 0000 00ss sss0 0000 0000 0000 0000 1000
            unsigned int src_reg = MIPS::parse_register(parsed_args[1]);
            // build instruction
            unsigned int instruction = (src_reg << 21) + (0b1000);

            Functions::write_to_file(BIN_File, instruction, 4);
            Functions::write_to_txt_file(LOG_File, std::format("{0:08X} [x] - {1:08X} - {2}\n", MIPS::Current_PC, instruction, current_line));
            MIPS::Current_PC += 4;
            continue;
        }
        // Instruction LUI
        if (parsed_args[0] == "LUI")
        {
            // arg control
            if (parsed_args.size() != 3)
            {
                Functions::print_error("LUI Instruction has wrong amount of Arguments !", current_line);
                continue;
            }
            // LUI D, Imm
            // 0011 1100 000d dddd iiii iiii iiii iiii
            unsigned int dst_reg = MIPS::parse_register(parsed_args[1]);
            unsigned int immediate = MIPS::parse_numeral_upper(parsed_args[2]);
            // if we detect a Literal with 4 or less actual Hex Digits, assume that it
            // was meant to represent the upper 16 bits. Overwrite the immediate then:
            if (parsed_args[2].find("X") != std::string::npos && parsed_args[2].size() <= 6)
            {
                immediate = MIPS::parse_numeral_lower(parsed_args[2]);
            }
            // build instruction
            unsigned int instruction = (0b001111 << 26) + (dst_reg << 16) + (immediate);

            Functions::write_to_file(BIN_File, instruction, 4);
            Functions::write_to_txt_file(LOG_File, std::format("{0:08X} [x] - {1:08X} - {2}\n", MIPS::Current_PC, instruction, current_line));
            MIPS::Current_PC += 4;
            continue;
        }
        // Instruction Type SPECIAL_DSI
        if (Functions::list_contains(MIPS::SPECIAL_DSI, parsed_args[0]) == true)
        {
            // arg control
            if (parsed_args.size() != 4)
            {
                Functions::print_error("SPECIAL_DSI instruction has wrong amount of Arguments !", current_line);
                continue;
            }
            // SPECIAL_DSI D, S, Imm
            // oooo ooss sssd dddd iiii iiii iiii iiii
            unsigned int opcode = MIPS::parse_opcode(parsed_args[0]);
            unsigned int dst_reg = MIPS::parse_register(parsed_args[1]);
            unsigned int src_reg = MIPS::parse_register(parsed_args[2]);
            unsigned int immediate = MIPS::parse_numeral_lower(parsed_args[3]);
            // build instruction
            unsigned int instruction = (opcode << 26) + (src_reg << 21) + (dst_reg << 16) + (immediate);

            Functions::write_to_file(BIN_File, instruction, 4);
            Functions::write_to_txt_file(LOG_File, std::format("{0:08X} [x] - {1:08X} - {2}\n", MIPS::Current_PC, instruction, current_line));
            MIPS::Current_PC += 4;
            continue;
        }
        // Instruction Type LOADSTORE
        if (Functions::list_contains(MIPS::LOADSTORE, parsed_args[0]) == true)
        {
            // arg control
            if (parsed_args.size() != 4)
            {
                Functions::print_error("LOADSTORE instruction has wrong amount of Arguments !", current_line);
                continue;
            }
            // LOADSTORE R, Imm(A)
            // LOADSTORE = oooo ooaa aaar rrrr iiii iiii iiii iiii
            unsigned int opcode = MIPS::parse_opcode(parsed_args[0]);
            unsigned int reg_reg = MIPS::parse_register(parsed_args[1]);
            unsigned int immediate = MIPS::parse_numeral_lower(parsed_args[2]);
            unsigned int adr_reg = MIPS::parse_register(parsed_args[3]);
            // build instruction
            unsigned int instruction = (opcode << 26) + (adr_reg << 21) + (reg_reg << 16) + (immediate);

            Functions::write_to_file(BIN_File, instruction, 4);
            Functions::write_to_txt_file(LOG_File, std::format("{0:08X} [x] - {1:08X} - {2}\n", MIPS::Current_PC, instruction, current_line));
            MIPS::Current_PC += 4;
            continue;
        }
        // Instruction Type BRANCH_SS
        if (Functions::list_contains(MIPS::BRANCH_SS, parsed_args[0]) == true)
        {
            // arg control
            if (parsed_args.size() != 4)
            {
                Functions::print_error("BRANCH_SS instruction has wrong amount of Arguments !", current_line);
                continue;
            }
            // BRANCH_SS s, S, i
            // oooo ooss sssS SSSS iiii iiii iiii iiii
            unsigned int opcode = MIPS::parse_opcode(parsed_args[0]);
            unsigned int src1_reg = MIPS::parse_register(parsed_args[1]);
            unsigned int src2_reg = MIPS::parse_register(parsed_args[2]);
            unsigned int immediate = MIPS::parse_label_relative(parsed_args[3]);
            // check target alignment, and shift
            immediate = MIPS::reshape_target(immediate, 16);
            if (immediate == Functions::ERROR_CODE)
            {
                Functions::print_error("Branch Target is misaligned or too big !", current_line);
                continue;
            }

            // build instruction
            unsigned int instruction = (opcode << 26) + (src1_reg << 21) + (src2_reg << 16) + (immediate);

            Functions::write_to_file(BIN_File, instruction, 4);
            Functions::write_to_txt_file(LOG_File, std::format("{0:08X} [x] - {1:08X} - {2}\n", MIPS::Current_PC, instruction, current_line));
            MIPS::Current_PC += 4;
            continue;
        }
        // Instruction Type JUMP
        if (Functions::list_contains(MIPS::JUMP, parsed_args[0]) == true)
        {
            // arg control
            if (parsed_args.size() != 2)
            {
                Functions::print_error("JUMP instruction has wrong amount of Arguments !", current_line);
                continue;
            }
            // JUMP d
            // oooo ooii iiii iiii iiii iiii iiii iiii
            unsigned int opcode = MIPS::parse_opcode(parsed_args[0]);
            unsigned int immediate = MIPS::parse_numeral(parsed_args[1]);
            // for JUMP instructions, we actually cut off the upper 4 bits, because
            // those will be determined by the "local neighborhood" anyways
            immediate = (immediate & 0x08FFFFFF);
            // check target alignment, and shift
            immediate = MIPS::reshape_target(immediate, 26);
            if (immediate == Functions::ERROR_CODE)
            {
                Functions::print_error("Branch Target is misaligned or too big !", current_line);
                continue;
            }

            // build instruction
            unsigned int instruction = (opcode << 26) + (immediate);

            Functions::write_to_file(BIN_File, instruction, 4);
            Functions::write_to_txt_file(LOG_File, std::format("{0:08X} [x] - {1:08X} - {2}\n", MIPS::Current_PC, instruction, current_line));
            MIPS::Current_PC += 4;
            continue;
        }
        // Instruction Type SPECIAL_DSS
        if (Functions::list_contains(MIPS::SPECIAL_DSS, parsed_args[0]) == true)
        {
            // arg control
            if (parsed_args.size() != 4)
            {
                Functions::print_error("SPECIAL_DSS instruction has wrong amount of Arguments !", current_line);
                continue;
            }
            // SPECIAL_DSS d, s, S
            // 0000 00ss sssS SSSS dddd d000 00ff ffff
            unsigned int dst_reg = MIPS::parse_register(parsed_args[1]);
            unsigned int src1_reg = MIPS::parse_register(parsed_args[2]);
            unsigned int src2_reg = MIPS::parse_register(parsed_args[3]);
            unsigned int functioncode = MIPS::parse_functioncode(parsed_args[0]);

            // build instruction
            unsigned int instruction = (src1_reg << 21) + (src2_reg << 16) + (dst_reg << 11) + (functioncode);

            Functions::write_to_file(BIN_File, instruction, 4);
            Functions::write_to_txt_file(LOG_File, std::format("{0:08X} [x] - {1:08X} - {2}\n", MIPS::Current_PC, instruction, current_line));
            MIPS::Current_PC += 4;
            continue;
        }
        // Instruction Type HILO_MOVE
        if (Functions::list_contains(MIPS::HILO_MOVE, parsed_args[0]) == true)
        {
            // arg control
            if (parsed_args.size() != 2)
            {
                Functions::print_error("HILO_MOVE instruction has wrong amount of Arguments !", current_line);
                continue;
            }
            // HILO_MOVE d
            // 0000 0000 0000 0000 dddd d000 00ff ffff
            unsigned int functioncode = MIPS::parse_functioncode(parsed_args[0]);
            unsigned int dst_reg = MIPS::parse_register(parsed_args[1]);

            // build instruction
            unsigned int instruction = (dst_reg << 11) + (functioncode);

            Functions::write_to_file(BIN_File, instruction, 4);
            Functions::write_to_txt_file(LOG_File, std::format("{0:08X} [x] - {1:08X} - {2}\n", MIPS::Current_PC, instruction, current_line));
            MIPS::Current_PC += 4;
            continue;
        }
        // Instruction Type SHIFT
        if (Functions::list_contains(MIPS::SHIFT, parsed_args[0]) == true)
        {
            // arg control
            if (parsed_args.size() != 4)
            {
                Functions::print_error("SHIFT instruction has wrong amount of Arguments !", current_line);
                continue;
            }
            // SHIFT d, s, imm
            // 0000 0000 000s ssss dddd diii iiff ffff
            unsigned int functioncode = MIPS::parse_functioncode(parsed_args[0]);
            unsigned int dst_reg = MIPS::parse_register(parsed_args[1]);
            unsigned int src_reg = MIPS::parse_register(parsed_args[2]);
            unsigned int immediate = MIPS::parse_numeral_lower(parsed_args[3]);
            // the SHIFT immediate has a max of 5 bits. if there are more, we set it to the max
            immediate = min(immediate, 0b11111);

            // build instruction
            unsigned int instruction = (src_reg << 16) + (dst_reg << 11) + (immediate << 6) + (functioncode);

            Functions::write_to_file(BIN_File, instruction, 4);
            Functions::write_to_txt_file(LOG_File, std::format("{0:08X} [x] - {1:08X} - {2}\n", MIPS::Current_PC, instruction, current_line));
            MIPS::Current_PC += 4;
            continue;
        }
        // Instruction Type FP_TRANSFER
        if (Functions::list_contains(MIPS::FP_TRANSFER, parsed_args[0]) == true)
        {
            // arg control
            if (parsed_args.size() != 3)
            {
                Functions::print_error("FP_TRANSFER instruction has wrong amount of Arguments !", current_line);
                continue;
            }
            // FP_TRANSFER r, F
            // 0100 01tt tttr rrrr FFFF F000 0000 0000
            unsigned int transf_type = MIPS::parse_fp_transfer_specifier(parsed_args[0]);
            unsigned int reg_reg = MIPS::parse_register(parsed_args[1]);
            unsigned int flp_reg = MIPS::parse_fp_register(parsed_args[2]);

            // build instruction
            unsigned int instruction = (MIPS::COP1_HEAD << 26) + (transf_type << 21) + (reg_reg << 16) + (flp_reg << 11);

            Functions::write_to_file(BIN_File, instruction, 4);
            Functions::write_to_txt_file(LOG_File, std::format("{0:08X} [x] - {1:08X} - {2}\n", MIPS::Current_PC, instruction, current_line));
            MIPS::Current_PC += 4;
            continue;
        }
        // Instruction Type FP_LOADSTORE
        if (Functions::list_contains(MIPS::FP_LOADSTORE, parsed_args[0]) == true)
        {
            // arg control
            if (parsed_args.size() != 4)
            {
                Functions::print_error("FP_LOADSTORE instruction has wrong amount of Arguments !", current_line);
                continue;
            }
            // FP_LOADSTORE F, Imm(A) - NOTE: same format as LOADSTORE, but fp-regs
            // oooo ooaa aaaF FFFF iiii iiii iiii iiii
            unsigned int opcode = MIPS::parse_opcode(parsed_args[0]);
            unsigned int fpr_reg = MIPS::parse_fp_register(parsed_args[1]);
            unsigned int immediate = MIPS::parse_numeral_lower(parsed_args[2]);
            unsigned int adr_reg = MIPS::parse_register(parsed_args[3]);

            // build instruction
            unsigned int instruction = (opcode << 26) + (adr_reg << 21) + (fpr_reg << 16) + (immediate);

            Functions::write_to_file(BIN_File, instruction, 4);
            Functions::write_to_txt_file(LOG_File, std::format("{0:08X} [x] - {1:08X} - {2}\n", MIPS::Current_PC, instruction, current_line));
            MIPS::Current_PC += 4;
            continue;
        }
        //==========================================================================================
        // from here on out, we expect to parse an instruction that includes a Format-Specifier FMT
        // so first, we need to dissect the first arg a bit more
        //==========================================================================================
        // parse the FMT - (all FMTs are single chars, and always sit at the end)
        unsigned int FMT = MIPS::parse_fmt(std::format("{}", parsed_args[0].back()));

        // strip away the FMT from the instruction if we were successful - (INSTR.M ==> INSTR)
        if (FMT != Functions::ERROR_CODE)
        {
            // INSTR.M  ==>  INSTR
            parsed_args[0] = parsed_args[0].substr(0, (parsed_args[0].size() - 2));
        }
        //==========================================================================================
        // then we can continue with the potentially trimmed parsed_args[0] string
        //==========================================================================================
        // Instruction Type FP_DSS
        if (Functions::list_contains(MIPS::FP_DSS, parsed_args[0]) == true)
        {
            // arg control
            if (parsed_args.size() != 4)
            {
                Functions::print_error("FP_DSS instruction has wrong amount of Arguments !", current_line);
                continue;
            }
            // FP_DSS.M fd, fs, fS (NOTE: s and S are swapped)
            // 0100 01mm mmmS SSSS ssss sDDD DDff ffff
            unsigned int functioncode = MIPS::parse_fp_functioncode(parsed_args[0]);
            unsigned int dst_reg = MIPS::parse_fp_register(parsed_args[1]);
            unsigned int src1_reg = MIPS::parse_fp_register(parsed_args[2]);
            unsigned int src2_reg = MIPS::parse_fp_register(parsed_args[3]);

            // build instruction - (again, NOTE: s and S are swapped !)
            unsigned int instruction = (MIPS::COP1_HEAD << 26) + (FMT << 21) + (src2_reg << 16) + (src1_reg << 11) + (dst_reg << 6) + (functioncode);

            Functions::write_to_file(BIN_File, instruction, 4);
            Functions::write_to_txt_file(LOG_File, std::format("{0:08X} [x] - {1:08X} - {2}\n", MIPS::Current_PC, instruction, current_line));
            MIPS::Current_PC += 4;
            continue;
        }
        // Instruction Type FP_DS
        if (Functions::list_contains(MIPS::FP_DS, parsed_args[0]) == true)
        {
            // arg control
            if (parsed_args.size() != 3)
            {
                Functions::print_error("FP_DS instruction has wrong amount of Arguments !", current_line);
                continue;
            }
            // FP_DS.M fd, fs
            // 0100 01mm mmm0 0000 ssss sDDD DDff ffff
            unsigned int functioncode = MIPS::parse_fp_functioncode(parsed_args[0]);
            unsigned int dst_reg = MIPS::parse_fp_register(parsed_args[1]);
            unsigned int src1_reg = MIPS::parse_fp_register(parsed_args[2]);

            // build instruction
            unsigned int instruction = (MIPS::COP1_HEAD << 26) + (FMT << 21) + (src1_reg << 11) + (dst_reg << 6) + (functioncode);

            Functions::write_to_file(BIN_File, instruction, 4);
            Functions::write_to_txt_file(LOG_File, std::format("{0:08X} [x] - {1:08X} - {2}\n", MIPS::Current_PC, instruction, current_line));
            MIPS::Current_PC += 4;
            continue;
        }
        else
        {
            // if we get to here, we failed this line. Most likely because the instruction wasn't recognized
            Functions::print_error("Unrecognized Instruction !", current_line);

            // write some error-padding regardless, so other stuff is not affected as much
            Functions::write_to_file(BIN_File, 0xFFFFFFFF, 4);
            Functions::write_to_txt_file(LOG_File, std::format("{0:08X} [!] - //////// - {1}\n", MIPS::Current_PC, current_line));
            MIPS::Current_PC += 4;
            continue;
        }
    }
    ////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////
    ////////                   DONE ! - ENTER WRAP-UP                     //////////
    ////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////
    // check if there have been any errors, and restore the backfile file if so
    if (Functions::error_count > 0)
    {
        Functions::print_in_color(std::format("Assembly contains {} errors, restoring Backup File...", Functions::error_count), Functions::COL_WHI_ON_RED);

        BIN_File.close();
        // copy the backup back over the editted binary File to restore it
        std::filesystem::copy(BackUp_filename, BIN_filename, std::filesystem::copy_options::overwrite_existing);

        LOG_File.close();
        std::cin.get();
    }
    else
    {
        Functions::print_in_color(std::format(" << Assembly contained no errors. >> ", Functions::error_count), Functions::COL_WHI_ON_GRNL);

        BIN_File.close();
        LOG_File.close();
        std::cin.get();
    }
    return 0;
}


// 00000A54 [x] - 4FFFFE84 - BEQ R0 R0 OBJ_LOOP wrong, Caje = 10 00 FE 84