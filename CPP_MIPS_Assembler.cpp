#include <iostream>
#include <map>
#include <string>
#include <format>
#include <Windows.h>
#include <fstream>

#include "MIPS.h"
#include "Functions.h"

int main()
{
    MIPS::add_constant("POOP2", 0x20);
    std::cout << MIPS::parse_constant("POOP2") << std::endl;

    std::ifstream ASM_File("testing.asm");

    std::list <std::string> ASM_lines;
    std::string current_line;

    int relative_offset = 0;
    ////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////
    /////////       PASS-THROUGH #01 - FILTERING AND GRABBING SYMBOLS      /////////
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
            while (relative_offset % 4 > 0)
                relative_offset++;
            // add the bytesize of the parsed string +1 for '\0' to PC
            relative_offset += (string_split[1].size() + 1);

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
        current_line.erase(std::remove(current_line.begin(), current_line.end(), ','), current_line.end());
        // remove all dollars ($T0 == T0 etc.)
        current_line.erase(std::remove(current_line.begin(), current_line.end(), '$'), current_line.end());
        // and trim off everything that has been moved to the back
        current_line = Functions::remove_double_spaces(current_line);

        std::cout << current_line << std::endl;


        // LOADSTORE instructions might be written in a way, that makes
        // it hard to sepperate the last arg correctly. So prepare for that
        //filtered_ASM_lines[i] = filtered_ASM_lines[i].Replace("(", " ");
        //filtered_ASM_lines[i] = filtered_ASM_lines[i].Replace(")", String.Empty);


        /*
        //-------------------------------------------------------
        // SPLIT INTO ARGS
        //-------------------------------------------------------
        string[] parsed_args = filtered_ASM_lines[i].Split(" ");
        //-------------------------------------------------------
        // LABEL DEFINITIONS
        //-------------------------------------------------------
        if (parsed_args.Length == 1 && parsed_args[0].Contains(":"))
        {
            parsed_args[0] = parsed_args[0].Replace(":", "");
            Labels.Add(parsed_args[0], relative_offset);
            print_in_color(String.Format("(LABEL) {0} \t=> 0x{1:X8}", parsed_args[0], relative_offset), ConsoleColor.Green);
            // we are done with this line now, discard it
            filtered_ASM_lines[i] = String.Empty;
            continue;
        }
        //-------------------------------------------------------
        // INJECTION-COMMANDS + DATA INSERTS
        //-------------------------------------------------------
        else if (parsed_args.Length == 2 && parsed_args[0].StartsWith("."))
        {
            if (MIPS.RAM_Start_Identifiers.ContainsKey(parsed_args[0]))
            {
                // set the RAM start to the parsed value
                RAM_Start = Convert.ToInt32(parsed_args[1], 16);
                print_in_color(String.Format("(RAM_Start) 0x{0:X8}", RAM_Start), ConsoleColor.DarkMagenta);
                // we are done with this line now, discard it
                filtered_ASM_lines[i] = String.Empty;
                continue;
            }
            if (MIPS.InjectionPoints.ContainsKey(parsed_args[0]))
            {
                // set the relative offset to the injection point
                relative_offset = Convert.ToInt32(parsed_args[1], 16);
                print_in_color(String.Format("(INJECT-P.) 0x{0:X8}", relative_offset), ConsoleColor.Magenta);
                // we CANNOT discard these, because we need them on pass 2 aswell
                continue;
            }
            if (MIPS.DataIdentifiers.ContainsKey(parsed_args[0]))
            {
                // NOTE: on pass #1 we only care about the size of these

                // get the desired datasize
                int data_size = MIPS.DataIdentifiers[parsed_args[0]];
                // alignment control
                while (relative_offset % data_size > 0)
                {
                    // here we would pad on pass #2, but we only increment
                    // the relative offset on pass #1 and pretend we padded
                    relative_offset++;
                }
                // and finally increment the relative_offset
                relative_offset += data_size;
                continue;
            }
        }
        //-------------------------------------------------------
        // CONSTANT DEFINTIONS
        //-------------------------------------------------------
        if (parsed_args.Length == 2 && parsed_args[0].Contains("["))
        {
            // grab the name of the constant
            parsed_args[0] = parsed_args[0].Substring(1, parsed_args[0].IndexOf(']') - 1);
            // and parse the value
            int value = parse_numeral(parsed_args[1]);
            // then put it into the dict
            Constants.Add(parsed_args[0], value);
            print_in_color(String.Format("(CONSTANT) {0} \t=> 0x{1:X8}", parsed_args[0], value), ConsoleColor.Cyan);
            // we are done with this line now, discard it
            filtered_ASM_lines[i] = String.Empty;
            continue;
        }
        // if we got up to here, we are facing a real instruction.
        // we dont care about those on pass #1, so just do the
        // alignment control and increment the relative offset by 4
        while (relative_offset % 4 > 0)
        {
            // here we would pad on pass #2, but we only increment
            // the relative offset on pass #1 and pretend we padded
            relative_offset++;
        }
        relative_offset += 4;
    }
    // now everything is pre-processed, and we can dump empty lines
    // by iterating through the list backwards
    for (int i = (filtered_ASM_lines.Count() - 1); i >= 0; i--)
    {
        // check for emptied lines
        if (filtered_ASM_lines[i].Equals(String.Empty))
            filtered_ASM_lines.RemoveAt(i);
    }
    */

    relative_offset = 0;
    ////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////
    ////////      PASS-THROUGH #02 - ACTUAL ASSEMBLY OF INSTRUCTIONS      //////////
    ////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////

    // and go for the 2nd pass through the ASM file - assembling instructions
    // we can now assume that there is no "garbage" in any of the lines anymore
    // and we can assume that there are no Constant- or Label-Definitions
    /*
    foreach(string line in filtered_ASM_lines)
    {
        //-------------------------------------------------------
        // CATCH + PRESERVE .STRING DATA
        //-------------------------------------------------------
        if (line.ToUpper().Contains(".STRING"))
        {
            // .STRING "Content"
            // will be split into 3 strings, we only care abt #2
            string[] string_split = line.Split("\"");
            if (string_split.Length != 3)
            {
                print_error(".STRING data entry seems to be malformatted !", line);
                continue;
            }
            byte[] string_bytes = Encoding.ASCII.GetBytes(string_split[1]);

            // alignment control
            while (relative_offset % 4 > 0)
            {
                write_to_BIN(BIN_file, 0x00, 1);
                TXT_file.WriteLine(String.Format("{0:X4} [.] - {1:X2}", relative_offset, 0x00));
                relative_offset++;
            }
            for (int i = 0; i < string_bytes.Length; i++)
            {
                write_to_BIN(BIN_file, string_bytes[i], 1);
                TXT_file.WriteLine(String.Format("{0:X4} [ ] - {1:X2}", relative_offset, string_bytes[i]));
                relative_offset++;
            }
            // and finally write a NULL Byte to terminate the String
            write_to_BIN(BIN_file, 0x00, 1);
            TXT_file.WriteLine(String.Format("{0:X4} [ ] - {1:X2}", relative_offset, 0x00));
            relative_offset++;
            continue;
        }
        //-------------------------------------------------------
        // SPLIT INTO ARGS
        //-------------------------------------------------------
        string[] parsed_args = line.Split(" ");
        //-------------------------------------------------------
        // INJECTION-COMMANDS + DATA INSERTS
        //-------------------------------------------------------
        if (parsed_args.Length == 2 && parsed_args[0].StartsWith("."))
        {
            if (MIPS.InjectionPoints.ContainsKey(parsed_args[0]))
            {
                TXT_file.WriteLine("-----------------");
                // set the relative offset to the injection point
                relative_offset = Convert.ToInt32(parsed_args[1], 16);
                // also move the write-pointer within the BIN FileStream
                BIN_file.Seek(relative_offset, System.IO.SeekOrigin.Begin);
                continue;
            }
            if (MIPS.DataIdentifiers.ContainsKey(parsed_args[0]))
            {
                // first, parse the 2nd arg into the value
                int value = parse_numeral(parsed_args[1]);
                // then get the desired datasize
                int data_size = MIPS.DataIdentifiers[parsed_args[0]];
                // and perform a small sanity check
                if (value >= Math.Pow(0x100, data_size))
                {
                    print_error("Given Value is too big for specified DataSize !", line);
                }
                // alignment control
                while (relative_offset % data_size > 0)
                {
                    write_to_BIN(BIN_file, 0x00, 1);
                    TXT_file.WriteLine(String.Format("{0:X4} [.] - {1:X2}", relative_offset, 0x00));
                    relative_offset++;
                }
                // then just write the formatted value into the file
                switch (data_size)
                {
                case (0x4): // WORD
                    write_to_BIN(BIN_file, value, 4);
                    TXT_file.WriteLine(String.Format("{0:X4} [ ] - {1:X8} - {2}", relative_offset, value, line));
                    break;
                case (0x2): // HALF
                    write_to_BIN(BIN_file, value, 2);
                    TXT_file.WriteLine(String.Format("{0:X4} [ ] - {1:X4} - {2}", relative_offset, value, line));
                    break;
                case (0x1): // BYTE
                    write_to_BIN(BIN_file, value, 1);
                    TXT_file.WriteLine(String.Format("{0:X4} [ ] - {1:X2} - {2}", relative_offset, value, line));
                    break;
                }
                // and increment the relative_offset
                relative_offset += data_size;
                continue;
            }
        }
        // alignment control
        while (relative_offset % 0x4 > 0)
        {
            write_to_BIN(BIN_file, 0x00, 1);
            TXT_file.WriteLine(String.Format("{0:X4} [.] - {1:X2}", relative_offset, 0x00));
            relative_offset++;
        }
        //-------------------------------------------------------
        // PSEUDO - INSTRUCTIONS
        //-------------------------------------------------------
        // (PSEUDO) B i ==> BEQ R0, R0, i
        if (parsed_args[0].Equals("B"))
        {
            // arg control
            if (parsed_args.Length != 2)
            {
                print_error("Incorrect amount of arguments provided to parse (PSEUDO) B instruction !", line);
                continue;
            }
            // we can neatly handle this by rewriting the line
            string corrected_line = String.Format("BEQ R0 R0 {0}", parsed_args[1]);
            parsed_args = corrected_line.Split(" ");
        }
        // (PSEUDO) MOVE d, s ==> ADD d, R0, s
        if (parsed_args[0].Equals("MOVE"))
        {
            // arg control
            if (parsed_args.Length != 3)
            {
                print_error("Incorrect amount of arguments provided to parse (PSEUDO) MOVE instruction !", line);
                continue;
            }
            // we can neatly handle this by rewriting the line
            string corrected_line = String.Format("ADD {0} R0 {1}", parsed_args[1], parsed_args[2]);
            parsed_args = corrected_line.Split(" ");
        }
        // (PSEUDO) LA d, i ==> (PSEUDO) LI, d, i
        // --- LUI d, i
        // --- ADDIU d, d, i
        //     0011 1100 000d dddd iiii iiii iiii iiii
        //     0010 01DD DDDd dddd iiii iiii iiii iiii
        if (parsed_args[0].Equals("LI") || parsed_args[0].Equals("LA"))
        {
            // arg control
            if (parsed_args.Length != 3)
            {
                print_error("Incorrect amount of arguments provided to parse (PSEUDO) LI / LA instruction !", line);
                continue;
            }
            // since LI / LA translate to 2 instructions, we cant just rewrite the line...
            // start out by parsing the args as normal
            int opcode_LUI = MIPS.parse_opcode("LUI");
            int opcode_ADDIU = MIPS.parse_opcode("ADDIU");
            int dst_reg = MIPS.parse_register(parsed_args[1]);

            // for the immediate, first parse the entire immediate, then split it
            int immediate = parse_numeral(parsed_args[2]);
            int imm_LO = get_lower_16(immediate);
            int imm_HI = get_upper_16(immediate);

            // 0011 1100 000d dddd iiii iiii iiii iiii
            int instruction_1 = (opcode_LUI << 26) + (dst_reg << 16) + (imm_HI);
            // 0010 01DD DDDd dddd iiii iiii iiii iiii
            int instruction_2 = (opcode_ADDIU << 26) + (dst_reg << 21) + (dst_reg << 16) + (imm_LO);

            // and write BOTH
            write_to_BIN(BIN_file, instruction_1, 4);
            TXT_file.WriteLine(String.Format("{0:X4} [ ] - {1:X8} - {2}", relative_offset, instruction_1, line));
            relative_offset += 4;
            write_to_BIN(BIN_file, instruction_2, 4);
            TXT_file.WriteLine(String.Format("{0:X4} [ ] - {1:X8} - ^ ^ ^", relative_offset, instruction_2));
            relative_offset += 4;
            continue;
        }
        //-------------------------------------------------------
        // INSTRUCTIONS
        //-------------------------------------------------------
        // Instruction NOP
        if (parsed_args[0].Equals("NOP"))
        {
            // NOP
            // 0000 0000 0000 0000 0000 0000 0000 0000
            int instruction = 0x0;
            // and write
            write_to_BIN(BIN_file, instruction, 4);
            TXT_file.WriteLine(String.Format("{0:X4} [ ] - {1:X8} - {2}", relative_offset, instruction, line));
            relative_offset += 4;
            continue;
        }
        // Instruction JR
        if (parsed_args[0].Equals("JR"))
        {
            // JR S
            // 0000 00ss sss0 0000 0000 0000 0000 1000
            int src_reg = MIPS.parse_register(parsed_args[1]);
            int instruction = (src_reg << 21) + (0b1000);
            // System.Console.WriteLine(line + " => " + int_to_binary(instruction, 32));
            // and write
            write_to_BIN(BIN_file, instruction, 4);
            TXT_file.WriteLine(String.Format("{0:X4} [ ] - {1:X8} - {2}", relative_offset, instruction, line));
            relative_offset += 4;
            continue;
        }
        // Instruction LUI
        if (parsed_args[0].Equals("LUI"))
        {
            // LUI D, Imm
            // 0011 1100 000d dddd iiii iiii iiii iiii
            int dst_reg = MIPS.parse_register(parsed_args[1]);
            // NOTE parse_immediate_X() takes care of constants
            int immediate = parse_immediate_upper(parsed_args[2]);
            // System.Console.WriteLine(line + " => " + int_to_hex(immediate, 4));
            // and write
            int instruction = (0b001111 << 26) + (dst_reg << 16) + (immediate);
            write_to_BIN(BIN_file, instruction, 4);
            TXT_file.WriteLine(String.Format("{0:X4} [ ] - {1:X8} - {2}", relative_offset, instruction, line));
            relative_offset += 4;
            continue;
        }
        // Instruction Type SPECIAL_DSI
        if (MIPS.SPECIAL_DSI.Contains(parsed_args[0]))
        {
            // arg control
            if (parsed_args.Length != 4)
            {
                print_error("Incorrect amount of arguments provided to parse SPECIAL_DSI instruction !", line);
                continue;
            }
            // SPECIAL_DSI D, S, Imm
            // oooo ooss sssd dddd iiii iiii iiii iiii
            int opcode = MIPS.parse_opcode(parsed_args[0]);
            int dst_reg = MIPS.parse_register(parsed_args[1]);
            int src_reg = MIPS.parse_register(parsed_args[2]);
            int immediate = parse_immediate_lower(parsed_args[3]);
            // and write
            int instruction = (opcode << 26) + (src_reg << 21) + (dst_reg << 16) + (immediate);
            // System.Console.WriteLine(line + " => " + int_to_binary(instruction, 32));
            write_to_BIN(BIN_file, instruction, 4);
            TXT_file.WriteLine(String.Format("{0:X4} [ ] - {1:X8} - {2}", relative_offset, instruction, line));
            relative_offset += 4;
            continue;
        }
        // Instruction Type LOADSTORE
        if (MIPS.LOADSTORE.Contains(parsed_args[0]))
        {
            // arg control
            // if Length == 3, we attempt to split the last arg
            if (parsed_args.Length == 3)
            {
                string[] arg3_split = parsed_args[2].Split("(");
                if (arg3_split.Length != 2)
                    break;
                parsed_args = new string[]{ parsed_args[0], parsed_args[1], arg3_split[0], arg3_split[1] };
            }
            if (parsed_args.Length != 4)
            {
                print_error("Incorrect amount of arguments provided to parse LOADSTORE instruction !", line);
                continue;
            }
            // LOADSTORE R, Imm(A)
            // LOADSTORE = oooo ooaa aaar rrrr iiii iiii iiii iiii
            int opcode = MIPS.parse_opcode(parsed_args[0]);
            int reg_reg = MIPS.parse_register(parsed_args[1]);
            int immediate = parse_immediate_lower(parsed_args[2]);
            // for the address reg, we get rid of the paranthesis first
            int adr_reg = MIPS.parse_register(parsed_args[3].Replace("(", "").Replace(")", ""));
            // and write
            int instruction = (opcode << 26) + (adr_reg << 21) + (reg_reg << 16) + (immediate);
            // System.Console.WriteLine(line + " => " + int_to_binary(instruction, 32));
            write_to_BIN(BIN_file, instruction, 4);
            TXT_file.WriteLine(String.Format("{0:X4} [ ] - {1:X8} - {2}", relative_offset, instruction, line));
            relative_offset += 4;
            continue;
        }
        // Instruction Type BRANCH_CC
        if (MIPS.BRANCH_CC.Contains(parsed_args[0]))
        {
            // arg control
            if (parsed_args.Length != 4)
            {
                print_error("Incorrect amount of arguments provided to parse SPECIAL_DSI instruction !", line);
                continue;
            }
            // BRANCH_CC c, C, i
            // oooo oocc cccC CCCC iiii iiii iiii iiii
            int opcode = MIPS.parse_opcode(parsed_args[0]);
            int c1_reg = MIPS.parse_register(parsed_args[1]);
            int c2_reg = MIPS.parse_register(parsed_args[2]);

            int immediate = parse_label_relative(parsed_args[3], (relative_offset + 4)); // Branch rel. to BranchDelaySlot=(PC + 4)
            // check if the determined branch target is word-aligned
            if (immediate % 0x4 != 0)
            {
                print_error("Branch Target is not Word-Aligned ! (Has to be multiple of 4)", line);
                continue;
            }
            // and shift the target to get the correct immediate for Branch
            immediate = (immediate / 0x4);
            // also test if the target might be too far away (after shifting, it can only have 16 bits)
            if (Math.Abs(immediate) > 0x10000)
            {
                print_error(String.Format("Branch Target is too far away ! Dist = {0:X8}", immediate), line);
                continue;
            }
            // in any case (positive/negative) we only want the final 16 bits
            immediate = (immediate & 0xFFFF);
            // System.Console.WriteLine(String.Format("{0:X4} {1:X8} - {2}", relative_offset, immediate, line));
            // and write
            int instruction = (opcode << 26) + (c1_reg << 21) + (c2_reg << 16) + (immediate);
            // System.Console.WriteLine(line + " => " + int_to_hex(instruction, 8));
            write_to_BIN(BIN_file, instruction, 4);
            TXT_file.WriteLine(String.Format("{0:X4} [ ] - {1:X8} - {2}", relative_offset, instruction, line));
            relative_offset += 4;
            continue;
        }
        // Instruction Type JUMP
        if (MIPS.JUMP.Contains(parsed_args[0]))
        {
            // arg control
            if (parsed_args.Length != 2)
            {
                print_error("Incorrect amount of arguments provided to parse JUMP instruction !", line);
                continue;
            }
            // JUMP d
            // oooo ooii iiii iiii iiii iiii iiii iiii - NOTE: the Immediate is shifted !
            // because the only valid jump targets are a multiple of 0x4, the Imm here
            // is divided by 0x4 (or RShifted by 2). That makes it a bit trickier...
            int opcode = MIPS.parse_opcode(parsed_args[0]);

            // I also want to allow Literals, Constants AND Labels to be used as a jump target...
            int immediate = 0;
            // Label JAL Target
            if (parsed_args[1].Contains(":"))
            {
                // NOTE: If we are using Labels as JAL Targets, we HAVE to set .RAMStart
                if (RAM_Start == 0)
                {
                    print_error("When using JAL on a Label, you have to set RAM_Start !\n(.RAMorg, .RAM_org, .RAMStart, .RAM_Start)", line);
                    continue;
                }
                immediate = parse_label_absolute(parsed_args[1]); // NOTE: JAL is NOT relative
                immediate += RAM_Start;
            }
            // Constant JAL Target
            else if (parsed_args[1].Contains("@"))
            {
                immediate = parse_constant(parsed_args[1]);
            }
            // Literal JAL Target
            else
            {
                immediate = parse_numeral(parsed_args[1]);
            }

            // for JAL, we chop off the first 4 bits, because those are actually
            // determined by the PC of the Branch-Delay-Slot internally anyways
            immediate = ((immediate << 4) >> 4);
            // then we check if the determined jump target is word-aligned
            if (immediate % 0x4 != 0)
            {
                print_error("Jump Target is not Word-Aligned ! (Has to be multiple of 4)", line);
                continue;
            }
            // and shift the target to get the correct immediate for JAL
            immediate = (immediate / 0x4);
            // also test if the target might be too far away (after shifting, it can only have 26 bits)
            if (Math.Abs(immediate) > 0x9000000)
            {
                print_error(String.Format("Jump Target is too far away ! Dist = {0:X8}", immediate), line);
                continue;
            }
            // in any case (positive/negative) we only want the final 26 bits
            immediate = (immediate & 0x8FFFFFF);
            // and write
            int instruction = (opcode << 26) + (immediate);
            // System.Console.WriteLine(line + " => " + int_to_binary(instruction, 32));
            write_to_BIN(BIN_file, instruction, 4);
            TXT_file.WriteLine(String.Format("{0:X4} [ ] - {1:X8} - {2}", relative_offset, instruction, line));
            relative_offset += 4;
            continue;
        }
        // Instruction Type SPECIAL_DSS
        if (MIPS.SPECIAL_DSS.Contains(parsed_args[0]))
        {
            // arg control
            if (parsed_args.Length != 4)
            {
                print_error("Incorrect amount of arguments provided to parse SPECIAL_DSS instruction !", line);
                continue;
            }
            // SPECIAL_DSS d, s, S
            // 0000 00ss sssS SSSS dddd d000 00ff ffff
            int dst_reg = MIPS.parse_register(parsed_args[1]);
            int s1_reg = MIPS.parse_register(parsed_args[2]);
            int s2_reg = MIPS.parse_register(parsed_args[3]);
            int functioncode = MIPS.parse_functioncode(parsed_args[0]);
            // and write
            int instruction = (s1_reg << 21) + (s2_reg << 16) + (dst_reg << 11) + (functioncode);
            // System.Console.WriteLine(line + " => " + int_to_hex(instruction, 8));
            write_to_BIN(BIN_file, instruction, 4);
            TXT_file.WriteLine(String.Format("{0:X4} [ ] - {1:X8} - {2}", relative_offset, instruction, line));
            relative_offset += 4;
            continue;
        }
        // Instruction Type HILO_MOVE
        if (MIPS.HILO_MOVE.Contains(parsed_args[0]))
        {
            // arg control
            if (parsed_args.Length != 2)
            {
                print_error("Incorrect amount of arguments provided to parse HILO_MOVE instruction !", line);
                continue;
            }
            // HILO_MOVE d
            // 0000 0000 0000 0000 dddd d000 00ff ffff
            int dst_reg = MIPS.parse_register(parsed_args[1]);
            int functioncode = MIPS.parse_functioncode(parsed_args[0]);
            // and write
            int instruction = (dst_reg << 11) + (functioncode);
            // System.Console.WriteLine(line + " => " + int_to_hex(instruction, 8));
            write_to_BIN(BIN_file, instruction, 4);
            TXT_file.WriteLine(String.Format("{0:X4} [ ] - {1:X8} - {2}", relative_offset, instruction, line));
            relative_offset += 4;
            continue;
        }
        // Instruction Type SHIFT
        if (MIPS.SHIFT.Contains(parsed_args[0]))
        {
            // arg control
            if (parsed_args.Length != 2)
            {
                print_error("Incorrect amount of arguments provided to parse SHIFT instruction !", line);
                continue;
            }
            // SHIFT D, S, I
            // 0000 0000 000s ssss dddd diii iiff ffff
            int dst_reg = MIPS.parse_register(parsed_args[1]);
            int src_reg = MIPS.parse_register(parsed_args[2]);
            int immediate = parse_immediate_lower(parsed_args[3]); // NOTE: this Imm can only be 5-bits (max 31)
            int functioncode = MIPS.parse_functioncode(parsed_args[0]);
            // check if the immediate is okay
            if (immediate >= 0b100000)
            {
                print_error("SHIFT Immediate can only have 5 bits !", line);
                continue;
            }
            // and write
            int instruction = (src_reg << 16) + (dst_reg << 11) + (immediate << 6) + (functioncode);
            // System.Console.WriteLine(line + " => " + int_to_hex(instruction, 8));
            write_to_BIN(BIN_file, instruction, 4);
            TXT_file.WriteLine(String.Format("{0:X4} [ ] - {1:X8} - {2}", relative_offset, instruction, line));
            relative_offset += 4;
            continue;
        }
        // Instruction Type COP1_INTERCHANGE
        if (MIPS.COP1_INTERCHANGE.Contains(parsed_args[0]))
        {
            // arg control
            if (parsed_args.Length != 3)
            {
                print_error("Incorrect amount of arguments provided to parse COP1_INTERCHANGE instruction !", line);
                continue;
            }
            // COP1_INTERCHANGE r, F
            // 0100 01oo ooor rrrr FFFF F000 0000 0000
            int opcode = MIPS.parse_cop1_opcode(parsed_args[0]);
            int reg_reg = MIPS.parse_register(parsed_args[1]);
            int flp_reg = MIPS.parse_fp_register(parsed_args[2]);
            // and write
            int instruction = (0b010001 << 26) + (opcode << 21) + (reg_reg << 16) + (flp_reg << 11);
            // System.Console.WriteLine(line + " => " + int_to_hex(instruction, 8));
            write_to_BIN(BIN_file, instruction, 4);
            TXT_file.WriteLine(String.Format("{0:X4} [ ] - {1:X8} - {2}", relative_offset, instruction, line));
            relative_offset += 4;
            continue;
        }
        // Instruction Type COP1_LOADSTORE
        if (MIPS.COP1_LOADSTORE.Contains(parsed_args[0]))
        {
            // arg control
            // if Length == 3, we attempt to split the last arg
            if (parsed_args.Length == 3)
            {
                string[] arg3_split = parsed_args[2].Split("(");
                if (arg3_split.Length != 2)
                    break;
                parsed_args = new string[]{ parsed_args[0], parsed_args[1], arg3_split[0], arg3_split[1] };
            }
            if (parsed_args.Length != 4)
            {
                print_error("Incorrect amount of arguments provided to parse COP1_LOADSTORE instruction !", line);
                continue;
            }
            // COP1_LOADSTORE F, Imm(A) - NOTE: same format as LOADSTORE, but fp-regs
            // oooo ooaa aaaF FFFF iiii iiii iiii iiii
            int opcode = MIPS.parse_opcode(parsed_args[0]);
            int fpr_reg = MIPS.parse_fp_register(parsed_args[1]);
            int immediate = parse_immediate_lower(parsed_args[2]);
            // for the address reg, we get rid of the paranthesis first
            int adr_reg = MIPS.parse_register(parsed_args[3].Replace("(", "").Replace(")", ""));
            // and write
            int instruction = (opcode << 26) + (adr_reg << 21) + (fpr_reg << 16) + (immediate);
            // System.Console.WriteLine(line + " => " + int_to_binary(instruction, 32));
            write_to_BIN(BIN_file, instruction, 4);
            TXT_file.WriteLine(String.Format("{0:X4} [ ] - {1:X8} - {2}", relative_offset, instruction, line));
            relative_offset += 4;
            continue;
        }
        //==========================================================================================
        // from here on out, we expect to parse an instruction that includes a Format-Specifier FMT
        // so first, we need to dissect the first arg a bit more
        //==========================================================================================
        // strip away the FMT
        int FMT_index = parsed_args[0].LastIndexOf(".");
        int FMT = MIPS.parse_format_specifier(parsed_args[0].Substring(FMT_index + 1));
        parsed_args[0] = parsed_args[0].Substring(0, FMT_index);
        // we can also already store the opcode, because that's always 010001 for COP1 instructions
        int cop1_opcode = 0b010001;
        //==========================================================================================
        // then we can continue with the trimmed parsed_args[0] string
        //==========================================================================================
        // Instruction Type COP1_DSS
        if (MIPS.COP1_DSS.Contains(parsed_args[0]))
        {
            // arg control
            if (parsed_args.Length != 4)
            {
                print_error("Incorrect amount of arguments provided to parse COP1_DSS instruction !", line);
                continue;
            }
            // COP1_DSS.M fd, fs, fS (NOTE: s and S are swapped)
            // 0100 01mm mmmS SSSS ssss sDDD DDff ffff
            int functioncode = MIPS.parse_cop1_functioncode(parsed_args[0]);
            int dst_reg = MIPS.parse_fp_register(parsed_args[1]);
            int src1_reg = MIPS.parse_fp_register(parsed_args[2]);
            int src2_reg = MIPS.parse_fp_register(parsed_args[3]);

            // and write (again, NOTE: s and S are swapped !)
            int instruction = (cop1_opcode << 26) + (FMT << 21) + (src2_reg << 16) + (src1_reg << 11) + (dst_reg << 6) + (functioncode);
            // System.Console.WriteLine(line + " => " + int_to_binary(instruction, 32));
            write_to_BIN(BIN_file, instruction, 4);
            TXT_file.WriteLine(String.Format("{0:X4} [ ] - {1:X8} - {2}", relative_offset, instruction, line));
            relative_offset += 4;
            continue;
        }
        // Instruction Type COP1_DS
        if (MIPS.COP1_DS.Contains(parsed_args[0]))
        {
            // arg control
            if (parsed_args.Length != 3)
            {
                print_error("Incorrect amount of arguments provided to parse COP1_DS instruction !", line);
                continue;
            }
            // COP1_DSS.M fd, fs
            // 0100 01mm mmm0 0000 ssss sDDD DDff ffff
            int functioncode = MIPS.parse_cop1_functioncode(parsed_args[0]);
            int dst_reg = MIPS.parse_fp_register(parsed_args[1]);
            int src1_reg = MIPS.parse_fp_register(parsed_args[2]);
            // and write
            int instruction = (cop1_opcode << 26) + (FMT << 21) + (src1_reg << 11) + (dst_reg << 6) + (functioncode);
            // System.Console.WriteLine(line + " => " + int_to_binary(instruction, 32));
            write_to_BIN(BIN_file, instruction, 4);
            TXT_file.WriteLine(String.Format("{0:X4} [ ] - {1:X8} - {2}", relative_offset, instruction, line));
            relative_offset += 4;
            continue;
        }
        else
        {
            // if we get here, we failed this line
            print_error("Unable to parse Instruction !", line);
            // we are still writing something and incrementing the relative offset
            write_to_BIN(BIN_file, (-1), 4);
            TXT_file.WriteLine(String.Format("{0:X4} [{1}] - {2}", relative_offset, parsed_args.Length, line));
            relative_offset += 0x4;
        }
    }
    */


        
    }
    ASM_File.close();

    std::cin.get();
}
