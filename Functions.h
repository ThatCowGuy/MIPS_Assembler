
#pragma once

#include <iostream>
#include <map>
#include <format>
#include <list>
#include <string>
#include <vector>
#include <Windows.h>
class Functions
{
    public:

        static unsigned int twos_complement(unsigned int value);

        static bool list_contains(std::list<std::string> container, std::string key);

        static void write_to_file(std::fstream& file, unsigned int content, unsigned int bytecount);
        static void write_to_file(std::fstream& file, std::string content);
        static void write_to_txt_file(std::fstream& file, std::string content);

        static unsigned int try_key_on_dict(std::string key, std::map<std::string, unsigned int> dict);

        static std::string remove_double_spaces(std::string input);
        static std::string trim_whitespace(std::string input);
        static std::string replace_char(std::string input, char unwanted, char replacement);
        static std::string remove_char(std::string input, char unwanted);

        static std::string convert_to_upper(std::string input);
        static std::vector<std::string> split_string_at(std::string input, char delim);

        static unsigned int error_count;
        static const unsigned int ERROR_CODE = 3210123; // I'm banking hard on "Nobody will ever use this Num"

        static HANDLE console_handle;// = GetStdHandle(STD_OUTPUT_HANDLE);
        /*===================
        0   BLACK
        1   BLUE
        2   GREEN
        3   CYAN
        4   RED
        5   MAGENTA
        6   BROWN
        7   LIGHTGRAY
        8   DARKGRAY
        9   LIGHTBLUE
        10  LIGHTGREEN
        11  LIGHTCYAN
        12  LIGHTRED
        13  LIGHTMAGENTA
        14  YELLOW
        15  WHITE
        ===================*/
        static const unsigned int COL_GRN = 0x02; // labels
        static const unsigned int COL_CYN = 0x03; // constants

        static const unsigned int COL_MAG = 0x5; // RAM offset
        static const unsigned int COL_MAGL = 0x0D; // file offset

        static const unsigned int COL_RED = 0x0C; // errors
        static const unsigned int COL_WHI = 0x0F;
        static const unsigned int COL_WHI_ON_RED = 0xCF; // error notifier
        static const unsigned int COL_WHI_ON_GRNL = 0xAF; // success

        static void print(std::string msg);
        static void print_in_color(std::string msg, unsigned int color);
        static void print_error(std::string msg, std::string input);
};

