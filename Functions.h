
#pragma once

#include <iostream>
#include <map>
#include <format>
#include <string>
#include <vector>
#include <Windows.h>
class Functions
{
    public:

        static int try_key_on_dict(std::string key, std::map<std::string, int> dict);

        static std::string remove_double_spaces(std::string input);
        static std::string trim_whitespace(std::string input);

        static std::string convert_to_upper(std::string input);
        static std::vector<std::string> split_string_at(std::string input, char delim);

        static int error_count;
        static const int ERROR_CODE = 3210123; // I'm banking hard on "Nobody will ever use this Num"

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
        static const int COL_RED = 0x0C;
        static const int COL_WHI = 0x0F;
        static const int COL_WHI_ON_RED = 0xCF;

        static void print(std::string msg);
        static void print_in_color(std::string msg, int color);
        static void print_error(std::string msg, std::string input);
};

