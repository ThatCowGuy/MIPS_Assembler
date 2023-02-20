#include "Functions.h"
#include <Windows.h>

#include <iostream>
#include <map>
#include <format>
#include <string>
#include <Windows.h>
#include <vector>
#include <list>

int Functions::try_key_on_dict(std::string key, std::map<std::string, int> dict)
{
    // try finding the key within the dict
    std::map<std::string, int>::iterator index = dict.find(key);
    // if the iterator doesn't point to the end => success
    if (index != dict.end())
        return index->second;
    // else, return failure
    return Functions::ERROR_CODE;
}

std::string Functions::remove_double_spaces(std::string input)
{
    // split the input at every space
    std::vector<std::string> split = Functions::split_string_at(input, ' ');

    // make a list of all split elements that are not empty
    std::list<std::string> non_empty_elements;
    for (unsigned int i = 0; i < split.size(); i++)
    {
        if (split[i].size() > 0)
            non_empty_elements.push_back(split[i]);
    }
    // create a concatenation of the non-empty elements
    std::string concat = "";
    size_t split_count = non_empty_elements.size();
    for (unsigned int i = 0; i < split_count; i++)
    {
        if (i > 0) concat.append(" ");
        concat.append(non_empty_elements.front());
        non_empty_elements.pop_front();
    }
    // and return
    return concat;
}
std::string Functions::trim_whitespace(std::string input)
{
    size_t first_non_whitespace = input.find_first_not_of(" \t");
    if (first_non_whitespace == std::string::npos)
    {
        return "";
    }
    size_t last_non_whitespace = input.find_last_not_of(" \t");
    if (last_non_whitespace == std::string::npos)
    {
        return "";
    }
    size_t length = (last_non_whitespace - first_non_whitespace);
    return input.substr(first_non_whitespace, (length + 1));
}

std::string Functions::convert_to_upper(std::string input)
{
    // make a copy of the input (C++ has actual call by ref)
    std::string copy;
    copy.resize(input.size());
    input.copy(&copy[0], input.size());
    // convert the copy to upper, and return that
    for (char& c : copy)
        c = std::toupper(c);
    return copy;
}
std::vector<std::string> Functions::split_string_at(std::string input, char delim)
{
    // init a vector to store the split string
    std::vector<std::string> split;
    size_t start = 0;
    size_t index = input.find(delim, start);
    // if index hasnt reached the end, we add a substring 
    while (index != std::string::npos)
    {
        split.push_back(input.substr(start, (index - start)));
        start = (index + 1);
        index = input.find(delim, start);
    }
    // add the last substring (which may be the whole thing)
    split.push_back(input.substr(start, std::string::npos));
    // done
    return split;
}

// defining non-constant static members
HANDLE Functions::console_handle = GetStdHandle(STD_OUTPUT_HANDLE);
int Functions::error_count = 0;

// and defining some print shortcuts
void Functions::print(std::string msg)
{
    std::cout << msg + "\n";
}
void Functions::print_in_color(std::string msg, int color)
{
    SetConsoleTextAttribute(console_handle, color);
    std::cout << msg + "\n";
    SetConsoleTextAttribute(console_handle, COL_WHI);
}
void Functions::print_error(std::string msg, std::string input)
{
    Functions::error_count++;
    print_in_color(std::format("[+ ERROR #{:02} +]", Functions::error_count), COL_WHI_ON_RED);
    print_in_color(" >> " + msg, COL_RED);
    print_in_color(std::format("Input: \"{0}\"", input), COL_RED);
}