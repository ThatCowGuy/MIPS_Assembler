#include "Functions.h"
#include <Windows.h>

#include <iostream>
#include <map>
#include <format>
#include <string>
#include <Windows.h>
#include <vector>
#include <list>
#include <fstream>

#include "MIPS.h"

unsigned int Functions::twos_complement(unsigned int value)
{
    // abusing casts
    return (unsigned int) ((-1) * (int) value);
}

bool Functions::list_contains(std::list<std::string> container, std::string key)
{
    // C++ be like...
    return (std::find(std::begin(container), std::end(container), key) != std::end(container));
}

void Functions::write_to_file(std::fstream& file, unsigned int content, unsigned int bytecount)
{
    // convert the unsigned int into an array of bytes == chars
    char bytes[4];
    bytes[0] = (content >> 24) & 0xFF;
    bytes[1] = (content >> 16) & 0xFF;
    bytes[2] = (content >>  8) & 0xFF;
    bytes[3] = (content >>  0) & 0xFF;
    // and write them into the file, starting with an offset
    file.write(&bytes[4 - bytecount], bytecount);
}
void Functions::write_to_file(std::fstream& file, std::string content)
{
    // write the content into the file
    file.write(content.c_str(), (content.size() + 1));
}
void Functions::write_to_txt_file(std::fstream& file, std::string content)
{
    // write the content into the file (no null termination)
    file.write(content.c_str(), content.size());
}

unsigned int Functions::try_key_on_dict(std::string key, std::map<std::string, unsigned int> dict)
{
    // try finding the key within the dict
    std::map<std::string, unsigned int>::iterator index = dict.find(key);
    // if the iterator doesn't pounsigned int to the end => success
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
std::string Functions::replace_char(std::string input, char unwanted, char replacement)
{
    for (char& c : input)
    {
        if (c == unwanted)
            c = replacement;
    }
    return input;
}
std::string Functions::remove_char(std::string input, char unwanted)
{
    // this is just unreadable...
    input.erase(std::remove(input.begin(), input.end(), unwanted), input.end());
    return input;
}

std::string Functions::convert_to_upper(std::string input)
{
    // convert the input to upper, and return that
    for (char& c : input)
        c = std::toupper(c);
    return input;
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
unsigned int Functions::error_count = 0;

// and defining some print shortcuts
void Functions::print(std::string msg)
{
    std::cout << msg + "\n";
}
void Functions::print_in_color(std::string msg, unsigned int color)
{
    SetConsoleTextAttribute(console_handle, color);
    std::cout << msg;
    SetConsoleTextAttribute(console_handle, COL_WHI);
}
void Functions::print_error(std::string msg, std::string input)
{
    Functions::error_count++;
    print_in_color(std::format("[+ ERROR #{:02} +]", Functions::error_count), COL_WHI_ON_RED);
    print_in_color("\n >> " + msg + "\n", COL_RED);
    print_in_color(std::format("Input: \"{0}\"\n", input), COL_RED);
}