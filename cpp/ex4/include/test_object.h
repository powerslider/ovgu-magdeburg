#pragma once

// This also has a few bugs and things missing.
// TODO: Identify and fix errors / bugs / mistakes

// TODO: Identify functions and return values that should be const
// also see instructions in programm.cpp, if you haven't already

#include <iostream>
#include <string>
#include <cstdlib>

/// contains an array of strings of random chars
class TestObject
{
public:
    TestObject();
    TestObject(int numberOfStrings);
    ~TestObject();

    // TODO: swap elements using references.
    void swapStrings(std::string& first, std::string& second);

    void printAllStrings();

    /// return the number of strings / elements
    int& elementCount();

    // BONUS: Reverse the array. // Do you use pointers or references? what is the difference here?
    void reverseArray();

    //TODO return reference to string at index. (read only)
    std::string& readAccess(int index);

private:
    void generateStrings();
    std::string* m_arrayOfStrings;
    int m_numberOfStrings;
};