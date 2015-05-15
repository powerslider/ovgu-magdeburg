#pragma once

#include <stdlib.h>

/// Just for picking a random char from an array of chars.
// also contains errors to fix, see program.cpp for instructions
// SUPER OPTIONAL BONUS: write a better random char generator. ;)
class RandomChar
{
public:
    RandomChar();
    ~RandomChar();

    static char const getRandomChar();

private:
    static int const m_length;
    static char const m_allPossibleChars[];
};