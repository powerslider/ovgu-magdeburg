#include "random_char.h"

int const RandomChar::m_length = 71;
char const RandomChar::m_allPossibleChars[] = "01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!�$%&?@";

RandomChar::RandomChar()
{

}

RandomChar::~RandomChar()
{
    //delete[] m_allPossibleChars;
}

const char RandomChar::getRandomChar()
{
    return m_allPossibleChars[rand() % m_length];
}
