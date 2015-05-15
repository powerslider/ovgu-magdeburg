#include "random_char.h"
#include "test_object.h"

TestObject::TestObject()
        : m_numberOfStrings(10)
{
    generateStrings();
}


TestObject::TestObject(int numberOfStrings)
        : m_numberOfStrings(numberOfStrings)
{
    generateStrings();
}


TestObject::~TestObject()
{
    delete m_arrayOfStrings;
}

void TestObject::generateStrings()
{
    m_arrayOfStrings = new std::string[m_numberOfStrings];

    for (int i = 0; i < m_numberOfStrings; i++)
    {
        std::string& currentString = m_arrayOfStrings[i];
        
        int numberOfChars = rand() % 30 + 1;
        currentString.resize((unsigned long) numberOfChars);
        int count = 0;
        for (auto& c : currentString)
        {
            c = RandomChar::getRandomChar();
            currentString[count] = c;
            if(++count != numberOfChars)
            {
                break;
            }
        }
     }
}

void TestObject::swapStrings(std::string& first, std::string& second)
{
    //Assignment copies the address of first
    std::string& temp = first;
    first = second;
    second = temp;
}


void TestObject::printAllStrings()
{
    for (int i = 0; i < m_numberOfStrings; i++)
    {
        std::cout << m_arrayOfStrings[i];
    }
    std::cout << std::endl;
}


// return the number of elements
int& TestObject::elementCount()
{
    return m_numberOfStrings;
}

void TestObject::reverseArray()
{
    for (int i = 0; i < m_numberOfStrings / 2; i++)
    {
        swapStrings(m_arrayOfStrings[m_numberOfStrings - i - 1], m_arrayOfStrings[i]);
    }
}

std::string& TestObject::readAccess(int index)
{
    return m_arrayOfStrings[index];
}
