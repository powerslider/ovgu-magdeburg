#include <iostream>

void func1(int param) {}

int main()
{
    typedef int XTYPE;

    int y;
    auto x = &y;
    XTYPE* xType = &y;


    int y1[3];
    auto x1 = y1[1];
    XTYPE x1Type = y1[1];


    int y2[3];
    auto x2 = &y2[1];
    XTYPE* x2Type = &y2[1];


    int* y3;
    auto x3 = y3;
    XTYPE* x3Type = y3;


    int* y4;
    auto x4 = &y4;
    XTYPE** x4Type = &y4;


    int* x5;
    int** px5 = &x5;


    int y6 = 0;
    int& x6 = y6;
    int* px6 = &x6;


    int x7[2];
    int* px7 = x7;

    // function pointer
    void (*pfunc1)(int) = func1;

    char* arr = "abcdefg";
    std::cout << arr + 3 << std::endl;


    return 0;
}