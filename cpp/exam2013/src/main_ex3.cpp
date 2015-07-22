
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
    auto x2 = y2[1];
    XTYPE* x2Type = &y2[1];


    int* y3;
    auto x3 = y3;
    XTYPE* x3Type = y3;


    int* y4;
    auto x4 = &y4;
    XTYPE** x4Type = &y4;


    return 0;
}