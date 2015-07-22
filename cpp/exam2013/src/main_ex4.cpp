
int main ()
{
    /*
     Pointer Aritmetics

     - sizeof(int) = 4
     - in every row of the table an integer can be placed

            0   1   2   3
     0x3c |   |     |   |   |
     0x38 |   |     |   |   |
     0x34 |   |     |   |   |
     0x30 |   |     |   |   |
     0x2c |   |     |   |   |
     0x28 |   |     |   |   |
     0x24 |   |     |   |   |
     0x20 | 4 | 6/7 | 8 |   |     7 overwrites 6
     0x1c |   |     |   |   |
     0x18 |   |     |   |   |
     0x14 | 3 | 2   |   |   |
     0x10 | 5 | 1   |   |   |
     0x0c |   |     |   |   |
     0x08 |   |     |   |   |
     0x04 |   |     |   |   |
     0x00 |   |     |   |   |


     */

    void* p = (void*) 0x11;

    char* q = (char*) p;
    *q = 1;                   //0x11
    *(q + 4) = 2;             //0x15
    ++q;                      //0x12
    q[2] = 3;                 //0x14

    int* r = (int*) &q[6];    //0x18
    q = (char*) (r + 2);      //0x20
    *q = 4;                   //0x20
    *((char*) &r[-2]) = 5;

    *++q = 6;                 //0x21
    *q++ = 7;                 //0x21
    q[0] = 8;                 //0x22

    return 0;
}