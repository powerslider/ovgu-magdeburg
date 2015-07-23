#include "ex8.h"

int main()
{
    try
    {
        Scope scope(99);
        f(2);
    }
    catch (int e)
    {
        cout << "caught " << e << endl;
    }

    /*
      OUTPUT:

        create
        enter f, n = 2
        create
        enter f, n = 1
        create
        enter f, n = 0
        create
        destroy 0
        destroy 1
        destroy 2
        destroy 99
        caught 42
     */

    return 0;
}