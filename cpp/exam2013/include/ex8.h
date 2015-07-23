#include <iostream>


using namespace std;

struct Scope
{
    int id;

    Scope(int p) : id(p) { cout << "create " << endl; }

    ~Scope() { cout << "destroy " << id << endl; }
};

void f(int n)
{
    cout << "enter f, n = " << n << endl;
    Scope scope(n);
    if (n == 0)
        throw 42;
    f(n - 1);
    cout << "leave f, n = " << n << endl;
}