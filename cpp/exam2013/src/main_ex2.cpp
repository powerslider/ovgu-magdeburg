#include "ex2.h"


int main()
{
    Animal* tux = new Penguin;
    tux->move();

    //get pointer type for dynamic_cast<Bird*>(tux)
    Bird* bird = dynamic_cast<Bird*>(tux);

    //get pointer type for dynamic_cast<Fish*>(tux)
    Fish* fish = dynamic_cast<Fish*>(tux);

    Fish* nemo = new Fish;
    nemo->move();

    delete tux;
    delete nemo;

    return 0;
}