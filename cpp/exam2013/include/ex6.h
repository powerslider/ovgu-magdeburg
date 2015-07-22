#pragma once

#include <iostream>
#include <memory>
#include <string>

using namespace std;

struct Cookie
{
    string type;

    Cookie() : type("Dough")
    {
        cout << "Dough prepared" << endl;
    }

    Cookie(const string& type) : type(type)
    {
        cout << type << " baked" << endl;
    }

    ~Cookie()
    {
        cout << type << " vanished" << endl;
    }
};

unique_ptr<Cookie[]> mix(const unique_ptr<Cookie>& box1, int num1,
                         const unique_ptr<Cookie>& box2, int num2)
{
    unique_ptr<Cookie[]> kmix(new Cookie[num1 + num2]);
    for (int i = 0; i < num1; ++i)
    {
        kmix[i] = box1.get()[i];
    }

    for (int i = 0; i < num2; ++i)
    {
        kmix[i + num1] = box2.get()[i];
    }

    return kmix;
}

bool nobodyEatsCookies(const unique_ptr<Cookie[]>& bowl)
{
    return false;
}

bool meeting()
{
    unique_ptr<Cookie> box1(new Cookie("Double Chocolate"));
    unique_ptr<Cookie> box2(new Cookie("Acacookie"));
    unique_ptr<Cookie[]> bowl = mix(box1, 1, box2, 1);

    if(nobodyEatsCookies(bowl))
        return false;

    cout << bowl[1].type << " is the best one. "
         << box1->type << " is as good as well." << endl;

    return true;
}
