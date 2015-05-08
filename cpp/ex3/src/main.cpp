/**
 * @file    main.cpp
 * @author  Tsvetan Dimitrov <tsvetan.dimitrov23@gmail.com>
 * @date    2015-05-03
 *
 * @brief   Main execution of the ArrayList class
 * @details Test list operations
 */
#include "arraylist.h"
#include "arraylist.cpp"


void show(ArrayList<int> *list);

void addBack(ArrayList<int> *list);

using namespace std;


void addBack(ArrayList<int> *list)
{
    for (int i = 0; i < 10; ++i)
    {
        list->addBack(i);
        printf("Added %d at the end\n", i);
    }
}

void addFront(ArrayList<int> *list)
{
    for (int i = 0; i < 10; ++i)
    {
        list->addFront(i);
        printf("Added %d at the beginning\n", i);
    }
}

void show(ArrayList<int> *list)
{
    for (unsigned int j = 0; j < 10; ++j)
    {
        printf("Got %d at index %d\n", *(list->getAt(j)), j);
    }
}

void destroy(ArrayList<int> *list, unsigned int position)
{
    list->destroy(position);
    printf("Deleted element at position %d with value %d\n",
           position, *(list->getAt(position)));
}

int main(int argc, char *argv[])
{
    //Initialize list object
    ArrayList<int> *list = new ArrayList<int>(10);

    cout << "*****************************" << endl;
    addBack(list);
    cout << "*****************************" << endl;
    show(list);
    cout << "*****************************" << endl;
    destroy(list, 2);
    cout << "*****************************" << endl;
    show(list);
    cout << "*****************************" << endl;
    destroy(list, 4);
    cout << "*****************************" << endl;
    show(list);

    return 0;
}



