/**
 * @file    main.cpp
 * @author  Tsvetan Dimitrov <tsvetan.dimitrov23@gmail.com>
 * @date    2015-04-15
 *
 * @brief   Main execution of the Timer class
 * @details Get elapsed time between constuctor and destructor calls of the Notifier object
 */
#include "arraylist.h"
#include <iostream>

using namespace std;

/**
 * A stub class used to signal constuctor and destructor calls.
 */
class Notifier
{
public:
    Notifier()
    {
        std::cout << "Ctor called!" << std::endl;
    }

    ~Notifier()
    {
        std::cout << "Dtor called!" << std::endl;
    }
};

int main(int argc, char *argv[])
{
    //Initialize custom timer object
    ArrayList timer;

    {
        //Start the timer
        cout << "ArrayList started!" << endl;
        timer.start();

        //Initialize test object
        Notifier notifier;
    }

    //stop timer
    timer.stop();
    cout << "ArrayList stopped" << endl;

    //Observe results in millis, micros and nanos
    cout << "Elapsed time between ctor and dtor calls is " << endl;
    printf("%6.f ms\n", timer.getDuration().toMillis());
    printf("%6.f μs\n",timer.getDuration().toMicros());
    printf("%6.f ns\n",timer.getDuration().toNanos());

    return 0;
}



