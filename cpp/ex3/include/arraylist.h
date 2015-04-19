/**
 * @file timer.h
 * @author Tsvetan Dimitrov <tsvetan.dimitrov23@gmail.com>
 * @date 2015-04-15
 *
 * @class Timer
 * @brief Header defining class Timer
 * @details Defines timer operations such as start, stop, pause...
 *          Uses c++11 chrono library for the timestamps.
 */
#ifndef ARRAY_LIST_H
#define ARRAY_LIST_H

#include <chrono>

using namespace std;

template <class T>
class ArrayList
{
private:
    T* arr;
    int size;
    int capacity;


public:
    ArrayList();
    ~ArrayList();
};

#endif
