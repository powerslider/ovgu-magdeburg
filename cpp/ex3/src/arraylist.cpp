/**
 * @file    timer.cpp
 * @author  Tsvetan Dimitrov <tsvetan.dimitrov23@gmail.com>
 * @date    2015-04-15
 *
 * @class   Timer
 * @brief   Implementation of Timer class methods.
 * @details Implementation of Timer class operations such as start, stop, pause, resume...
 *          Uses c++11 chrono library for the timestamps.
 */
#include "arraylist.h"
#include <iostream>

using namespace std;

ArrayList::ArrayList()
{
   started = false;
   paused = false;
}

void ArrayList::start()
{
    if (started) return;

    started = true;
    paused = false;
    startTime = Time::now();
}

void ArrayList::stop()
{
    started = false;
}

void ArrayList::pause()
{
    if (paused || !started)
    {
        cout << "ArrayList is not running!" << endl;
        return;
    }

    paused = true;
    endTime = Time::now();
}

void ArrayList::resume()
{
    if (!paused)
    {
        cout << "ArrayList is already running!" << endl;
        return;
    }

    paused = false;
    startTime += (Time::now() - endTime);
}

ArrayList &ArrayList::getDuration()
{
    //if (!started) return *this;

    if (paused)
    {
        duration = endTime - startTime;
    }

    duration = Time::now() - startTime;

    return *this;
}

float ArrayList::toMicros()
{
    return chrono::duration_cast<chrono::microseconds>(duration).count();
}

float ArrayList::toMillis()
{
    return chrono::duration_cast<chrono::milliseconds>(duration).count();
}

float ArrayList::toNanos()
{
    return chrono::duration_cast<chrono::nanoseconds>(duration).count();
}

