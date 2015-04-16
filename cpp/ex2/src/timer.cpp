/**
 * @file    timer.cpp
 * @author  Tsvetan Dimitrov <tsvetan.dimitrov23@gmail.com>
 * @date    2015-04-15
 *
 * @class   Timer
 * @brief   Implementation of Timer class methods.
 * @details Implementation of Timer class operations such as start, stop, pause...
 *          Uses c++11 chrono library for the timestamps.
 */
#include "timer.h"
#include <iostream>

using namespace std;

Timer::Timer()
{
   started = false;
   paused = false;
}

void Timer::start()
{
    if (started) return;

    started = true;
    paused = false;
    startTime = Time::now();
}

void Timer::stop()
{
    started = false;
}

void Timer::pause()
{
    if (paused || !started)
    {
        cout << "Timer is not running!" << endl;
        return;
    }

    paused = true;
    endTime = Time::now();
}

void Timer::resume()
{
    if (!paused)
    {
        cout << "Timer is already running!" << endl;
        return;
    }

    paused = false;
    startTime += (Time::now() - endTime);
}

Timer& Timer::getDuration()
{
    //if (!started) return *this;

    if (paused)
    {
        duration = endTime - startTime;
    }

    duration = Time::now() - startTime;

    return *this;
}

float Timer::toMicros()
{
    return chrono::duration_cast<chrono::microseconds>(duration).count();
}

float Timer::toMillis()
{
    return chrono::duration_cast<chrono::milliseconds>(duration).count();
}

float Timer::toNanos()
{
    return chrono::duration_cast<chrono::nanoseconds>(duration).count();
}

