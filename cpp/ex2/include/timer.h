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
#ifndef TIMER_H
#define TIMER_H

#include <chrono>

using namespace std;

class Timer
{
private:
    typedef std::chrono::high_resolution_clock Time;
    typedef std::chrono::duration<float> Duration;
    typedef std::chrono::time_point<Time> TimePoint;
    TimePoint startTime;
    TimePoint endTime;
    bool started;
    bool paused;
    Duration duration;


public:
    Timer();

public:
    //Getters
    bool isStarted()
    {
        return started;
    }

    bool isPaused()
    {
        return paused;
    }

    bool isStopped()
    {
        return !started;
    }

    bool isRunning()
    {
        return !paused && started;
    }

    TimePoint getStartTime()
    {
        return startTime;
    }

    TimePoint getEndTime()
    {
        return endTime;
    }

    //Setters
    void setStarted(bool started)
    {
        this->started = started;
    }

    void setPaused(bool paused)
    {
        this->paused = paused;
    }
   
    //Timer actions
    void start();
    void stop();
    void pause();
    void resume();

    
    //Get elapsed time
    Timer& getDuration();

    //Convert duration to milliseconds
    float toMillis();

    //Convert duration to microseconds
    float toMicros();

    //Convert duration to nanoseconds
    float toNanos();
};

#endif
