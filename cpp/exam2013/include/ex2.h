#pragma once


#include <iostream>

class Animal
{
public:
    ~Animal() { std::cout << "die" << std::endl; }
    virtual void move() = 0;
};

class Fish : public Animal
{
public:
    virtual ~Fish() { std::cout << "turn upside down" << std::endl; }
    virtual void move() { std::cout << "swim" << std::endl; }
};

class Bird : public Animal
{
public:
    virtual void move() { std::cout << "fly" << std::endl; }
};

class Penguin : public Bird
{
public:
    ~Penguin() { std::cout << "freeze" << std::endl; }
    virtual void move() { std::cout << "swim or walk" << std::endl; }
};
