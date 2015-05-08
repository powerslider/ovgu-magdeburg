/**
 * @file    arraylist.cpp
 * @author  Tsvetan Dimitrov <tsvetan.dimitrov23@gmail.com>
 * @date    2015-05-03
 *
 * @class   ArrayList
 * @brief   Implementation of ArrayList class methods.
 * @details Implementation of ArrayList class operations such as add, remove, indexOf...
 */
#include "arraylist.h"
#include <iostream>
#include <string.h>


template <typename T>
ArrayList<T>::ArrayList(size_t capacity)
{
    this->capacity = capacity;
    this->arr = new T[capacity];
}

template <typename T>
ArrayList<T>::~ArrayList()
{
    delete[] this->arr;
    this->capacity = 0;
    this->count = 0;
    this->arr = nullptr;
}

template <typename T>
void ArrayList<T>::reserve(size_t capacity)
{
    if (this->capacity == capacity)
    {
        T* tempArr = new T[capacity];
        std::copy(&this->arr[0], &this->arr[count - 1], &tempArr[0]);

        this->arr = tempArr;
        this->count = 0;
        this->capacity = sizeof(this->arr) / sizeof(this->arr[0]);
    }

}

template <typename T>
bool ArrayList<T>::addBack(const T &item)
{
    this->arr[count] = item;
    this->count++;
    if (this->capacity == this->count)
    {
        reserve(this->capacity / 2);
    }
    return true;
}

template <typename T>
bool ArrayList<T>::addFront(const T &item)
{
    memmove(&this->arr[1], &this->arr[0], sizeof(this->arr[capacity]) - sizeof(this->arr[0]));
    this->arr[0] = item;
    this->count++;
    if (this->capacity == this->count)
    {
        reserve(this->capacity / 2);
    }
    return true;
}

template <typename T>
bool ArrayList<T>::destroy(unsigned int position)
{
    memmove(&this->arr[position + 1], &this->arr[position],  sizeof(this->arr[capacity]) - sizeof(this->arr[position]));
    return true;
}

template <typename T>
T* ArrayList<T>::getAt(unsigned int position)
{
    return &this->arr[position];
}
