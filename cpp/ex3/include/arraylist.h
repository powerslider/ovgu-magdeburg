/**
 * @file    arraylist.h
 * @author  Tsvetan Dimitrov <tsvetan.dimitrov23@gmail.com>
 * @date    2015-05-03
 *
 * @class   ArrayList
 * @brief   Header defining class ArrayList
 * @details Defines list operations such as add, remove etc.
 */
#ifndef ARRAY_LIST_H
#define ARRAY_LIST_H

#include <stddef.h>

using namespace std;

template <class T>
class ArrayList
{
public:

    ArrayList(size_t capacity);
    ~ArrayList();
    /**
     * Returns the number of valid objects.
     */
    size_t getSize() { return this->count; }

    /**
     * Returns the capacity of the intern array.
     */
    size_t getCapacity() { return this->capacity; }

    /**
     * @brief Add a new object at the end of the list.
     *
     * If the capacity is too small, the internal array will be reallocated with the double size.
     *
     * @param item
     *   The name of the object.
     * @return
     *   If the object was added succesfully.
     */
    bool addBack(const T &item);


    /**
     * @brief Add a new object at the start of the list.
     *
     * Contrary to addBack, this means that all existing objects have to be moved!
     * If the capacity is too small, the internal array will be reallocated with the double size.
     *
     * @param item
     *   The name of the object.
     * @return
     *   If the object was added succesfully.
     */
    bool addFront(const T &item);

    /**
     * @brief Destroys an object at a given position.
     *
     * Will relocate all objects after this position (greater than position) to avoid a gap.
     * It will not reallocate any memory - that means, that the capacity stays the same.
     *
     * @param position
     *    Index of the object that should be deleted. If the list does not have this many objects, nothing will happen.
     */
    bool destroy(unsigned int position);

    /**
     * @brief Returns the object at the given position index.
     * @param position
     *    Index of the queried object.
     * @return
     *    Object pointer at position or nullptr if there is no such object.
     */
    T* getAt(unsigned int position);

private:

    T* arr;
    size_t count;
    size_t capacity;

    /**
     * Reallocates the intern array to at least a given capacity.
     *
     * @param capacity
     *   After this call the capacity will be at least this value.
     *   If the current capacity is equal or greater, nothing will happen.
     */
    void reserve(size_t capacity);
};
#endif
