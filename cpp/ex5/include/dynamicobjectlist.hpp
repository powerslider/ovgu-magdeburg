#pragma once

class Object;

/// \brief A dynamic list of objects.
///
/// It is similar to the inner workings of Java's ArrayList. Intentionally it is not possible to add already existing objects.
/// This way DynamicObjectList is responsible for deleting and creating new objects.
class DynamicObjectList
{
public:
  /// Constructs a new, empty dynamic object list.
	DynamicObjectList();
    DynamicObjectList(const DynamicObjectList& otherList);

  /// Destroys the list
  ~DynamicObjectList();

  /// sort list using </> operators for object
  void sort();
  /// Returns the number of valid objects.
  unsigned int getCount() { return m_count; }

  unsigned int getCapacity() { return m_capacity; }


  /// Reallocates the intern array to at least a given capacity.
  /// \param capacity
  ///   After this call the capacity will be at least this value.
  ///   If the current capacity is equal or greater, nothing will happen.
  void reserve(unsigned int capacity);
  ///Shrink allocated memory to max 5*m_count, do not shrink under 2*m_count, only half or double m_capacity
  ///e.g. [1,2,3,4] with m_capacity=16 has to be shrunk to [1,2,3] with m_capacity=8 if the last element is removed
  /// shrink is called when an object is deleted from the list
  void shrink();

  /// \brief Creates a new object at the end of the list.
  ///
  /// If the capacity is too small, the intern array will be reallocated with the double size. 
  /// \param name
  ///   The name of the object.
  /// \return 
  ///   The just created object.
  Object* createObject_back(char* name);

  /// \brief Push an existing object the end of the list.
  ///
  /// If the capacity is too small, the intern array will be reallocated with the double size. 
  /// \param name
  ///   The name of the object.
  /// \return 
  ///   The just created object.
  Object* push_back(Object*);

  /// \brief Creates a new object at the start of the list.
  ///
  /// Contrary to createObject_back, this means that all existing objects have to be moved!
  /// If the capacity is too small, the intern array will be reallocated with the double size. 
  /// \param name
  ///   The name of the object.
  /// \return
  ///   The just created object.
  Object* createObject_front(char* name);

  /// \brief Destroys an object at a given position.
  /// 
  /// Will relocate all objects after this position (greater than position) to avoid a gap.
  /// It will not reallocate any memory - that means, that the capacity stays the same.
  ///
  /// \param position
  ///    Index of the object that should be deleted. If the list does not have this many objects, nothing will happen.
  void destroyObject(unsigned int position);

  /// \brief Returns the object at the given position index.
  /// \param position
  ///    Index of the queried object. 
  /// \return
  ///    Object pointer at position or nullptr if there is no such object.
  Object* getAt(unsigned int position);

  Object* getAt(const unsigned int position) const;
 

private:
  /// The current number of objects, contained in this list
  unsigned int m_count;

  /// The current size of the intern object array.
  unsigned int m_capacity;

  /// The intern list of object pointer. A array of size m_capacity.
  Object** m_list;

public:
	/// Implementiert den jeweils angegebenen und die verwandten Operatoren
    DynamicObjectList& operator=(const DynamicObjectList& otherList);
	///push_back element
	DynamicObjectList& operator+=(const Object&);
	///push_back element
	DynamicObjectList& operator+=(Object&);
	///merge lists, use multiset union semantic.
	///e.g. [1,2,3,4,5,6]+=[2,5,9,9] returns [1,2,3,4,5,6,2,5,9,9]
	DynamicObjectList& operator|=(const DynamicObjectList&);
	///intersect lists
	///e.g. [1,2,3,4,5,6,9,21]&=[2,5,9,9] returns [2,5,9,9]
	DynamicObjectList& operator&=(const DynamicObjectList&);
	///get element @position i
	///e.g. a[3] returns 3 for the list a=[0,1,2,3,4]
	Object& operator[](const unsigned int) const;
	Object& operator[](const unsigned int);
};