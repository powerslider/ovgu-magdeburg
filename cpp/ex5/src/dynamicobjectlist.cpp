#include "dynamicobjectlist.h"
#include "object.h"

DynamicObjectList::DynamicObjectList()
        : m_count(0),
          m_capacity(0),
          m_list(nullptr)
{
}

DynamicObjectList::DynamicObjectList(DynamicObjectList const &otherList)
        : m_count(otherList.m_count),
          m_capacity(otherList.m_capacity),
          m_list(otherList.m_list)
{
    m_list = new Object*[m_capacity];

    for (int i = 0; i < m_capacity; ++i)
    {
        m_list[i] = otherList.m_list[i];
    }
}

DynamicObjectList::~DynamicObjectList()
{
    delete[] m_list;
}

void DynamicObjectList::reserve(unsigned int capacity)
{
  // Check if anything should happen.
  if(m_capacity >= capacity)
    return;

  // Create new object array with the given capacity.
  Object** newArray = new Object*[capacity];

  // Moves old list contents to new list.
  for(unsigned int i = 0; i < m_count; ++i)
  {
      newArray[i] = m_list[i];
  }

  // Optional: Set all other pointer to nullptr, just for easier debugging.
  for(unsigned int i = m_count; i < capacity; ++i)
  {
      newArray[i] = nullptr;
  }

  // delete old array!
  delete[] m_list;

  // Save new data.
  m_list = newArray;
  m_capacity = capacity;
}
Object* DynamicObjectList::createObject_back(char* name)
{
	// Ensure capacity.
	reserve((m_count + 1) * 2);

	// Add new object.
	m_list[m_count] = new Object(name);

	return m_list[m_count++];
}

void DynamicObjectList::sort()
{
	for (unsigned int j = 1; j < m_count - 2; j++)    // Start with 1 (not 0)
	{
		unsigned int i = 0;
		Object* tmp = m_list[j];
		for (i = j - 1; (i >= 0) && (*m_list[i] > *tmp); i--)   // Smaller values move up
		{
			m_list[i + 1] = m_list[i];
		}
		m_list[i + 1] = tmp;    //Put key into its proper location
	}
}

Object* DynamicObjectList::push_back(Object *a)
{
	if (a == nullptr)
		return a;
	// Ensure capacity.
	reserve((m_count + 1) * 2);

	// Add new object.
	m_list[m_count] = a;

	return m_list[m_count++];
}

Object* DynamicObjectList::createObject_front(char* name)
{
  // Ensure capacity.
  reserve((m_count + 1) * 2);

  // Move all other objects.
  for (unsigned int i = m_count; i >= 1; --i)
  {
      m_list[i] = m_list[i - 1];
  }

  // Add new object.
  m_list[0] = new Object(name);
  ++m_count;

  return m_list[0];
}

void DynamicObjectList::destroyObject(unsigned int position)
{
  // Does the element exist?
  if(m_count <= position)
  {
      return;
  }

  // Destroy the object at the given position.
  delete m_list[position];

  // Now we have less objects!
  --m_count;

  // Move all other objects.
  for (unsigned int i = position; i < m_count; ++i)
  {
    m_list[i] = m_list[i + 1];
  }
  shrink();
  // Optional: Set former back to nullptr for easier debugging.
  m_list[m_count] = nullptr;
}

Object* DynamicObjectList::getAt(unsigned int position)
{
	// Does the element exist?
	if (m_count <= position)
	{
		return nullptr;
	}

	return m_list[position];
}


Object* DynamicObjectList::getAt(const unsigned int position) const
{
    // Does the element exist?
    if (m_count <= position)
    {
        return nullptr;
    }

    return m_list[position];
}


void DynamicObjectList::shrink()
{
    unsigned int newCapacity = m_capacity / 2;

    if (m_capacity >= 2 * m_count && newCapacity < 5 * m_count)
    {
        reserve(newCapacity);
    }
}

/**
 * Overloaded operators
 */
DynamicObjectList& DynamicObjectList::operator=(const DynamicObjectList &otherList)
{
    delete[] m_list;
    m_count = otherList.m_count;
    m_capacity = otherList.m_capacity;
    m_list = new Object*[m_capacity];
    for (int i = 0; i < m_capacity; ++i)
    {
        m_list[i] = otherList.m_list[i];
    }

    return *this;
}

DynamicObjectList& DynamicObjectList::operator+=(Object const &aConst)
{
    const Object* pointerToObj = &aConst;
    push_back(const_cast<Object*>(pointerToObj));
    return *this;
}

DynamicObjectList& DynamicObjectList::operator+=(Object &object)
{
    Object* pointerToObj = &object;
    push_back(pointerToObj);
    return *this;
}

//DynamicObjectList& DynamicObjectList::operator|=(DynamicObjectList const &aConst)
//{
//  return <#initializer#>;
//}
//
//DynamicObjectList& DynamicObjectList::operator&=(DynamicObjectList const &aConst)
//{
//  return <#initializer#>;
//}

const Object& DynamicObjectList::operator[](unsigned int const i) const
{
    return *getAt(i);
}

Object& DynamicObjectList::operator[](unsigned int const i)
{
    return *getAt(i);
}
