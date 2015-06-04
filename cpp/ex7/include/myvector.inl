#include "myvector.h"

template<unsigned int Size, typename Element>
Vector<Size, Element>::Vector()
{
	for (unsigned int i = 0; i < Size; ++i)
		elements[i] = 0;
}


template<unsigned int Size, typename Element>
Vector<Size, Element>::Vector(const Vector& cpy)
{
	for (unsigned int i = 0; i < Size; ++i)
		elements[i] = cpy[i];
}

template<unsigned int Size, typename Element>
template<typename ElementRhs>
Vector<Size, Element>::Vector(const Vector<Size, ElementRhs>& cpy)
{
	for(unsigned int i = 0; i < Size; ++i)
		elements[i] = static_cast<Element>(cpy[i]);
}

template<unsigned int Size, typename Element>
Vector<Size, Element>::Vector(const std::string& values)
{
	std::stringstream valueStream(values);
	unsigned int curElement = 0;
	while (!valueStream.eof() && curElement < Size)
		valueStream >> elements[curElement++];
	for (unsigned int i = curElement; i < Size; ++i)
		elements[i] = 0;
}


template<unsigned int Size, typename Element>
Vector<Size, Element>::operator std::string()
{
	std::string out;
	for (unsigned int i = 0; i < Size; ++i)
		out += std::to_string(elements[i]) + ' ';
	return out;
}


template<unsigned int Size, typename Element>
const Element& Vector<Size, Element>::operator[] (unsigned int index) const
{
#ifndef NDEBUG
	if (index >= Size)
		throw std::out_of_range("Invalid index!");
#endif

	return elements[index];
}


template<unsigned int Size, typename Element>
Element& Vector<Size, Element>::operator[] (unsigned int index)
{
#ifndef NDEBUG
	if (index >= Size)
		throw std::out_of_range("Invalid index!");
#endif

	return elements[index];
}


template<unsigned int Size, typename Element>
Vector<Size, Element> Vector<Size, Element>::operator + (const Vector<Size, Element>& rhs) const
{
	Vector<Size, Element> out;
	for (unsigned int i = 0; i < Size; ++i)
		out[i] = static_cast<Element>((*this)[i] + rhs[i]);
	return out;
}
