#pragma once

#include <sstream>
#include <stdexcept>

/// \brief Mixin for calculating the length (euclidian norm / distance) of a vector
// BONUS: Implement a mixin that calculates the length of a vector.
// Change the vector template to include this mixin.


/// \brief Template class for (math) vectors of variable length and type.
/// Works with all basic number types.
template<unsigned int Size, typename Element = float>
class Vector
{
public:

	/// Initializes all elements with zero.
	Vector();

	/// Copies the vector.
	Vector(const Vector& cpy);
	
	/// Copies a vector of an arbitrary type.
	/// The type must be convertible by a static_cast to Element.
	template<typename ElementRhs>
	Vector(const Vector<Size, ElementRhs>& cpy);

	/// \brief Initializes the vector from a string.
	/// The string should contain values, separated by spaces. Uses stringstream for parsing.
	/// Will set all other elements to zero.
	Vector(const std::string& values);

	/// \brief Returns a string representation of the vector.
	operator std::string();

	/// \brief Read access to an element.
	/// Checks for errors in debug mode - will then perform "throw std::out_of_range("Invalid index!")" on error.
	const Element& operator[] (unsigned int index) const;

	/// \brief Write access to an element.
	/// Checks for errors in debug mode - will then perform "throw std::out_of_range("Invalid index!")" on error.
	Element& operator[] (unsigned int index);

	/// \brief Adds to vectors of the same size elementwise.
	Vector<Size, Element> operator + (const Vector& rhs) const;

	/// \brief Simple template programming trick to make size accessible from outside.
	enum
	{
		SIZE = Size
	};

private:
	/// Intern data representation.
	Element elements[Size];
};


/// \brief Templates must be defined in the header file - that is why we include
/// the implementation here.
#include "myvector.inl"
