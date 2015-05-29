#pragma once

#include <string>
#include <ostream>
#include <unordered_map>

/// \brief Usage of an associative container to store pairs of a variable name
///		and values.
///	\details An expression could contain variable names instead of values.
///		During evaluation the map is used to lookup the current values of the
///		specific variable.
///	
///		This is only a typedef to reduce the code size.
typedef std::unordered_map<std::string, int> VariableMap;

/// \brief Base class for a simple syntax tree.
/// \details You do not need to change this class much - there is only one TODO. This is the
///		interface/base class for all other node types which must implement
///		all the methods.
class Node
{
public:
	/// \brief Empty default constructor - required because the removal of
	///		of the copy CTor also deletes the normal standard constructor.
	Node() {}

	/// \brief Each child class must know how to evaluate itself
	/// \param [in] _varMap Read access to the variable mapping.
	/// 
	///		When trying to evaluate a string value first use
	///		�_varMap->find(valueString)�. 
	///		�find� returns an iterator which is equal to _varMap->end() when
	///		the key cannot be found.
	///		If there is an entry with that name use its value, otherwise do a
	///		node specific evaluation.
	virtual int evaluate(const VariableMap* _varMap) const = 0;

	// TODO: the destructor
	virtual ~Node() {};

	/// \brief Create a string representation for the expression
	/// \details
	///		Use the toString() method of the children and create a new one
	///		by concatenation or a sstream. For operators use brackets to make
	///		the output unequivocal.
	/// 
	///		Question: Why not return a pointer or reference?
	virtual std::string toString() const = 0;

	/// \brief Create a copy of THIS object.
	virtual Node* clone() const = 0;

	/// \brief Make non-copyable to force the usage of clone (otherwise you
	///		would also need to implement the two operators in addition.
	Node(const Node&) = delete;
	Node& operator = (const Node&) = delete;
};

// Extend c++ streaming for a possibility to print expressions
// TODO
// Write a stream operator << which uses the toString() method to create a
// representation of the expression.
std::ostream &operator<<(std::ostream &lhs, Node const &rhs);