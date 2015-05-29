#pragma once

// Include is necessary: inheritance cannot be solved with a forward declaration
#include "node.hpp"

#include <string>

class Value: virtual public Node
{
public:
	/// \brief The constructor must only take the string and store it for later.
	Value(const std::string& val) : m_val(val)	{}

	// Still Non-Copyable
	Value(const Value&) = delete;
	Value& operator = (const Value&) = delete;

	/// \brief Implements the clone operator. TODO: have a look at the
	///		implementation.
	virtual Node* clone() const override;

	using Node::evaluate;


	// TODO: fill this class
	~Value();

private:
	/// \brief Store the value as string.
	/// \details Imagine you parsed an input and you don't want to call
	///		expensive conversion before you know if you really need that
	///		value. So we store the raw input first.
	///		Further this enables to use other bases e.g.: 0xaffe
	std::string m_val;
};