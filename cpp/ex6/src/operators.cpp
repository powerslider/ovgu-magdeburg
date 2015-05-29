#include <cassert>
#include "operators.hpp"

// ************************************************************************* //
OpAdd::OpAdd(const Node& lhs, const Node& rhs)
{
	m_children[0] = lhs.clone();
	m_children[1] = rhs.clone();
}

OpAdd::OpAdd(Node* lhs, Node* rhs)
{
	assert(lhs != nullptr);
	assert(rhs != nullptr);
	m_children[0] = lhs;
	m_children[1] = rhs;
}

Node* OpAdd::clone() const
{
	// Cloning requires a deep copy. Otherwise multiple pointer would address
	// the same memory.
	return new OpAdd(m_children[0]->clone(), m_children[1]->clone());
}

int OpAdd::evaluate(const VariableMap *_varMap) const
{
	return m_children[0]->evaluate(_varMap) + m_children[1]->evaluate(_varMap);
}

std::string OpAdd::toString() const
{
	return "(" + m_children[0]->toString() + "+" + m_children[1]->toString();
}

OpAdd::~OpAdd()
{
	delete[] m_children;
}

// ************************************************************************* //
OpMul::OpMul(const Node& lhs, const Node& rhs)
{
	m_children[0] = lhs.clone();
	m_children[1] = rhs.clone();
}

OpMul::OpMul(Node* lhs, Node* rhs)
{
	assert(lhs != nullptr);
	assert(rhs != nullptr);
	m_children[0] = lhs;
	m_children[1] = rhs;
}

Node* OpMul::clone() const
{
	// Cloning requires a deep copy. Otherwise multiple pointer would address
	// the same memory.
	return new OpMul(m_children[0]->clone(), m_children[1]->clone());
}

int OpMul::evaluate(const VariableMap *_varMap) const
{
	return m_children[0]->evaluate(_varMap) * m_children[1]->evaluate(_varMap);
}

std::string OpMul::toString() const
{
	return "(" + m_children[0]->toString() + "*" + m_children[1]->toString();
}

OpMul::~OpMul()
{
	delete[] m_children;
}

// ************************************************************************* //
OpNegate::OpNegate(const Node& lhs)
{
	m_child = lhs.clone();
}


OpNegate::OpNegate(Node* rhs)
{
	assert(rhs != nullptr);
	m_child = rhs;
}

Node* OpNegate::clone() const
{
	// Cloning requires a deep copy. Otherwise multiple pointer would address
	// the same memory.
	return new OpNegate(m_child->clone());
}

int OpNegate::evaluate(const VariableMap *_varMap) const
{
	return (-1) * m_child->evaluate(_varMap);
}

std::string OpNegate::toString() const
{
	return "-" + m_child->toString();
}

OpNegate::~OpNegate()
{
	delete m_child;
}
