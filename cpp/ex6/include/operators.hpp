#pragma once

// Include is necessary: inheritance cannot be solved with a forward declaration
#include "node.hpp"

/// \brief Binary addition operator
class OpAdd: virtual public Node
{
public:
	/// \brief Must have two defined children -> references are a good choice.
	/// \details You need to COPY the nodes to have an own node*. To do that
	///		you will need to add a clone() function which returns a real copy
	///		of the calling object by a �Node*�.
	///		Question: Is there an other solution and if not why?
	OpAdd(const Node& lhs, const Node& rhs);

	/// \brief Take ownership of two new children.
	/// \details Both parameters must be valid nodes which are not referenced
	///		elsewhere. The operator is responsible to delete them later.
	OpAdd(Node* lhs, Node* rhs);

	// Still Non-Copyable
	OpAdd(const OpAdd&) = delete;
	OpAdd& operator = (const OpAdd&) = delete;

	/// \brief Implements the clone operator. TODO: have a look at the
	///		implementation.
	virtual Node* clone() const override;

	using Node::evaluate;

	using Node::toString;

	// TODO: fill this class
	// TODO: don't forget the destructor
	~OpAdd();

private:
	Node* m_children[2];
};


/// \brief Binary multiplication operator (Ternary MAD????)
class OpMul: virtual public Node
{
public:
	/// \copydoc OpAdd(const Node&, const Node&)
	OpMul(const Node& lhs, const Node& rhs);

	/// \brief Take ownership of two new children.
	/// \details Both parameters must be valid nodes which are not referenced
	///		elsewhere. The operator is responsible to delete them later.
	OpMul(Node* lhs, Node* rhs);

	// Still Non-Copyable
	OpMul(const OpMul&) = delete;
	OpMul& operator = (const OpMul&) = delete;

	/// \brief Implements the clone operator. TODO: have a look at the
	///		implementation.
	virtual Node* clone() const override;

	using Node::evaluate;

	using Node::toString;

	// TODO: fill this class
	// TODO: don't forget the destructor
	~OpMul();

private:
	Node* m_children[2];
};


/// \brief Unary - operator
class OpNegate: virtual public Node
{
public:
	/// \copydoc OpAdd(const Node&, const Node&)
	OpNegate(const Node& lhs);

	/// \brief Take ownership of one new children.
	/// \details The Parameters must be a valid node which is not referenced
	///		elsewhere. The operator is responsible to delete it later.
	OpNegate(Node* rhs);

	// Still Non-Copyable
	OpNegate(const OpNegate&) = delete ;
	OpNegate& operator = (const OpNegate&) = delete;

	/// \brief Implements the clone operator. TODO: have a look at the
	///		implementation.
	virtual Node* clone() const override;

	using Node::evaluate;

	using Node::toString;

	// TODO: fill this class
	// TODO: don't forget the destructor
	~OpNegate();

private:
	Node* m_child;
};