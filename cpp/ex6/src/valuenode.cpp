#include "valuenode.hpp"


Node* Value::clone() const
{
	// Create a new instance with the same type and data. As you can see we
	// need to know the type �Value� that is why we cannot implement it
	// elsewhere.
	return new Value(m_val);
}

// TODO: evaluate implementation using string to int conversation functions
int Value::evaluate(const VariableMap *_varMap) const
{
	auto p = _varMap->find(m_val);
	if (p != _varMap->end())
	{
		return p->second;
	}

	return (int) atol(m_val.c_str());
}

Value::~Value() {
	delete m_val;
}
