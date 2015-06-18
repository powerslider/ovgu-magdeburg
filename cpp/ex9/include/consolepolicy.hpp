#pragma once

#include "policy.hpp"

/// \brief This policy simply uses std::cerr for debug output.
class ConsolePolicy: public Policy
{
public:

	// Copy constructor is auto generated
	// Destructor is auto generated

	virtual void write( const std::string& message ) override;
};