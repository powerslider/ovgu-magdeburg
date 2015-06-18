#pragma once

#include <string>

/// \brief Basic interface class to handle the output generic
class Policy
{
public:
	virtual ~Policy()	{}

	/// \brief Output a fully formated error message.
	/// \param [in] message A full message string but without a line end.
	virtual void write(const std::string& message) = 0;
};