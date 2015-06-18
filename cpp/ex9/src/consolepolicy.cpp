#include <iostream>

#include "consolepolicy.hpp"

void ConsolePolicy::write(const std::string& message)
{
	std::cerr << message << std::endl;
}