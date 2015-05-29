#include "valuenode.hpp"
#include "operators.hpp"

#include <iostream>

int main()
{
	// Map a single variable
	VariableMap variables;
	variables["x"] = 42;

	// Create the following syntax tree for the expression
	// 2 * -(x + 42) + -2
	//         +
	//     *       -2
	//   2   -
	//       +
	//     x   42
	Node* expression = new OpAdd(new OpMul(new Value("2"),
										   new OpNegate(new OpAdd(new Value("x"),
										                          new Value("42")))),
								 new Value("-2"));

	//std::cout << "The expression: " << *expression << " evaluates to ";
	std::cout << expression->evaluate(&variables) << std::endl;
	// If you did everything right the output should be:
	// The expression: ((2 * -(x + 42)) + -2) evaluates to -170

	delete expression;

	Node* expression2 = new OpNegate(OpMul(Value("x"), Value("x")));
	//std::cout << "The expression: " << *expression2 << " evaluates to ";
	std::cout << expression2->evaluate(&variables) << std::endl;
	// If you did everything right the output should be:
	// The expression: -(x * x) evaluates to -1764
	
	delete expression2;

	return 0;
}