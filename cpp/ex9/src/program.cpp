/// \brief This define controls the amount of reported messages
#define LOG_LEVEL 1
#include "logger.hpp"
#include "filepolicy.hpp"
#include "consolepolicy.hpp"

#include <type_traits>

/// \brief This class only exists for demonstration purpose.
class UltimativeNotifier
{
public:
	UltimativeNotifier() { LOG_LVL2("Created important static object."); }
	~UltimativeNotifier() { LOG_LVL2("Destroyed important static object."); }
};

/// \brief Use a static instance to demonstrate that the logger is active from
///		the very beginning.
static UltimativeNotifier g_notify;


/// \brief Static testing if the singleton could be copied.
//static_assert( !std::is_copy_constructible<Logger>::value, "Your Logger can still be copied by Copy-Ctor" );
// The second test here is buggy in v110 and v120
//static_assert( !std::is_copy_assignable<Logger>::value, "Your Logger can still be copied by operator=()" );

int main()
{
	LOG_LVL1("Reached entry point.");

	LOG_LVL0("Last message before policy registration.");
	std::unique_ptr<Policy> filePolicy(new FilePolicy("log.txt"));
	Logger::instance().registerPolicy(std::move(filePolicy));
	LOG_LVL0("Registered file policy.");

	std::unique_ptr<Policy> consolePolicy(new ConsolePolicy());
	Logger::instance().registerPolicy( std::move(consolePolicy) );
	LOG_LVL0("Registered console policy.");

	LOG_ERROR("Error code: " << 321 << " - yea a stupid number!");

	LOG_LVL1("Reached end of program.");
	return 0;

	// If you did everything correct you should get a file log.txt with the
	// following content (up to formation):
	// Mon May 19 17:44:08 2014  [program.cpp : 10] <level 2> Created important static object.
	// Mon May 19 17:44:08 2014  [program.cpp : 20] <level 1> reached entry point.
	// Mon May 19 17:44:08 2014  [program.cpp : 26] <error>   Bad
	// Mon May 19 17:44:08 2014  [program.cpp : 28] <level 1> Reached end of program.
	// Mon May 19 17:44:08 2014  [program.cpp : 11] <level 2> Destroyed important static object.
	// 
	// If you change the LOG_LEVEL above, the number of messages should
	// increase / decrease by 2 (in each direction respectively).
}