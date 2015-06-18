#pragma once

#include <sstream>
#include <memory>
#include <vector>
#include "policy.hpp"

class Policy;

class Logger
{
public:
	/// \brief Send a message to each registered policy.
	/// \details Adds specific information about location and time to the
	///		message before output.
	/// \param [in] file The file of the calling code. Use __FILE__ to get
	///		this information.
	/// \param [in] line The line of code where the log message is sent.
	///		Use __LINE__ to get this information.
	/// \param [in] message A problem specific message text.
	// TODO: Implement this method
	// Hint: to get a nice time string use the utility function in logger.cpp
	void write(const std::string& file, long line, const std::string& message);

	// TODO: Implement this method
	void registerPolicy(std::unique_ptr<Policy> _policy);

	// TODO: Singleton access
	static Logger& instance();

private:
	// TODO: Hide constructors...
	Logger() {}

	// TODO: somehow collect the policies
	std::vector<std::unique_ptr<Policy>> m_policies;

	/// \brief Everything reported so far.
	/// \details This allows to write out messages even before a policy is added.
	///		Be careful about it in larger applications  - it might consume much space.
	std::string m_history;
};




/// The following macros insert the logging code in case the level is
///	set to the required level and do nothing else.
/// the do {} while(false) is a trick to get the macro behaving like a function.
///		You can do:
///			if( notOK )
///				LOG_ERROR("Not OK!");
///			else ...
///
///	This is only possible with this wired construct otherwise you will get
///	syntactical errors.
///	Play around if you are interested. Otherwise you can just IGNORE THIS PART.

#if (LOG_LEVEL <= 0 )
    /// \brief Report a message on lowest severity level
#   define LOG_LVL0(messageStream) do {std::ostringstream stream; stream << "<level 0> " << messageStream; Logger::instance().write(__FILE__, __LINE__, stream.str());} while(false)
#else
#   define LOG_LVL0(...) do {} while(false)
#endif

#if (LOG_LEVEL <= 1 )
    /// \brief Report a message on medium severity level
#   define LOG_LVL1(messageStream) do {std::ostringstream stream; stream << "<level 1> " << messageStream; Logger::instance().write(__FILE__, __LINE__, stream.str());} while(false)
#else
#   define LOG_LVL1(...) do {} while(false)
#endif

/// \brief Report a message on highest severity level
#define LOG_LVL2(messageStream) do {std::ostringstream stream; stream << "<level 2> " << messageStream; Logger::instance().write(__FILE__, __LINE__, stream.str());} while(false)

/// \brief Report an error message.
#define LOG_ERROR(messageStream) do {std::ostringstream stream; stream << "<error>   " << messageStream; Logger::instance().write(__FILE__, __LINE__, stream.str());} while(false)