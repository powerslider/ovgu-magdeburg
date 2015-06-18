#define _CRT_SECURE_NO_WARNINGS	// For Windows-Users (ctime gets warnings) - all other can delete or ignore this line
#include <ctime>

#include "logger.hpp"
#include "policy.hpp"

/// \brief Helper function to get a time-string with second resolution.
static std::string getTimeString()
{
	std::string timeStr;
	time_t rawTime;
	time(&rawTime);
	timeStr = ctime(&rawTime);
	return timeStr.substr(0 , timeStr.size() - 1);
}

void Logger::write(const std::string &file, long line, const std::string &message)
{
	// Add more information to the message output
	std::string msg = getTimeString() +
			"  [" + file.substr(file.find_last_of("/\\") + 1) + " : " + std::to_string(line) + "] "
					  + message;

	for(size_t i = 0; i < m_policies.size(); ++i)
	{
		m_policies[i]->write( msg );
	}

	// Collect for the repository
	m_history.empty() ? (m_history = msg) : (m_history += '\n' + msg);
}

void Logger::registerPolicy(std::unique_ptr<Policy> _policy)
{
	m_policies.push_back(std::move(_policy));
	m_policies.back()->write(m_history);
}

Logger& Logger::instance()
{
	static Logger logger;
	return logger;
}
