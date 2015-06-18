#include <filepolicy.hpp>
#include <stdexcept>

FilePolicy::FilePolicy(const char* fileName)
        : m_filename(fileName)
{
    if (m_filename.bad())
    {
        throw std::runtime_error("Cannot open log file to create the logger policy!!!");
    }
}

void FilePolicy::write(const std::string& message)
{
    m_filename << message << std::endl;
    m_filename.flush();
}
