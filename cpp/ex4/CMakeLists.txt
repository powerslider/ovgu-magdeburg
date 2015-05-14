cmake_minimum_required (VERSION 2.6)

#Project name
project (randomchar)

#Check support of the compiler for c++11.
#Various compiler versions of gcc and clang use different flags to specify C++11 support,
#namely older ones accept -std=c++0x and newer one -std=c++11
include(CheckCXXCompilerFlag)
CHECK_CXX_COMPILER_FLAG("-std=c++11" COMPILER_SUPPORTS_CXX11)
CHECK_CXX_COMPILER_FLAG("-std=c++0x" COMPILER_SUPPORTS_CXX0X)
if(COMPILER_SUPPORTS_CXX11)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
elseif(COMPILER_SUPPORTS_CXX0X)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++0x")
else()
    message(STATUS "The compiler ${CMAKE_CXX_COMPILER} has no C++11 support. Please use a different C++ compiler.")
endif()

#Set build directory to "bin"
set(EXECUTABLE_OUTPUT_PATH "${CMAKE_CURRENT_SOURCE_DIR}/bin")

#Detect header files in "include" folder
include_directories(include)

#Generate executable
add_executable(randomchar
                src/program.cpp
                src/test_object.cpp
                src/random_char.cpp)
