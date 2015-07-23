cmake_minimum_required (VERSION 2.6)

#Project name
project (exam2013)

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
add_executable(exam2013_ex1 src/main_ex1.cpp)
add_executable(exam2013_ex2 src/main_ex2.cpp)
add_executable(exam2013_ex3 src/main_ex3.cpp)
add_executable(exam2013_ex4 src/main_ex4.cpp)
add_executable(exam2013_ex5 src/main_ex5.cpp)
add_executable(exam2013_ex6 src/main_ex6.cpp)
add_executable(exam2013_ex7 src/main_ex7.cpp)
