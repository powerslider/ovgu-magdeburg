cmake_minimum_required (VERSION 2.8)

# Project name
set(PROJECT_NAME_STR unit_tests_demo)
project(${PROJECT_NAME_STR})

# Check support of the compiler for c++11.
# Various compiler versions of gcc and clang use different flags to specify C++11 support,
# namely older ones accept -std=c++0x and newer one -std=c++11
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

if(CMAKE_COMPILER_IS_GNUCXX)
    add_definitions(-Wall -ansi -Wno-deprecated -pthread)
endif()

# Locate pthreads required from gtest
find_package(Threads REQUIRED)

# Set project includes folder
set(COMMON_INCLUDES ${PROJECT_SOURCE_DIR}/include)

# Set gtest include folder
set(EXT_PROJECTS_DIR ${PROJECT_SOURCE_DIR}/ext)

# Add gtest submodule
add_subdirectory(${EXT_PROJECTS_DIR}/gtest)

# Enable testing
enable_testing()
# Set project test name
set(PROJECT_TEST_NAME ${PROJECT_NAME_STR}_test)

# Detect header files in the project and gtest
include_directories(${GTEST_INCLUDE_DIRS} ${COMMON_INCLUDES})

# Get all test files
file(GLOB TEST_SRC_FILES ${PROJECT_SOURCE_DIR}/test/*.cpp)
# Add test files to the executable
add_executable(${PROJECT_TEST_NAME} ${TEST_SRC_FILES})
add_dependencies(${PROJECT_TEST_NAME} googletest)
target_link_libraries(${PROJECT_TEST_NAME}
        ${GTEST_LIBS_DIR}/libgtest.a
        ${GTEST_LIBS_DIR}/libgtest_main.a
)

target_link_libraries(${PROJECT_TEST_NAME} ${CMAKE_THREAD_LIBS_INIT})

add_test(test1 ${PROJECT_TEST_NAME})
