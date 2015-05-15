
#include <iostream>
#include <test_object.h>

// ### Exercise 4 - References ###
// # Firstly, some of the code has questions to it, please answer these in your own words. (not necessarily in code or comments. ;) )
// # Secondly, please fix any errors you see, explain your solution in your own words.
// # 
// # Bonus 1: Think of a good use of pointers, then try to implement it with references. Document any problems, errors and solutions.
// # Bonus 2: Make everything const correct.


// This codes has quite a few (intentional) errors in it
// TODO: debug and fix the errors.

// Should return the reference to a sum of two ints
// TODO: rewrite this as a template (without the bug)
template<typename T>
T& sumTheElements(const T& a, const T& b)
{
    static T result = a + b;
    return result;
}

int main()
{
    // using a bit of random here.
    srand((unsigned int) time(nullptr)); //initialize random seed based on time

    // Where will this object live? Is this a good choice? Why or why not?
    // Object allocated on the stack. Good for small objects, but not for big ones.
    TestObject anObject = TestObject(30);

    // Name differences between pointers and references:
    // - References cannot be null.
    // - References must be initialized when derlared.
    // - Once a reference is initialized to an object, it cannot be changed to refer to another object.
    // - Pointers can be pointed to another object at any time.

    // Has to return TestObject* rather then TestObject
    TestObject* anotherObject = new TestObject(2);

    // When should we use pointers, references and values? why?
    // Has to be anObject not &anObject
    TestObject& referenceToAnObject = anObject;

    //Has to be a *anotherObject not anotherObject
    TestObject &referenceToAnotherObject = *anotherObject;

    // Has to be &anObject not anObject
    TestObject* pointerToAnObject = &anObject;

    //Has to be &referenceToAnotherObject not referenceToAnotherObject
    TestObject* pointerToAnotherObject = &referenceToAnotherObject;
    

    // Testing if the correct strings are being printed.
    referenceToAnotherObject.printAllStrings();
    std::cout << "The Following should be the same as above: " << std::endl;
    pointerToAnotherObject->printAllStrings();

    std::cout << std::endl << "total number of strings:" << std::endl;
   
    int& totalElementNumber = sumTheElements(referenceToAnObject.elementCount(), referenceToAnotherObject.elementCount());
    std::cout << totalElementNumber << std::endl;
    std::cout << "Do calculations in another scope and display the same value again inside that scope: " << std::endl; 
    { 
        //this isn't really doing anything, just for testing purposes
        int ignoreMe = 20; 
        ignoreMe *= totalElementNumber;
        std::cout << totalElementNumber << std::endl;
    }

    std::cout << "And again outside the scope: " << std::endl;
    std::cout << totalElementNumber << std::endl;


    delete anotherObject;
    delete pointerToAnObject;
    delete pointerToAnotherObject;


}

