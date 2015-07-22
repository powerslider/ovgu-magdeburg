// Task: write a program that handles interactions between planets, spaceships and deathstars
// Do this with unit tests.
// Step 1. Write Unit Tests for each class and functionality first. 
// Step 2. Write the classes and functions until each unit test passes.
//         - Also write error, debug and status output. 
//         - Each function should have error logging as well as "in-game" output. ("Deathstar was destroyed")
// Step 3. Test your work with the unit tests.
// 
// Note: Use exceptions and asserts where necessary / appropriate to test for programming and user errors.
// BONUS: write error logs to a seperate file, only display "normal" output in the console.

// Classes: Target, Planet, Spaceship and Deathstar. 
// Planet, Spaceship and Deathstar are all be targets. 

// Target.
// Every Target has a position and name. 
// Each target can be attacked and can explode
// Exploding cannot be triggered from outside the class. (only through receiving damage)
// Should have a stream out operator. 
// Can be purely virtual (abstract).

// Planet
// Inherits from Target
// Can only be destroyed be Deathstars.
// Planets cannot attack

// Spaceship
// Inherits from Target
// Has a number of lasers
// Has shields (percentage)
// Can attack other targets
// When attacked, shields are damaged. When shields <= 0, it explodes.

// Deathstar
// Inherits from Spaceship
// Has far higher shields
// Can be damaged by spaceships with a 25% chance of exploding directly (without needing to get the shields to zero).
//        (- the reactor core can be hit through the exhaust vent. ;) )       


void main()
{
    

}
