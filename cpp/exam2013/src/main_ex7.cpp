#include "ex7.h"
#include <iostream>

int main()
{
   Laser laser;
   SpaceShip* ship = new SpaceShip(100, laser);
   ship->applyDamage(25);

   std::cout << "HEALTH: " << ship->getRelativeHealth() << std::endl;

   return 0;
}