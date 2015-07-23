#include "ex7.h"
#include <iostream>

int main()
{
   Laser laser;
   Laser enemyLaser;

   SpaceShip* ship = new SpaceShip(100, laser);
   SpaceShip* enemyShip = new SpaceShip(100, enemyLaser);

   ship->getLaser().shootLaser(*enemyShip);
   enemyShip->getLaser().shootLaser(*ship);

   std::cout << "SHIP HEALTH: " << ship->getRelativeHealth() << std::endl;
   std::cout << "ENEMY SHIP HEALTH: " << enemyShip->getRelativeHealth() << std::endl;

   return 0;
}