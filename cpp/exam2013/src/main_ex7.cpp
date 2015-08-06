#include "ex7.h"
#include <iostream>

int main()
{
   Laser laserStack;
   Laser enemyLaserStack;

   SpaceShip shipStack(100, laserStack);
   SpaceShip enemyShipStack(100, enemyLaserStack);

   shipStack.getLaser().shootLaser(enemyShipStack);
   enemyShipStack.getLaser().shootLaser(shipStack);

   std::cout << "SHIP HEALTH [STACK]: " << shipStack.getRelativeHealth() << std::endl;
   std::cout << "ENEMY SHIP HEALTH [STACK]: " << enemyShipStack.getRelativeHealth() << std::endl;

   /**********************************************************************************************/

   Laser* laserHeap = new Laser();
   Laser* enemyLaserHeap = new Laser();

   SpaceShip* shipHeap = new SpaceShip(100, *laserHeap);
   SpaceShip* enemyShipHeap = new SpaceShip(100, *enemyLaserHeap);

   shipHeap->getLaser().shootLaser(*enemyShipHeap);
   enemyShipHeap->getLaser().shootLaser(*shipHeap);

   std::cout << "SHIP HEALTH [HEAP]: " << shipHeap->getRelativeHealth() << std::endl;
   std::cout << "ENEMY SHIP HEALTH [HEAP]: " << enemyShipHeap->getRelativeHealth() << std::endl;

   return 0;
}