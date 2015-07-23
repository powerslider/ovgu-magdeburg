#include "ex1.h"


int main()
{
    Wing leftWing;

    //get laser of the right wing
    Laser& laser = leftWing.ship->right->laser;

    //get data of the spaceship
    int spaceShipData = leftWing.ship->body.cargo->data;

    //get pointer to body of spaceship
    Body* spaceShipBody = &(leftWing.ship->body);

    //dereference pointer to body of spaceship
    Body& spaceShipBodyDereferenced = *spaceShipBody;

    //get object of body of spaceship
    Body spaceShipBodyObj = *(&spaceShipBodyDereferenced);

    // get type of &(leftWing.laser)
    Laser* laser1 = &(leftWing.laser);

    //---------Both are the same---------
    Wing dereferencedWing = *(&leftWing);
    Wing arrayElementWing = (&leftWing)[0];
    //-----------------------------------

    //get type of &(&(leftWing)[0].ship->right)
    Wing** wing1 = &((&leftWing)[0].ship->right);

    return 0;
}
