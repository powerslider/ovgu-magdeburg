class Wing;
class Body;
class Laser;
class Cargo;

class SpaceShip
{
public:
    Wing* left;
    Body& body;
    Wing* right;

};

class Wing
{
public:
    SpaceShip* ship;
    Laser& laser;
};

class Body
{
public:
    SpaceShip* ship;
    Cargo* cargo;
};

struct Cargo
{
    int data;
};