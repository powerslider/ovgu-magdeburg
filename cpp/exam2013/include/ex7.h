#pragma once

class Laser;

class SpaceShip
{
public:
    void applyDamage(float damage)
    {
        health -= damage;
        if (health <= 0) throw "BIG EXPLOSION";
    }

    SpaceShip(float startHealth, /*const*/ Laser& laser)
            : totalHealth(startHealth), health(startHealth), laser(laser)
    {
    }

    float getRelativeHealth() const
    {
        return health / totalHealth;
    }

    Laser& getLaser() const { return laser; }

private:
    const float totalHealth;
    float health;
    Laser& laser;
};

class Laser {
public:
    Laser() : shotCount(0) { }

    void shootLaser(SpaceShip &enemyShip)
    {
        enemyShip.applyDamage(DAMAGE_PER_SHOT);
        ++shotCount;
    }

private:
    int shotCount;
    static const float DAMAGE_PER_SHOT;
};

const float Laser::DAMAGE_PER_SHOT = 10.01f;
