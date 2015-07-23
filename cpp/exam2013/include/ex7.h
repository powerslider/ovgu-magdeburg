#pragma once

class Laser;

class SpaceShip
{
public:
    SpaceShip(float startHealth, const Laser& laser)
            : totalHealth(startHealth), health(startHealth), laser(laser)
    {
    }

    void applyDamage(float damage)
    {
        health -= damage;
        if (health <= 0) throw "BIG EXPLOSION";
    }

    float getRelativeHealth() const
    {
        return health / totalHealth;
    }

    const Laser& getLaser() const { return laser; }

private:
    const float totalHealth;
    float health;
    const Laser& laser;
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

const float Laser::DAMAGE_PER_SHOT = 90.01f;
