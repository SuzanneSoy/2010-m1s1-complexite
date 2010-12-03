#include "Arc.h"

Arc::Arc(unsigned int a, unsigned int b, unsigned int c, unsigned int f)
{
    this->a = a;
    this->b = b;
    this->capacite = c;
    this->flot = f;
}

Arc::Arc(const Arc *arc)
{
    this->a = arc->a;
    this->b = arc->b;
    this->capacite = arc->capacite;
    this->flot = arc->flot;
}

void Arc::setS1(unsigned int val)
{
    this->a = val;
}

unsigned int Arc::getS1() const
{
    return this->a;
}

void Arc::setS2(unsigned int val)
{
    this->b = val;
}

unsigned int Arc::getS2() const
{
    return this->b;
}

void Arc::setFlot(unsigned int val)
{
    this->flot = val;
}

unsigned int Arc::getFlot() const
{
    return this->flot;
}

void Arc::setCapacite(unsigned int val)
{
    this->capacite = val;
}

unsigned int Arc::getCapacite() const
{
    return this->capacite;
}

bool Arc::getArcRetour()
{
    return this->arcRetour;
}

void Arc::setArcRetour(bool v)
{
    this->arcRetour = v;
}

void Arc::afficheArc()
{
    cout << " " << this->a << "  " << this->b << "     c : "            // Affichage des noms des sommets.
        << this->capacite << "    f : " << this->flot << endl;          // Affichage de la capacité et du flot.
}
