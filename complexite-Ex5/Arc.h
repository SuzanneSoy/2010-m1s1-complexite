#ifndef ARC_H_INCLUDED
#define ARC_H_INCLUDED

#include <iostream>
#include <vector>

using namespace std;

class Arc
{
    private :
    unsigned int a;                         // Somment de d�part
    unsigned int b;                         // Sommet d'arriv�.
    unsigned int capacite;                  // Capacit� de l'arc.
    unsigned int flot;                      // Flot circulant dans l'arc.
    bool arcRetour;                         // Vrai si arc retour, faux sinon

    public :
    Arc(unsigned int a, unsigned int b, unsigned int c, unsigned int f);
    Arc(const Arc*);
    ~Arc();
    void setS1(unsigned int val);
    unsigned int getS1() const;
    void setS2(unsigned int val);
    unsigned int getS2() const;
    void setFlot(unsigned int val);
    unsigned int getFlot() const;
    void setCapacite(unsigned int val);
    unsigned int getCapacite() const;
    bool getArcRetour();
    void setArcRetour(bool);
    void afficheArc();

};

// Liste d'arcs sans classement par sommets.
typedef vector<Arc*> listeArcs_t;

#endif // ARC_H_INCLUDED
