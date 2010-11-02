#ifndef GRAPHEECART_H_INCLUDED
#define GRAPHEECART_H_INCLUDED

#include "Graphe.h"

class GrapheEcart : public Graphe
{
    private :


    public :
    GrapheEcart(string nom);
    GrapheEcart(string,const Graphe*);
    ~GrapheEcart();

    void miseAJour(listeArcs_t chemin, int k);
};

#endif // GRAPHEECART_H_INCLUDED
