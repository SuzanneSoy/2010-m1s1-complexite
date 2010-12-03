#include "GrapheEcart.h"

GrapheEcart::GrapheEcart(string nom) : Graphe(nom)
{

}

 //------- Fonctions du TP --------//
//================================//

/* Q1 | Retourne le graphe d'écart associé pour un flot nul.
* Prerequis : le graphe doit être orienté et ne pas contenir d'ars retour. TODO revoir l'histoire de pas d'arcs retour.*/
GrapheEcart::GrapheEcart(string nom, const Graphe *g) :  Graphe(nom,g)
{
    Arc *arcInv;                                                            // Arc inverse.
    Arc *arc;                                                               // Arc courrant.

    for(unsigned int i = 0; i < g->getArcs().size(); i++)
        for(unsigned int j = 0; j < g->getArcs()[i].size(); j++)
            this->arcs[i][j]->setFlot(0);

    for(unsigned int i = 0; i < g->getArcs().size(); i++)
        for(unsigned int j = 0; j < g->getArcs()[i].size(); j++)
        {
            arc = g->getArcs()[i][j];

            arcInv = new Arc(arc->getS2(),arc->getS1(),0,0);
            arcInv->setArcRetour(true);

            this->ajouteArc(arcInv);
        }
}



/* Q4 | Met à jour la chaine augmentante du graphe correspondant au chemin pour une augment de k du flot.
*/
// TODO A tester !!!
void GrapheEcart::miseAJour(listeArcs_t l,int k)
{
    Arc *arcInv;

    for(unsigned int i = 0; i < l.size(); i++)
    {
        l[i]->setCapacite(l[i]->getCapacite()-k);
        arcInv = this->arcInverse(l[i]);
        arcInv->setCapacite(arcInv->getCapacite()+k);
    }
}

//================================//

arcs_t GrapheEcart::getListeArcs()
{
    return this->arcs;
}
