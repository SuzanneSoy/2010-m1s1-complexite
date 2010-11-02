#include <iostream>
#include <vector>
#include "Graphe.h"
#include "GrapheEcart.h"

using namespace std;

void afficheListeArcs(listeArcs_t l);

int main()
{
    Graphe *g1 = new Graphe("G1");              // Un graphe quelconque g1.
    GrapheEcart *gEcart;                        // Un graphe d'écart.
    listeArcs_t pcc;                            // Une liste d'arcs pour le plus court chemin.


    g1->chargeG1();                             // Initialisation de g1.
    gEcart = g1->grapheEcartFlotNul();          // Calcule le graphe d'écart pour le graphe.
    pcc = g1->PCCsp(0,3);                       // Calcule le plus court chemin.


    g1->afficheGraphe();                        // Affichage du graphe.
    cout << endl << endl;
    gEcart->afficheGraphe();                    // Affichage du graphe d'écart.
    cout << endl << endl;
    afficheListeArcs(pcc);                      // Affichage du chemin.

    return 0;
}

void afficheListeArcs(listeArcs_t l)
{
    for(unsigned int i = 0; i < l.size(); i++)
        l[i]->afficheArc();
}








