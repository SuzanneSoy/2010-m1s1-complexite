#ifndef GRAPHE_H_INCLUDED
#define GRAPHE_H_INCLUDED

#include <iostream>

#include "Arc.h"

class GrapheEcart;

// Définition du type de donnée : "liste de sommets et arcs".
typedef vector<listeArcs_t> arcs_t;

class Graphe
{
    protected :
    unsigned int s;                                 // La source.
    unsigned int p;                                 // Le puit.
    arcs_t arcs;                                    // Les sommets et arrêtes associées.
    string nom;                                     // Nom du graphe.


    public :
    Graphe(string nom);                             // Constructeur par défaut.
    Graphe(string nom,const Graphe*);               // Constructeur par copie.
    ~Graphe();                                      // Destructeur de l'objet.

    void chargeG1();                                // Charge le graphe G1.
    bool ajouteArc(Arc*);                           // Ajoute un arc au graphe.
    bool contientArc(Arc*) const;                   // Appartenance de l'arc au graphe en terme de pointeurs.
    void afficheGraphe() const;                     // Affiche le graphe.

    // Accesseurs.
    arcs_t getArcs() const;

    // Fonctions répondants à l'énoncé.
    GrapheEcart* grapheEcartFlotNul() const;                                // Donne le graphe d'écart pour un flot nul.
    listeArcs_t PCCsp(unsigned int s, unsigned int p) const;                // Le plus court chemin entre s et p.
    unsigned int capaciteMinDuChemin(listeArcs_t chemin) const;             // La plus petite valuation du chemin.
    int miseAJour(GrapheEcart*);

    protected :
    bool estDansListe(Arc*, listeArcs_t) const;
    void unCheminSaP(unsigned int s, unsigned int p, listeArcs_t l, listeArcs_t &pcc) const;
    Arc* arcInverse(Arc*);
    Arc* arcDansGraphe(Arc*);                                               // Retourne l'arc du graphe correspondant (par rappert au sommets).
};

#endif // GRAPHE_H_INCLUDED
