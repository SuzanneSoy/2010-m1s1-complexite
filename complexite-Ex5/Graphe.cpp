#include "Graphe.H"
#include "GRapheEcart.h"

/* Constructeur par d�faut.
*/
Graphe::Graphe(string nom)
{
    this->nom = nom;
}

/* Constructeur par copie d'un graphe.
*/
Graphe::Graphe(string nom, const Graphe *g)
{
    this->s = g->s;
    this->p = g->p;
    listeArcs_t lArcs;
    this->nom = nom;

    arcs_t arcsTmp = g->arcs;

    for(unsigned int i = 0; i < arcsTmp.size(); i++)
    {
        lArcs.clear();

        for(unsigned int j = 0; j < arcsTmp[i].size(); j++)
            lArcs.push_back(new Arc(arcsTmp[i][j]));

        this->arcs.push_back(lArcs);
    }
}

Graphe::~Graphe()
{

}

arcs_t Graphe::getArcs() const { return this->arcs; };


 //------- Fonctions du TP --------//
//================================//

/* Q1 | Retourne le graphe d'�cart associ� pour un flot nul.
* Prerequis : le graphe doit �tre orient� et ne pas contenir d'ars retour. TODO revoir l'histoire de pas d'arcs retour.
* Seconde partie de la fonction dans le constructeur de la classe GrapheEcart*/
GrapheEcart* Graphe::grapheEcartFlotNul() const
{
    string nom;                                                             // Nom du graphe.

    nom = "Graphe d'�cart du graphe : " + this->nom;
    GrapheEcart *gEcart = new GrapheEcart(nom,this);

    return gEcart;
}


/* Q2 | D�termineur un plus court chemin entre les sommets s et p.
* Prerequis : le graphe ne doit pas �tre vide.*/
listeArcs_t Graphe::PCCsp(unsigned int s, unsigned int p) const
{
    listeArcs_t pcc;
    listeArcs_t arcsParcourus;
    vector<unsigned int> file;
    vector<bool> sVu;                                                       // Sommets d�j� vu.
    unsigned int sommet;

    if(s == p)
        return pcc;

    // Initialisation du vecteur de sommets d�j� vus..
    for(unsigned int i = 0; i < this->arcs.size(); i++)
        sVu.push_back(false);

    // Mise dans la file du sommet de d�part.
    file.push_back(s);

    while(file.size() > 0)
    {
        sommet = file[0];

        for(unsigned int i = 0; i < this->arcs[sommet].size(); i++)
            if(this->arcs[sommet][i]->getS2() == p)
            {
                arcsParcourus.push_back(this->arcs[sommet][i]);
                unCheminSaP(s,p,arcsParcourus,pcc);
                return pcc;
            }
            else
                if(sVu[this->arcs[sommet][i]->getS2()] == false)
                {
                    file.push_back(this->arcs[sommet][i]->getS2());
                    arcsParcourus.push_back(this->arcs[sommet][i]);
                    sVu[this->arcs[sommet][i]->getS2()] = true;
                }

        file.erase(file.begin());
    }

    return pcc;
}


/* Q3 | Donne la plus petite valuation (capacit�) sur le chemin.
* Prerequis : Le chemin doit contenir au moins un arc.*/
unsigned int Graphe::capaciteMinDuChemin(listeArcs_t chemin) const
{
    unsigned int c = chemin[0]->getCapacite();

    for(unsigned int i = 1; i < chemin.size(); i++)
        c = min(c,chemin[i]->getCapacite());

    return c;
}
//================================//




/* Ajoute une arc au graphe
* L'arc est palc�e en fonction de son sommets de d�part puis de celui d'arriv�
*/
bool Graphe::ajouteArc(Arc *arc)
{
    // Si les deux sommets de l'arc appartiennent au graphe.
    if(arc->getS1() <= (this->arcs.size() -1) && arc->getS2() <= (this->arcs.size() -1))
    {
        // On place l'arc dans la liste correspondante au sommet de d�part
        // et on d�termine la position da l'arc dans cette liste selon le sommet d'arriv�.
        for(unsigned int j = 0; j < this->arcs[arc->getS1()].size(); j++)
            if(this->arcs[arc->getS1()][j]->getS2() > arc->getS2())
            {
                this->arcs[arc->getS1()].insert(this->arcs[arc->getS1()].begin()+j,arc);
                return true;
            }

        this->arcs[arc->getS1()].push_back(arc);
    }
    else
        return false;                                                       // Le ne peut �tre ajout�.

    return true;
}


/* Charge un graphe fixe quelconque permettant de tester les diff�rentes fonctions
*/
void Graphe::chargeG1()
{
    /* Un graphe constitu� de 4 sommets et 3 arr�tes */
    // TODO Ajouter des sommets et des arr�tes afin de pouvoir tester les diff�rents cas limite.

    listeArcs_t lArcs;                                                      // Liste d'arcs.

    // Cr�ation de la liste d'arcs sortants pour le sommet 0.
    Arc *arc = new Arc(0,1,1,0);
    lArcs.push_back(arc);

    Arc *arc2 = new Arc(0,2,5,0);
    lArcs.push_back(arc2);

    arcs.push_back(lArcs);

    // Cr�ation de la liste d'arcs sortants pour le sommet 1.
    lArcs.clear();

    Arc *arc3 = new Arc(1,3,3,2);
    lArcs.push_back(arc3);

    arcs.push_back(lArcs);

    // Cr�ation de la liste d'arcs sortants pour le sommet 2 et 3.
    lArcs.clear();

    arcs.push_back(lArcs);
    arcs.push_back(lArcs);
}


/* Affichage du graphe.
*/
void Graphe::afficheGraphe() const
{
    cout << " - " << nom << endl;

    for(unsigned int i = 0; i < arcs.size(); i++)                           // Pour chaques sommets.
        for(unsigned int j = 0; j < arcs[i].size(); j++)                    // Pour chaque arcs.
            this->arcs[i][j]->afficheArc();                                 // Dessin de l'arc.
}

bool Graphe::estDansListe(Arc *arc, listeArcs_t lArcs) const
{
    for(unsigned int i = 0; i < lArcs.size(); i++)
        if(lArcs[i] == arc)
            return true;

    return false;
}

void Graphe::unCheminSaP(unsigned int s, unsigned int p, listeArcs_t l, listeArcs_t &pcc) const
{
    // Condition d'arr�t de la r�cursion.
    if(s == p)
        return;

    for(unsigned int i = 0; i < l.size(); i++)
        if(l[i]->getS2() == p)
        {
            pcc.insert(pcc.begin(),l[i]);
            unCheminSaP(s,l[i]->getS1(),l,pcc);
            return;
        }
}

Arc* Graphe::arcInverse(Arc *arc)
{
    for(unsigned int s = 0; s < this->arcs[arc->getS2()].size(); s++)
        if(this->arcs[arc->getS2()][s]->getS2() == arc->getS1())
            return this->arcs[arc->getS2()][s];

    return NULL;
}








