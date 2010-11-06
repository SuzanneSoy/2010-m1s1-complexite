#|
Auteur : John CHARRON
email : charron.john@gmail.com

Ce petit program a plein de défauts, je le sais, n'en parlons pas pour l'instant.
L'idée ici était de m'amuser, de faire des progrès en LISP, de faire une implémentation 
d'une question de complexité : le programme sera amélioré par la suite (meilleurs moyens 
de récupérer des données (plus efficace), etc.), il ne s'agit qu'un début.
L'idée ici est de générer des couples avec une clé :
  - *current* est la liste courante (clé x y)
  - *db* est la base de données, les valeurs générées sont stockées dans *db* 
  (inefficace, je sais, car il faudrait pour l'instant faire un parcours séquentiel pour
  retrouver la donnée... j'améliorera cela par la suite, laisser pour l'instant)
  - les fonctions "right" "down-left", "down", "up-right" imitent le movement des 
  coordonnées sur un graphe mais au les coordonnées "y" positifs sont en DESSOUS du graphe
  - "move" s'occupe de choisir "right", "down-left" etc. selon les valeurs dans *current*
  - Pour que "move" marche, il faut mettre à jour à chaque "move" *max-x* et *max-y* (ici à l'aide
  de la fonction "update-max-x-y"
  - "zig-zag" fait n "move-and-update" en un seul coup et affiche le contenu de *db* (toutes les couples)
|#
                                                                                


;; définition des variables globales (toujours entre astérisques)
(defvar *current* (list 0 0 0)) ;; liste courante (clé x y)
(setf *current* (list 0 0 0)) 
(defvar *db* nil) ;; base de données qui stocke tous les "(clé x y)"
(setf *db* nil)
(push *current* *db*)

(defvar *max-x* 0) ;; valeur maximal de x jusque "là"
(setf *max-x* 0)
(defvar *max-y* 0) ;; valeur maximal de y jusque "là"
(setf *max-y* 0)

#| pour remettre toutes les variables globales à leur valeurs par défaut 
afin de tester, de refaire un 'zig-zag', etc.
|#
(defun reset ()
  (progn
    (defvar *current* (list 0 0 0)) ;; liste courante (clé x y)
    (setf *current* (list 0 0 0))  
    (defvar *db* nil) ;; base de données qui stocke tous les "(clé x y)"
    (setf *db* nil)
    (push *current* *db*)
    (defvar *max-x* 0) ;; valeur maximal de x jusque "là"
    (setf *max-x* 0)
    (defvar *max-y* 0) ;; valeur maximal de y jusque "là"
    (setf *max-y* 0)
    *current*))
  
#| Les fonctions "right" "down-left", "down", "up-right" imitent le movement des 
coordonnées sur un graphe mais au les coordonnées "y" positifs sont en DESSOUS du graphe
|#
(defun right (L)
  (progn
    (push
      (setf *current*
        (cons (+ 1 (first L)) (cons (+ 1 (second L)) (last L)))) *db*)
    *current*))
  
(defun down (L)
  (progn
    (push
      (setf *current*
        (cons (+ 1 (first L)) (cons (second L) (cons (+ 1 (third L)) ())))) *db*)
     *current*))

(defun up-right (L)
  (progn
    (push
      (setf *current*
        (cons (+ 1 (first L)) (cons (+ 1 (second L)) (cons (- (third L) 1) ())))) *db*)
    *current*))
    
(defun down-left (L)
  (progn
	(push
	  (setf *current*
	    (cons (+ 1 (first L)) (cons (- (second L) 1) (cons (+ 1 (third L)) ())))) *db*)
    *current*))

(defun update-max-x (L)
  (if (> (second L) *max-x*)
    (setf *max-x* (second L))
    nil))

(defun update-max-y (L)
  (if (> (third L) *max-y*)
    (setf *max-y* (third L))
    nil))

(defun update-max-x-y (L)
  (cond
    ((> (second L) *max-x*)
      (setf *max-x* (second L)))
    ((> (third L) *max-y*)
      (setf *max-y* (third L)))
    (t ())))

;; "move" s'occupe de choisir "right", "down-left" etc. selon les valeurs dans *current*
(defun move (L)
  (cond
    ((and (zerop (third L)) (= *max-x* *max-y*)) ;; RIGHT takes precedence over LEFT becuase it occurs first
      (print "in RIGHT") ;; 
         (right L))
    ((and (zerop (second L)) (= *max-x* *max-y*)) ;; DOWN
      (print "in DOWN") 
         (down L))
    ((> *max-x* *max-y*) ;; DOWN-LEFT
      (print "in DOWN-LEFT")  
         (down-left L))
    ((< *max-x* *max-y*) ;; UP-RIGHT
      (print "in UP-RIGHT") 
         (up-right L))))

#|
On fait un "move" et puis un "update-max-x-y"
Attention : il faut bien faire un setf L, sinon, le paramètre L de "update-max-x-y utilise la valeur
de L inchangé !
|#
(defun move-and-update (L)
  (progn
    (setf L (move L))
    (update-max-x-y L)
    *db*))

;; "zig-zag" fait n "move-and-update" en un seul coup et affiche le contenu de *db* (toutes les couples) 
(defun zig-zag (L n)
  (if (zerop n) 
    (move-and-update *current*)
    (progn
      (move-and-update *current*)
      (zig-zag L (- n 1)))))

