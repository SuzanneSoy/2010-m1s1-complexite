#|
Auteur : John CHARRON
email : charron.john@gmail.com

Ce petit program a plein de défauts, je le sais, n'en parlons pas pour l'instant.
L'idée ici était de s'amuser, de faire des progrès en LISP, de faire une implémentation 
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
  - Pour que "move" marche, il faut mettre à jour à chaque "move" *max-x* et *max-y*, ce que 
  je n'arrive pas à faire pour l'instant. Ca marche si j'entre "manuellement" (move *current*)
  puis (update-max-x-y *current*) alternativement, mais je n'arrive pas à faire une fonction 
  qui fait cela automatiquement (voir la fonction "move-and-update" qui ne marche pas). *max-x* 
  et *max-y* n'est pas mis à jour à temps ! 
  - "zig-zag" est censé faire n "move-and-update" en un seul coup 
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


;;les fonctions "right" "down-left", "down", "up-right" imitent le movement des 
;; coordonnées sur un graphe mais au les coordonnées "y" positifs sont en DESSOUS du graphe
(defun right (L)
  (push
    (setf *current*
      (cons (+ 1 (first L)) (cons (+ 1 (second L)) (last L)))) *db*))

(defun down (L)
  (push
    (setf *current*
      (cons (+ 1 (first L)) (cons (second L) (cons (+ 1 (third L)) ())))) *db*))

(defun up-right (L)
    (push
      (setf *current*
        (cons (+ 1 (first L)) (cons (+ 1 (second L)) (cons (- (third L) 1) ())))) *db*))
    
(defun down-left (L)
  (push
    (setf *current*
      (cons (+ 1 (first L)) (cons (- (second L) 1) (cons (+ 1 (third L)) ())))) *db*))

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
      (print "in LEFT") 
         (down L))
    ((> *max-x* *max-y*) ;; DOWN-LEFT
      (print "in DOWN-LEFT")  
         (down-left L))
    ((< *max-x* *max-y*) ;; UP-RIGHT
      (print "in UP-RIGHT") 
         (up-right L))))

#|
Pour que "move" marche, il faut mettre à jour à chaque "move" *max-x* et *max-y*, ce que 
je n'arrive pas à faire pour l'instant. Ca marche si j'entre "manuellement" (move *current*)
puis (update-max-x-y *current*) alternativement, mais je n'arrive pas à faire une fonction 
qui fait cela automatiquement (voir la fonction "move-and-update" qui ne marche pas). *max-x* 
et *max-y* n'est pas mis à jour à temps ! |#
(defun move-and-update (L)
  (progn
    (move L)
    (update-max-x-y L)))

;; "zig-zag" est censé faire n "move-and-update" en un seul coup 
(defun zig-zag (L n)
  (if (zerop n) 
    (move-and-update L)
    (progn
      (move-and-update L)
      (zig-zag L (- n 1)))))
