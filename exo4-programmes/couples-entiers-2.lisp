#|
Auteur : John CHARRON
email : charron.john@gmail.com
|#
                                                                                
;; définition des variables globales (toujours entre astérisques)
(defvar *current* (list 0 0 0)) ;; liste courante (clé x y)
(setf *current* (list 0 0 0)) 
(defvar *db* nil) ;; base de données qui stocke tous les "(clé x y)"
(setf *db* nil)
(push *current* *db*)

(defvar *max-x* 0) ;; valeur maximale de x jusque atteinte
(setf *max-x* 0)
(defvar *max-y* 0) ;; valeur maximale de y jusque atteinte
(setf *max-y* 0)
(defvar *min-x* 0) ;; valeur minimale de x atteinte
(setf *min-x* 0) 
(defvar *min-y* 0) ;; valeur minimale de y atteinte
(setf *min-y* 0) 

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
    (defvar *min-x* 0) ;; valeur minimale de x atteinte
    (setf *min-x* 0)
    (defvar *min-y* 0) ;; valeur minimale de y atteinte
    (setf *min-y* 0)
    *current*))
  
#| Les fonctions "right" "down", "down", "up" imitent le movement des 
coordonnées sur un graphe mais au les coordonnées "y" positifs sont en DESSOUS du graphe
|#

(defun right (L)
  (progn
    (push
      (setf *current*
        (cons (+ 1 (first L)) (cons (+ 1 (second L)) (last L)))) *db*)
    *current*))
  
(defun left (L)
  (progn
    (push
      (setf *current*
        (cons (+ 1 (first L)) (cons (- (second L) 1) (last L)))) *db*)
    *current*))

(defun up (L)
  (progn
    (push
      (setf *current*
        (cons (+ 1 (first L)) (cons (second L) (cons (+ (third L) 1) ())))) *db*)
    *current*))
    
(defun down (L)
  (progn
	(push
	  (setf *current*
	    (cons (+ 1 (first L)) (cons (second L) (cons (- (third L) 1) ())))) *db*)
    *current*))

(defun loop-right (L)
  (loop
      (when (= (second L) (+ (- *min-x*) 1)) (*current*))
	(progn
	  (push
	    (setf *current*
	      (cons (+ 1 (first L)) (cons (+ 1 (second L)) (last L)))) *db*)
    (update-all L))))
  
(defun loop-left (L)
  (loop
    (when (= (second L) (- *max-x*)) (return))
      (progn
        (push
          (setf *current*
            (cons (+ 1 (first L)) (cons (- (second L) 1) (last L)))) *db*)
        (update-all L)
      *current*)))

(defun loop-up (L)
    (loop
      (when (= (third L) (+ (- *min-y*) 1)) (return))
        (progn
          (push
            (setf *current*
            (cons (+ 1 (first L)) (cons (second L) (cons (+ (third L) 1) ())))) *db*)
          (update-all L)
        *current*)))
    
(defun loop-down (L)
  (loop
    (when (= (third L) (- *max-y*)) (return))
      (progn  
    	(push
	    (setf *current*
	      (cons (+ 1 (first L)) (cons (second L) (cons (- (third L) 1) ())))) *db*)
        (update-all L)
      *current*)))




(defun update-max-x (L)
  (if (> (second L) *max-x*)
    (setf *max-x* (second L))
    nil))

(defun update-max-y (L)
  (if (> (third L) *max-y*)
    (setf *max-y* (third L))
    nil))

(defun update-min-x (L)
  (if (< (second L) *min-x*)
    (setf *min-x* (second L))
    nil))

(defun update-min-y (L)
  (if (> (third L) *max-y*)
    (setf *min-y* (third L))
    nil))

(defun print-all ()
  (print "*current*")
  (print *current*)
  (print "*max-x*: ~S")
  (print *max-x*))

(defun update-all (L)
  (cond
   ((> (second L) *max-x*)
     (setf *max-x* (second L)))
    ((> (third L) *max-y*)
      (setf *max-y* (third L)))
    ((< (second L) *min-x*)
      (setf *min-x* (second L)))
    ((< (third L) *min-y*)
      (setf *min-y* (third L)))  
    (t ())))

;; "move" s'occupe de choisir "right", "down-left" etc. selon les valeurs dans *current*
(defun move (L)
  (cond
    ((and (= (* (isqrt (first L)) (+ (isqrt (first L)) 1)) (first L)) (evenp (isqrt (first L))))
      (print "in RIGHT")
      (loop-right L))
    ((and (integerp (sqrt (first L))) (oddp (first L)))
      (print "in UP")
      (loop-up L))
    ((and (= (* (isqrt (first L)) (+ (isqrt (first L)) 1)) (first L)) (oddp (isqrt (first L))))
      (print "in LEFT")
      (loop-left L)) 
    ((and (integerp (sqrt (first L))) (evenp (first L)))
      (print "in DOWN")
      (loop-down L))   
    (t *current*)
    ))


#|
On fait un "move" et puis un "update-max-x-y"
Attention : il faut bien faire un setf L, sinon, le paramètre L de "update-max-x-y utilise la valeur
de L inchangé !
|#
(defun move-and-update (L)
  (progn
    (setf L (move L))
    *db*))

;; "zig-zag" fait n "move-and-update" en un seul coup et affiche le contenu de *db* (toutes les couples) 
(defun zig-zag (L n)
    (if (zerop n) 
      (move-and-update *current*)
      (progn
        (move-and-update *current*)
        (zig-zag L (- n 1)))))
