(defstruct (transport) nb-noeuds source puits arcs-sortants arcs capacites)
(defstruct (flot (:include transport)) flots)
(defstruct (file) (tete nil) (queue nil))
(defun list->file (l)
  (make-file l (last l)))
(defun end-file (f)
  (endp (file-tete f)))
(defun file-enqueue (f x)
  (if (endp (file-tete f))
      (progn (setf (file-tete  f) (list x))
             (setf (file-queue f) (file-tete f)))
      (progn (setf (cdr (file-queue f)) (cons x nil))
             (setf (file-queue f) (cdr (file-queue f))))))

(defun file-dequeue (f)
  (prog1 (car (file-tete f))
    (setf (file-tete f) (cdr (file-tete f)))
    (when (endp (file-tete f))
      (setf (file-queue f) nil))))

(defun transport->ecart (gt)
  (loop
     with len = (* 2 (length (transport-arcs gt)))
     with as = (make-array (transport-nb-noeuds gt) :initial-element nil)
     and a = (make-array len)
     and c = (make-array len)
     and index
     and index2
     with ge = (make-transport :nb-noeuds (transport-nb-noeuds gt)
                               :source (transport-source gt)
                               :puits (transport-puits gt)
                               :arcs-sortants as
                               :arcs a
                               :capacites c)
     for arc across (transport-arcs gt)
     for cap across (transport-capacites gt)
     for i upfrom 0
     do (setq index (* 2 i))
     do (setq index2 (+ 1 index))
     do (push index (aref as (car arc)))
     do (push index2 (aref as (cdr arc)))
     do (setf (aref a index)  arc)
     do (setf (aref a index2) (cons (cdr arc) (car arc)))
     do (setf (aref c index)  cap)
     do (setf (aref c index2) 0)
     finally (return ge)))

;; TODO : kdo

(defun plus-court-chemin (gt)
  "Renvoie le plus court chemin de s à t dans un graphe d'écart.
   Le chemin est représenté par les numéros des :arcs qui le composent, du puits à la source."
  (loop named pcc
     with file = (make-file)
     and chemins = (make-array (transport-nb-noeuds gt) :element-type t :initial-element nil) ;; TODO
     and puits = (transport-puits gt)
     and noeud-fils
     for noeud = (transport-source gt) then (file-dequeue file)
     do (dolist (arcnum (aref (transport-arcs-sortants gt) noeud))
          (setq noeud-fils (cdr (aref (transport-arcs gt) arcnum)))
          (unless (or (aref chemins noeud-fils) (= 0 (aref (transport-capacites gt) arcnum)))
            (setf (aref chemins noeud-fils) (cons arcnum (aref chemins noeud)))
            (file-enqueue file noeud-fils)
            (when (eql noeud-fils puits)
              (return-from pcc (aref chemins puits)))))
     when (end-file file)
       return nil
     end))

(defun delta-sur-chemin (gt chemin)
  (loop
     for arcnum in chemin
     with capa = (transport-capacites gt)
     minimize (aref capa arcnum)))

(defun maj-ecart (ge chemin delta)
  (loop
     for arcnum in chemin
     for arcnumpair = (if (evenp arcnum) arcnum (- arcnum 1))
     do (decf (aref (transport-capacites ge) arcnumpair) delta)
     do (incf (aref (transport-capacites ge) (+ arcnumpair 1)) delta)))

(defun get-flot-max (gf)
  (loop
     for arcnum in (aref (flot-arcs-sortants gf) (transport-source gf))
     sum (aref (flot-flots gf) arcnum)))

(defun get-valeurs-flot (gt ge)
  (loop
     with len = (length (transport-arcs gt))
     with len2 = (* len 2)
     with f = (make-array len)
     with gf = (make-flot :nb-noeuds (transport-nb-noeuds gt)
                          :source (transport-source gt)
                          :puits (transport-puits gt)
                          :arcs-sortants (transport-arcs-sortants gt)
                          :arcs (transport-arcs gt)
                          :capacites (transport-capacites gt)
                          :flots f)
     for i from 0 below len
     for i2 from 1 below len2 by 2
     do (setf (aref f i) (aref (transport-capacites ge) i2))
     finally (return gf)))

(defun get-flot (gt ge)
  (let ((flot (get-valeurs-flot gt ge)))
    (cons (get-flot-max flot) flot)))

(defun edmonds-karp (gt)
  (loop
     with ge = (transport->ecart gt)
     for pcc = (plus-court-chemin ge)
     for delta = (delta-sur-chemin ge pcc)
     unless pcc
       return (get-flot gt ge)
     do (maj-ecart ge pcc delta)))

(defmacro mbuild-transport (name across/in)
  `(defun ,name (source puits arcs+capa)
     (loop
        with source = source
        and puits = puits
        and noeuds = (remove-duplicates (append (map 'list #'car arcs+capa)
                                                (map 'list #'cadr arcs+capa)))
        with nb-noeuds = (length noeuds)
        and nb-arcs = (length arcs+capa)
        with arcs-sortants = (make-array nb-noeuds :initial-element nil)
        and arcs = (make-array nb-arcs)
        and capa = (make-array nb-arcs)
        for ac ,across/in arcs+capa
        for i upfrom 0
        do (push i (aref arcs-sortants (car ac)))
        do (setf (aref arcs i) (cons (car ac) (cadr ac)))
        do (setf (aref capa i) (caddr ac))
        finally (return
                  (make-transport :nb-noeuds nb-noeuds
                                  :source source
                                  :puits puits
                                  :arcs-sortants arcs-sortants
                                  :arcs arcs
                                  :capacites capa)))))

(mbuild-transport build-transport-array across)
(mbuild-transport build-transport-list  in)

(defvar exemple-gt (build-transport-array 0 3 #((0 1 3) (0 2 2) (1 3 4) (2 3 1) (2 1 1))))
(edmonds-karp exemple-gt)

(defun liste-plus-courts-chemins (gt)
  "Renvoie la liste des plus courts chemins de s à t dans un graphe d'écart.
   Chaque chemin est représenté par les numéros des :arcs qui le composent, du puits à la source."
  (loop named pcc
     with file = (make-file)
     and chemins = (make-array (transport-nb-noeuds gt) :element-type t :initial-element nil) ;; TODO
     and puits = (transport-puits gt)
     and noeud-fils
     and retchemins = nil
     for noeud = (transport-source gt) then (file-dequeue file)
     when (eql noeud puits)
       return retchemins
     do (dolist (arcnum (aref (transport-arcs-sortants gt) noeud))
          (setq noeud-fils (cdr (aref (transport-arcs gt) arcnum)))
          (unless (= 0 (aref (transport-capacites gt) arcnum))
            (if (eql noeud-fils puits)
                (progn
                  (push (cons arcnum (aref chemins noeud)) retchemins)
                  (file-enqueue file noeud-fils))
                (unless (aref chemins noeud-fils)
                  (setf (aref chemins noeud-fils) (cons arcnum (aref chemins noeud)))
                  (file-enqueue file noeud-fils)))))
     when (end-file file)
       return nil
     end))

(defun build-graphe-exemple (n &optional (density 10) (maxcapa 10))
  (loop
     with arcs = nil
     with dejafait = (make-array n :initial-element nil)
     for x from 0 below (- n 1)
     do (loop
           for i from 0 to (random density)
           for y = (+ 1 (random (- n 2))) ;; +1 : ne pas aller vers 0 (la source)
           when (>= y x)
             do (setq y (+ y 1)) ;; Pas de boucle.
           unless (member y (aref dejafait x))
             do (push y (aref dejafait x))
             and do (push (list x y (random maxcapa)) arcs))
     finally (return (build-transport-list 0 (- n 1) arcs))))

;; (edmonds-karp (build-graphe-exemple 5 3))
(car (edmonds-karp (build-graphe-exemple 20)))
(car (edmonds-karp (build-graphe-exemple 100 10)))
(car (edmonds-karp (build-graphe-exemple 1000 10 40)))
(car (edmonds-karp (build-graphe-exemple 10000 10 100)))

(liste-plus-courts-chemins (transport->ecart exemple-gt))