(defstruct (transport) nb-noeuds source puits arcs-sortants arcs capacites)
(defstruct (flot (:include transport)) flots)
(defstruct (couche (:include transport)) present)
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

(defun transport->couche (gt)
  (let ((ge (transport->ecart gt)))
    (make-couche :nb-noeuds (transport-nb-noeuds ge)
                 :source (transport-source ge)
                 :puits (transport-puits ge)
                 :arcs-sortants (transport-arcs-sortants ge)
                 :arcs (transport-arcs ge)
                 :capacites (transport-capacites ge)
                 :present (make-array 1 :initial-element nil)))) ;; sera écrasé par liste-plus-courts-chemins

(defun plus-court-chemin (gt)
  "Renvoie le plus court chemin de s à t dans un graphe d'écart.
   Le chemin est représenté par les numéros des :arcs qui le composent, du puits à la source."
  (loop named pcc
     with file = (make-file)
     and chemins = (make-array (transport-nb-noeuds gt) :element-type t :initial-element nil)
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

(defun transport/couche->flot (gt ge/c)
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
     and capa = (if (transport-p ge/c) (transport-capacites ge/c) (couche-capacites ge/c))
     for i from 0 below len
     for i2 from 1 below len2 by 2
     do (setf (aref f i) (aref capa i2))
     finally (return gf)))

(defun get-flot (gt ge/c)
  (let ((flot (transport/couche->flot gt ge/c)))
    (cons (get-flot-max flot) flot)))

(defun edmonds-karp (gt)
  (loop
     with ge = (transport->ecart gt)
     for pcc = (plus-court-chemin ge)
     for delta = (delta-sur-chemin ge pcc)
     unless pcc
       return (get-flot gt ge)
     do (maj-ecart ge pcc delta)))

(defun nettoyer (arcs+capa)
  (loop
   with nb-noeuds = (1+ (max (loop for i in arcs+capa maximize (car i))
                             (loop for i in arcs+capa maximize (cadr i))))
   and copaing
   with arcs-sortants = (make-array nb-noeuds :initial-element nil)
   initially (loop
	      for arc in arcs+capa
	      do (push (aref arcs-sortants (car arc)) arc))
   for arc in arcs+capa
   for arcsrest = (setf (aref arcs-sortants (car arc)) (cdr (aref arcs-sortants (car arc))))
   unless (= (car arc) (cadr arc)) ;; boucle
     if (setq copaing (find (cadr arc) arcsrest ;; arcs multiples
			    :key #'cadr))
       do (incf (caddr copaing) (caddr arc))
     else if (setq copaing (find (cadr arc) (aref arcs-sortants (cadr arc))
				 :key #'identity))
       collect (list (car arc) nb-noeuds (caddr arc))
       and collect (list nb-noeuds (cadr arc) (caddr arc))
       and do (setf (car arc) nb-noeuds)
       and do (incf nb-noeuds)
     else
       collect arc
     end))

(defun build-transport-list (source puits arcs+capa)
     (setq arcs+capa (nettoyer arcs+capa))
     (loop
        with source = source
        and puits = puits
        and nb-noeuds = (1+ (max (loop for i in arcs+capa maximize (car i))
                                 (loop for i in arcs+capa maximize (cadr i))))
        and nb-arcs = (length arcs+capa)
        with arcs-sortants = (make-array nb-noeuds :initial-element nil)
        and arcs = (make-array nb-arcs)
        and capa = (make-array nb-arcs)
        for ac in arcs+capa
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
                                  :capacites capa))))

(defun build-transport-array (source puits arcs+capa)
  (build-transport-list source puits (map 'list #'identity arcs+capa)))
  
(defun liste-plus-courts-chemins (gc)
  "Modifie les arcs présents dans un graphe de couche (seuls les arcs qui font partie d'un plus court chemin de s à t sont conservés)."
  (loop named pcc
     with len = (length (couche-arcs gc))
     with file = (make-file)
     and file2 = (make-file)
     and dejavu = (make-array len :initial-element -1)
     and niveau = 0
     and puits = (couche-puits gc)
     and noeud-fils
     for noeud = (couche-source gc) then (file-dequeue file)
     initially (setf (couche-present gc) (make-array (length (couche-arcs gc)) :initial-element nil))
     initially (setf (aref dejavu (couche-source gc)) niveau)
     when (eql noeud puits)
       return gc
     do (dolist (arcnum (aref (couche-arcs-sortants gc) noeud))
          (setq noeud-fils (cdr (aref (couche-arcs gc) arcnum)))
          (unless (= 0 (aref (couche-capacites gc) arcnum))           ;; Pas les arcs saturés
            (if (= (aref dejavu noeud-fils) niveau)                   ;; Lorsqu'on a déjà vu le noeud dans cette couche
                (setf (aref (couche-present gc) arcnum) niveau)       ;;     => L'arc fait partie de la couche
                (when (= (aref dejavu noeud-fils) -1)                 ;; Lorsqu'on n'a jamais vu le noeud
                  (file-enqueue file2 noeud-fils)                     ;;     On l'ajoute à la file d'attente
                  (setf (aref dejavu noeud-fils) niveau)              ;;     Il fait partie du niveau courant
                  (setf (aref (couche-present gc) arcnum) niveau))))) ;;     Fait partie de la couche
     when (end-file file)
       do (setq file file2)
       and when (end-file file)
         return nil
       end
       and do (incf niveau)
       and do (setq file2 (make-file))
     end))

(defun maj-ecart-couche (gc)
  (let ((noeud-fils nil)
        (liste-arcs-sortants (aref (couche-arcs-sortants gc) (couche-source gc)))
        (numarc nil)
        (delta nil)
        (pile-arcs-sortants nil)
        (pile-delta nil)
        (pile-arcs nil))
    (tagbody
     loopstart
       (when (endp liste-arcs-sortants)
         (go pop))
       (setq numarc (pop liste-arcs-sortants))
       (unless (aref (couche-present gc) numarc) ;; Ne prendre en compte que les arcs qui sont dans le graphe de couche
         (go loopstart))
       (unless (> (aref (couche-capacites gc) numarc) 0) ;; Ne pas prendre en compte les arcs qu'on a saturés durant cette fonction
         (go loopstart))
       (push numarc pile-arcs)
       (setq noeud-fils (cdr (aref (couche-arcs gc) numarc)))
       ;; TODO : sortir ce if le plus haut possible, ça coûte cher à chaque itération…
       (setq delta (if delta
                       (min delta (aref (couche-capacites gc) numarc))
                       (aref (couche-capacites gc) numarc)))
       (if (eql noeud-fils (couche-puits gc))
           (progn
             (loop
                for pdelta on pile-delta
                do (decf (car pdelta) delta))
             (loop
                ;; Remonter jusqu'à la racine en faisant +/- avec delta
                for arcnum in pile-arcs
                for arcnumpair = (if (evenp arcnum) arcnum (- arcnum 1))
                do (decf (aref (couche-capacites gc) arcnumpair) delta)
                do (incf (aref (couche-capacites gc) (+ arcnumpair 1)) delta)
                ;; pop de la pile
                finally (push delta pile-delta)
                finally (push liste-arcs-sortants pile-arcs-sortants)
                finally (go pop)))
           (progn
             (push liste-arcs-sortants pile-arcs-sortants)
             (push delta pile-delta)
             ;; Récupérer la liste des arcs sortants
             (setq liste-arcs-sortants (aref (couche-arcs-sortants gc) noeud-fils))
             (go loopstart)))
     pop
       (unless (endp pile-arcs-sortants)
         (setq delta (pop pile-delta))
         (setq liste-arcs-sortants (pop pile-arcs-sortants))
         (setf pile-arcs (cdr pile-arcs))
         (go loopstart))
     end)
    gc))

(defun dinic (gt)
  (loop
     with gc = (transport->couche gt)
     for gc-pcc = (liste-plus-courts-chemins gc)
     unless gc-pcc
       return (get-flot gt gc)
     do (maj-ecart-couche gc)))

(defun build-graphe-exemple (n &optional (density 10) (maxcapa 10))
  (when (<= n 2)
    (error "build-graphe-exemple : n est trop petit !"))
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

;; (defvar exemple-gt (build-transport-array 0 3 #((0 1 3) (0 2 2) (1 3 4) (2 3 1) (2 1 1))))

;; (edmonds-karp exemple-gt)
;; ;; => 5
;; (edmonds-karp (build-graphe-exemple 5 3))
;; (car (edmonds-karp (build-graphe-exemple 20)))
;; (car (edmonds-karp (build-graphe-exemple 100 10)))
;; (car (edmonds-karp (build-graphe-exemple 1000 10 40)))
;; (car (edmonds-karp (build-graphe-exemple 10000 10 100)))

;; (dinic exemple-gt)
;; ;; => 5

(defun test-between (maxn &optional (nb-average 5) (minn 3))
  (loop
     for n from (max minn 3) to maxn
     for gts = (loop
                repeat nb-average
                collect (build-graphe-exemple n))
     for eks = (progn
		 (format t "~&ek ~a~&" n)
		 (time (loop
			for gt in gts
			collect (car (edmonds-karp gt)))))
     for ds = (progn
		(format t "~&di ~a~&" n)
		(time (loop
		       for gt in gts
		       collect (car (dinic gt)))))
     do (loop
         for gt in gts
         for ek in eks
         for d in ds
         unless (equal ek d)
           do (print gt)
           and do (error "edmonds-karp et dinic ont des résultats différents ! Le graphe : ~a et ~a pour" ek d))))
