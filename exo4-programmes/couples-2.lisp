(defun gauss-formula (n)
  (/ (* n (+ n 1)) 2))

(defun get-n (code)
  (isqrt (* code 2)))

(defun get-axis (code)
  (gauss-formula (isqrt (* code 2))))

(defun get-axis-from-n (n)
  (gauss-formula n))

(defun axis-code-compare (code)
  (format t "~d    ~d~%" code (get-axis code))) 

(defun axis-code-compare-n (startcode n)
  (let ((i 0))
    (loop
      (when (> i n) (return))
      (axis-code-compare (+ startcode i))
      (incf i))))
  
(defun ord-pr-to-code (x y)
  (let ((sumxy (+ x y)))
    (if (evenp sumxy)
      (+ (gauss-formula sumxy) x)
      (+ (gauss-formula sumxy) y))))

(defun code-to-ord-pr (code)
  (let ((n (get-n code))
        (axis (get-axis code))
        (diff 0))
  (progn
    (when (> (get-axis code) code)
      (progn
        (setf n (- n 1))
        (setf axis (get-axis-from-n n))))
    (setf diff (- code axis))
    (if (evenp n)
      (cons diff (cons (- n diff) ()))
      (cons (- n diff) (cons diff ()))))))

(defun ord-mult-to-code (L)
  (if (= (list-length L) 1)
    (car L)
    (ord-mult-to-code (append (butlast (butlast L)) 
                        (cons (ord-pr-to-code (car (last (butlast L))) (car (last L))) ())))))

(defun code-to-ord-mult (L-or-code size)
  (if (atom L-or-code)
       (code-to-ord-mult (code-to-ord-pr L-or-code) (- size 1))
    (if (not (= size 1))
        (code-to-ord-mult (append (butlast L-or-code) (code-to-ord-pr (car (last L-or-code))))
          (- size 1))
      L-or-code)))

#| Les codes générés par cette fonction ne correspondent pas au code généré par le
diagramme du rapport ni des fonctions ord-mult-to-code et code-to-ord-mult. 
Toutefois, la fonction ci-dessous a été créées ici car son écriture et beaucoup plus idiomatique
en LISP (d'où le nom 'ord-mult-to-code-lisp'). En effet, si on avait à coder les nombres naturels en LISP, on ajouterait 
(resp. supprimerait) des éléments de la liste en partant du début de la liste afin de créer
une paire ou un n-uplet (resp. pour trouver le code correspondant à une paire ou un n-uplet
|#
(defun ord-mult-to-code-lisp (L)
  (if (= (list-length L) 1)
    (car L)
    (ord-mult-to-code-lisp (cons (ord-pr-to-code (first L) (second L)) (cddr L)))))

#| voir le commentaire précédent concernant la fonction ord-mult-to-code-lisp |#
(defun code-to-ord-mult-lisp (L-or-code size)
  (if (atom L-or-code)
    (code-to-ord-mult-lisp (code-to-ord-pr L-or-code) (- size 1))
    (if (not (= size 1))
      (code-to-ord-mult-lisp (append (code-to-ord-pr (car L-or-code)) (cdr L-or-code)) (- size 1))
      L-or-code)))

#|
(defun code-to-ord-pr (code)
  (;let* ((code*2 (* code 2))
         (n (isqrt code*2))
         (axis (gauss-formula n))
         (diff 0))
    (cond 
      ((> axis code)
         (loop while (> axis code)
           ((setf n (- n 1)) 
           (setf axis (gauss-formula n))))
       (< axis code)
          ((loop while (< axis code)
             ((setf n (- n 1)) 
             (setf axis (gauss-formula n))))
           (when (> axis code)
             ((setf n (- n 1))
             (setf axis (gauss-formula n))))))
        (t 5))))
  |#           
  

(defun loop-test (n)
  (let ((n 0))
    (loop
      (when (> n 10) (return))
      (format t "~d ~d ~d~%" n (isqrt n) (gauss-formula n))
      ;(print n) (write (* n n)) (write n)
      (incf n))))

