;
;
;     Program written by Robert Livingston, 2022-01-27
;
;     RFL:MATRIX is a collection of matrix subroutines
;
;          Thanks to Lee Mac at http://www.lee-mac.com/mathematicalfunctions.html for hi excelent code
;
;

;; Matrix Determinant (Upper Triangular Form)  -  ElpanovEvgeniy
;; Args: m - nxn matrix

;;;(defun detm ( m / d )
;;;    (cond
;;;        (   (null m) 1)
;;;        (   (and (zerop (caar m))
;;;                 (setq d (car (vl-member-if-not (function (lambda ( a ) (zerop (car a)))) (cdr m))))
;;;            )
;;;            (detm (cons (mapcar '+ (car m) d) (cdr m)))
;;;        )
;;;        (   (zerop (caar m)) 0)
;;;        (   (*  (caar m)
;;;                (detm
;;;                    (mapcar
;;;                        (function
;;;                            (lambda ( a / d ) (setq d (/ (car a) (float (caar m))))
;;;                                (mapcar
;;;                                    (function
;;;                                        (lambda ( b c ) (- b (* c d)))
;;;                                    )
;;;                                    (cdr a) (cdar m)
;;;                                )
;;;                            )
;;;                        )
;;;                        (cdr m)
;;;                    )
;;;                )
;;;            )
;;;        )
;;;    )
;;;)

;; Matrix Determinant (Laplace Formula)  -  Lee Mac
;; Args: m - nxn matrix
 
(defun RFL:DETM ( m / i j )
    (setq i -1 j 0)
    (cond
        (   (null (cdr  m)) (caar m))
        (   (null (cddr m)) (- (* (caar m) (cadadr m)) (* (cadar m) (caadr m))))
        (   (apply '+
                (mapcar
                   '(lambda ( c ) (setq j (1+ j))
                        (* c (setq i (- i))
                             (RFL:DETM
                                (mapcar
                                   '(lambda ( x / k )
                                        (setq k 0)
                                        (vl-remove-if '(lambda ( y ) (= j (setq k (1+ k)))) x)
                                    )
                                    (cdr m)
                                )
                            )
                        )
                    )
                    (car m)
                )
            )
        )
    )
)

;; Matrix Inverse  -  gile & Lee Mac
;; Uses Gauss-Jordan Elimination to return the inverse of a non-singular nxn matrix.
;; Args: m - nxn matrix

(defun RFL:INVM ( m / c f p r )
    
    (defun f ( p m )
        (mapcar '(lambda ( x ) (mapcar '(lambda ( a b ) (- a (* (car x) b))) (cdr x) p)) m)
    )
    (setq  m (mapcar 'append m (RFL:IMAT (length m))))
    (while m
        (setq c (mapcar '(lambda ( x ) (abs (car x))) m))
        (repeat (vl-position (apply 'max c) c)
            (setq m (append (cdr m) (list (car m))))
        )
        (if (equal 0.0 (caar m) 1e-14)
            (setq m nil
                  r nil
            )
            (setq p (mapcar '(lambda ( x ) (/ (float x) (caar m))) (cdar m))
                  m (f p (cdr m))
                  r (cons p (f p r))
            )
        )
    )
    (reverse r)
)

;; Identity Matrix  -  Lee Mac
;; Args: n - matrix dimension

(defun RFL:IMAT ( n / i j l m )
    (repeat (setq i n)
        (repeat (setq j n)
            (setq l (cons (if (= i j) 1.0 0.0) l)
                  j (1- j)
            )
        )
        (setq m (cons l m)
              l nil
              i (1- i)
        )
    )
    m
)

;; Matrix Transpose  -  Doug Wilson
;; Args: m - nxn matrix

(defun RFL:TRP ( m )
    (apply 'mapcar (cons 'list m))
)

;; Matrix Trace  -  Lee Mac
;; Args: m - nxn matrix

(defun RFL:TRC ( m )
    (if m (+ (caar m) (RFL:TRC (mapcar 'cdr (cdr m)))) 0)
)

;; Matrix x Matrix  -  Vladimir Nesterovsky
;; Args: m,n - nxn matrices

(defun RFL:MXM ( m n )
    ((lambda ( a ) (mapcar '(lambda ( r ) (RFL:MXV a r)) m)) (RFL:TRP n))
)

;; Matrix + Matrix  -  Lee Mac
;; Args: m,n - nxn matrices

(defun RFL:M+M ( m n )
    (mapcar '(lambda ( r s ) (mapcar '+ r s)) m n)
)

;; Matrix x Scalar  -  Lee Mac
;; Args: m - nxn matrix, n - real scalar

(defun RFL:MXS ( m s )
    (mapcar '(lambda ( r ) (mapcar '(lambda ( n ) (* n s)) r)) m)
)

;; Matrix x Vector  -  Vladimir Nesterovsky
;; Args: m - nxn matrix, v - vector in R^n

(defun RFL:MXV ( m v )
    (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)

;; Vector x Scalar  -  Lee Mac
;; Args: v - vector in R^n, s - real scalar

(defun RFL:VXS ( v s )
    (mapcar '(lambda ( n ) (* n s)) v)
)

;; Vector Dot Product  -  Lee Mac
;; Args: u,v - vectors in R^n

(defun RFL:VXV ( u v )
    (apply '+ (mapcar '* u v))
)

;; Vector Cross Product  -  Lee Mac
;; Args: u,v - vectors in R^3

(defun RFL:V^V ( u v )
    (list
        (- (* (cadr u) (caddr v)) (* (cadr v) (caddr u)))
        (- (* (car  v) (caddr u)) (* (car  u) (caddr v)))
        (- (* (car  u) (cadr  v)) (* (car  v) (cadr  u)))
    )
)

;; Unit Vector  -  Lee Mac
;; Args: v - vector in R^2 or R^3

(defun RFL:VXL ( v )
    (   (lambda ( n ) (if (equal 0.0 n 1e-10) nil (mapcar '/ v (list n n n))))
        (distance '(0.0 0.0 0.0) v)
    )
)

;; Vector Norm (R^n)  -  Lee Mac
;; Args: v - vector in R^n

(defun RFL:|V| ( v )
    (sqrt (apply '+ (mapcar '* v v)))
)

;; Unit Vector (R^n)  -  Lee Mac
;; Args: v - vector in R^n

(defun RFL:UNIT ( v )
    ((lambda ( n ) (if (equal 0.0 n 1e-10) nil (RFL:VXS v (/ 1.0 n)))) (|v| v))
)