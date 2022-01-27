;
;
;     Program written by Robert Livingston, 2022-01-27
;
;     RFL:BESTVCURVE returns the best parabolic curve for the input list of XY points
;
;          Reference: https://www.efunda.com/math/leastsquares/lstsqr2dcurve.cfm
;          Note:  For the code below the form of the parabolic equation is AX2 + BX + C
;
;
(defun RFL:BESTVCURVE (PLIST / A1 A2 A3 B1 B2 B3 C1 C2 C3 G M M1 M2 MI P P0 P1 P2 P3 PA PB PLIST2 X Z1 Z2 Z3)
 (defun CALCE (M PLIST / D P YMAX)
  (setq YMAX nil)
  (foreach P PLIST
   (setq D (abs (- (cadr P) (+ (* (car M) (car P) (car P)) (* (cadr M) (car P)) (caddr M)))))
   (if (or (= nil YMAX) (> D YMAX)) (setq YMAX D))
  )
  YMAX
 )
 (if (< (length PLIST) 3)
  (eval nil)
  (progn
   (setq P0 (car PLIST))
   (setq PLIST2 (mapcar '(lambda (P)
                          (list (- (car P) (car P0))  (- (cadr P) (cadr P0)))
                         )
                        PLIST
                )
   )
   (setq A1 0.0 B1 0.0 C1 0.0 Z1 0.0 A2 0.0 B2 0.0 C2 0.0 Z2 0.0 A3 0.0 B3 0.0 C3 0.0 Z3 0.0)
   (foreach P PLIST2
    (setq A1 (+ A1 (* (car P) (car P)))
          B1 (+ B1 (car P))
          C1 (+ C1 1.0)
          Z1 (+ Z1 (cadr P))
          A2 (+ A2 (* (car P) (car P) (car P)))
          B2 A1
          C2 B1
          Z2 (+ Z2 (* (car P) (cadr P)))
          A3 (+ A3 (* (car P) (car P) (car P) (car P)))
          B3 A2
          C3 B2
          Z3 (+ Z3 (* (car P) (car P) (cadr P)))
    )
   )
   (setq M1 (list (list A1 B1 C1) (list A2 B2 C2) (list A3 B3 C3)))
   (setq M2 (list Z1 Z2 Z3))
   (setq MI (RFL:INVM M1))
   (setq M (RFL:MXV MI M2))
   (setq PA (car PLIST2))
   (setq P1 (list (+ (car P0) (car PA)) (+ (cadr P0) (* (car M) (car PA) (car PA)) (* (cadr M) (car PA)) (caddr M))))
   (setq PB (last PLIST2))
   (setq P3 (list (+ (car P0) (car PB)) (+ (cadr P0) (* (car M) (car PB) (car PB)) (* (cadr M) (car PB)) (caddr M))))
   (setq X (/ (- (car PB) (car PA)) 2.0))
   (setq G (+ (* 2.0 (car M) (car PA)) (cadr M)))
   (setq P2 (list (+ (car P1) X) (+ (cadr P1) (* G X))))
   (list P1 P2 P3 (CALCE M PLIST2))
  )
 )
)