;
;
;     Program written by Robert Livingston, 2013-05-01
;
;     RFL:GETTRUCKANGS calculates the truck rotation angles about 3 axis
;
;
(if RFL:GETTRUCKANGS (princ "\nRFL:GETTRUCKANGS already loaded...")
(defun RFL:GETTRUCKANGS (STA1 STA2 GAUGE / ASIN ACOS P1 P2 RX RY RZ S S1 S2 STA2 TOL WB)
 (defun ACOS (X)
  (* 2.0 (atan (/ (sqrt (- 1.0 (expt X 2))) (+ 1.0 X))))
 )
 (defun ASIN (X)
  (* 2.0 (atan (/ X (+ 1.0 (sqrt (- 1.0 (expt X 2)))))))
 )
 (if (or (= nil (setq P1 (RFL:POINTATSTATION STA1)))
         (= nil (setq P2 (RFL:POINTATSTATION STA2))))
  nil
  (progn
   (if (= nil (setq S1 (RFL:SUPER STA1)))
    (setq S1 (list 0.0 0.0))
   )
   (if (= nil (setq S2 (RFL:SUPER STA2)))
    (setq S2 (list 0.0 0.0))
   )
   (setq S (/ (+ (* -1.0 (car S1)) (cadr S1) (* -1.0 (car S2)) (cadr S2)) 4.0))
   (setq RX (* -1.0 (ASIN (/ S (/ GAUGE 2.0)))))
   (setq RY (* -1.0 (atan (/ (- (caddr P2) (caddr P1)) (distance (list (car P1) (cadr P1)) (list (car P2) (cadr P2)))))))
   (setq RZ (angle (list (car P1) (cadr P1)) (list (car P2) (cadr P2))))
   (list RX RY RZ)
  )
 )
)
)
