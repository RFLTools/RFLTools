;
;
;     Program written by Robert Livingston, 2013-05-01
;
;     RFL:GETCARANGS calculates the car rotation angles about 3 axis
;
;
(if RFL:GETCARANGS (princ "\nRFL:GETCARANGS already loaded...")
(defun RFL:GETCARANGS (P1 P2 GAUGE / RX RY RZ S S1 S2 STA1 STA2 TOL WB)
 (if (or (= nil (setq STA1 (car (RFL:STAOFF P1))))
         (= nil (setq STA2 (car (RFL:STAOFF P2)))))
  nil
  (progn
   (if (= nil (setq S1 (RFL:SUPER STA1)))
    (setq S1 (list 0.0 0.0))
   )
   (if (= nil (setq S2 (RFL:SUPER STA2)))
    (setq S2 (list 0.0 0.0))
   )
   (setq S (/ (+ (* -1.0 (car S1)) (cadr S1) (* -1.0 (car S2)) (cadr S2)) 4.0))
   (setq RX (* -1.0 (RFL:ASIN (/ S (/ GAUGE 2.0)))))
   (setq RY (* -1.0 (atan (/ (- (caddr P2) (caddr P1)) (distance (list (car P1) (cadr P1)) (list (car P2) (cadr P2)))))))
   (setq RZ (angle (list (car P1) (cadr P1)) (list (car P2) (cadr P2))))
   (list RX RY RZ)
  )
 )
)
)
