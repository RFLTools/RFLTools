;
;
;     Program written by Robert Livingston 2019-08-19
;
;     Returns the elevation for a point at P as defined by 3 points P1, P2, P3
;     (RFL:PLANEZ P P1 P2 P3)
;
;     A(X-X0) + B(Y-Y0) + C(Z-Z0) = 0, Z = [CZ0 - A(X-X0) - B(Y-Y0)] / C
;
;     Ref: https://www.youtube.com/watch?v=0qYJfKG-3l8
;
(defun RFL:PLANEZ (P P1 P2 P3 / A B C V1 V2)
 (setq V1 (list (- (car P2) (car P1)) (- (cadr P2) (cadr P1)) (- (caddr P2) (caddr P1)))) ; Vector from P1 to P2
 (setq V2 (list (- (car P3) (car P1)) (- (cadr P3) (cadr P1)) (- (caddr P3) (caddr P1)))) ; Vector from P1 to P3
 
 (setq A (- (* (cadr V1) (caddr V2)) (* (caddr V1) (cadr V2))))
 (setq B (* -1.0 (- (* (car V1) (caddr V2)) (* (caddr V1) (car V2)))))
 (setq C (- (* (car V1) (cadr V2)) (* (cadr V1) (car V2))))
 
 (if (< (abs C) RFL:TOLFINE)
  nil
  (/ (- (* C (caddr P1)) (* A (- (car P) (car P1))) (* B (- (cadr P) (cadr P1)))) C)
 )
)