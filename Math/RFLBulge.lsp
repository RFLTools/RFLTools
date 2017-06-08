;
;
;     Program written by Robert Livingston, 2017-06-08
;
;
(defun RFL:BULGE (P1 P2 R / ATOTAL BULGE CHORD)
 (setq BULGE nil)
 (if (> (abs R) RFL:TOLFINE)
  (progn
   (setq CHORD (distance (list (car P1) (cadr P1)) (list (car P2) (cadr P2))))
   (setq ATOTAL (* 2 (atan (/ CHORD (* 2.0 (sqrt (- (expt R 2) (expt (/ CHORD 2.0) 2))))))))
   (setq BULGE (* (RFL:SIGN R) (RFL:TAN (/ ATOTAL 4.0))))
  )
 )
 BULGE
)
