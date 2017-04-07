;
;
;     Program written by Robert Livingston, 2013-05-01
;
;     RFL:INSWING Calculates the inswing for radius R and wheelbase D
;
;
(defun RFL:INSWING (D R)
 (if (< R RFL:TOL)
  0.0
  (- R (sqrt (- (expt R 2.0) (expt (/ D 2.0) 2.0))))
 )
)