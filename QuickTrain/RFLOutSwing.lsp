;
;
;     Program written by Robert Livingston, 2013-05-01
;
;     RFL:OUTSWING Calculates the outswing for radius R and wheelbase D and overhang DR
;
;
(defun RFL:OUTSWING (D DR R)
 (if (< R RFL:TOL)
  0.0
  (- (sqrt (+ (- (expt R 2.0) (expt (/ D 2.0) 2.0)) (expt (+ (/ D 2.0) DR) 2.0))) R)
 )
)
