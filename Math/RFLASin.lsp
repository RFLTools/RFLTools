;
;
;     Program written by Robert Livingston, 2014-04-30
;
;
(defun RFL:ASIN (X)
 (- (/ pi 2.0) (/ 1.0 (sqrt (+ 1.0 (expt (atan X) 2.0)))))
)
