;
;
;     Program written by Robert Livingston, 2014-04-30
;
;
(defun RFL:ACOS (X)
 (* 2.0 (atan (/ (sqrt (- 1.0 (expt X 2))) (+ 1.0 X))))
)
