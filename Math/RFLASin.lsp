;
;
;     Program written by Robert Livingston, 2014-04-30
;
;
(defun RFL:ASIN (X)
 (* 2.0 (atan (/ X (+ 1.0 (sqrt (- 1.0 (expt X 2)))))))
)
