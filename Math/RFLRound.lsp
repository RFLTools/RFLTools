;
;
;     Program written by Robert Livingston, 2014-04-30
;
;
(defun RFL:ROUND (X N / )
 (/ (float (fix (+ (* (float X) (expt 10.0 N)) (if (minusp X) -0.5 0.5)))) (expt 10.0 N))
)
