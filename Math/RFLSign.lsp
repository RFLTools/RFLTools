;
;
;     Program written by Robert Livingston, 2016-05-26
;
;
(defun RFL:SIGN (X)
 (if (< X 0)
  (eval -1)
  (eval 1)
 )
)
