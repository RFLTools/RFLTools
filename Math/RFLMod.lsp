;
;
;     Program written by Robert Livingston, 2014-04-30
;
;
(defun RFL:MOD (A B)
 (rem (+ (rem A B) B) B)
)
