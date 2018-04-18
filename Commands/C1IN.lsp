;
;
;     Program written by Robert Livingston, 03/07/14
;
;     C:1IN returns the angle for a 1 in X ratio
;         USAGE '1IN
;
;
(defun C:1IN (/ X)
 (setq X (getreal "\nRatio 1 in X, X = "))
 (* (/ 180.0 pi) (atan (/ 1.0 X)))
)