;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:SPIRALKR returns the spiral 'K' value for a given deflection
;
;
(defun RFL:SPIRALKR (THETA)
 (- (RFL:SPIRALFXR THETA) (sin THETA))
)
