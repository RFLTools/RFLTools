;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:SPIRALKR returns the spiral 'K' value for a given deflection
;
;
(if RFL:SPIRALKR (princ "\nRFL:SPIRALKR already loaded...")
(defun RFL:SPIRALKR (THETA)
 (- (RFL:SPIRALFXR THETA) (sin THETA))
)
)
