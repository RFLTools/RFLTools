;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:SPIRALPR returns (R * spiral 'P') for a given deflection
;
;
(if RFL:SPIRALPR (princ "\nRFL:SPIRALPR already loaded...")
(defun RFL:SPIRALPR (THETA)
 (- (RFL:SPIRALFYR THETA) (- 1.0 (cos THETA)))
)
)
