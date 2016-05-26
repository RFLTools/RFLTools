;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:SPIRALK returns the spiral 'K' value for a given radius and length
;
;
(defun RFL:SPIRALK (R LS / THETA)
 (setq THETA (/ LS R 2.0))
 (* R (- (SPIRALFXR THETA) (sin THETA)))
)
