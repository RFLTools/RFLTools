;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:SPIRALP returns the spiral 'P' offset for a given length and radius
;
;
(defun RFL:SPIRALP (R LS / THETA)
 (setq THETA (/ LS R 2.0))
 (* R (- (RFL:SPIRALFYR THETA) (- 1.0 (cos THETA))))
)
