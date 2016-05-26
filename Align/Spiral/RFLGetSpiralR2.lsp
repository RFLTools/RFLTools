;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:GETSPIRALR2 returns the spiral radius for given long pi and short tangent points
;
;
(defun RFL:GETSPIRALR2 (PLT PLTST PST / R)
 (setq THETA (RFL:GETSPIRALTHETA2 PLT PLTST PST))
 (setq R (/ (* (distance PLTST PST) (sin THETA)) (RFL:SPIRALFYR THETA)))
 R
)