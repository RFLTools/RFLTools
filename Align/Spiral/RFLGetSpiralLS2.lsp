;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:GETSPIRALLS2 returns the spiral length for given long pi and short tangent points
;
;
(defun RFL:GETSPIRALLS2 (PLT PLTST PST / THETA R)
 (setq THETA (RFL:GETSPIRALTHETA2 PLT PLTST PST))
 (setq R (RFL:GETSPIRALR2 PLT PLTST PST))
 (if (= THETA nil)
  nil
  (* 2.0 THETA R)
 )
)
