;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:GETSPIRALA2 returns the spiral 'A' for given long pi and short tangent points
;
;
(defun RFL:GETSPIRALA2 (PLT PLTST PST / R LS)
 (setq R (RFL:GETSPIRALR2 PLT PLTST PST))
 (setq LS (RFL:GETSPIRALLS2 PLT PLTST PST))
 (if (= LS nil)
  nil
  (sqrt (* LS R))
 )
)
