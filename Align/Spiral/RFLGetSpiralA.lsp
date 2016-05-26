;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:GETSPIRALA returns the spiral 'A' for an entity
;
;
(defun RFL:GETSPIRALA (ENT / R LS)
 (setq R (RFL:GETSPIRALR ENT))
 (setq LS (RFL:GETSPIRALLS ENT))
 (if (= LS nil)
  nil
  (sqrt (* LS R))
 )
)
