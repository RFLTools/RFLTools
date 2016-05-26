;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:GETSPIRALLS returns the spiral length for an entity
;
;
(defun RFL:GETSPIRALLS (ENT / THETA R)
 (setq THETA (RFL:GETSPIRALTHETA ENT))
 (setq R (RFL:GETSPIRALR ENT))
 (if (= THETA nil)
  nil
  (* 2.0 THETA R)
 )
)
