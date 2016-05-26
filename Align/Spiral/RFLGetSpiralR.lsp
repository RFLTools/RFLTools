;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:GETSPIRALR returns the spiral radius for an entity
;
;
(defun RFL:GETSPIRALR (ENT / PLTST PST R SPIRALLIST)
 (setq SPIRALLIST (RFL:GETSPIRALDATA ENT))
 (if (= SPIRALLIST nil)
  (progn
   (setq R nil)
  )
  (progn
   (setq PLTST (cadr SPIRALLIST))
   (setq PST (caddr SPIRALLIST))
   (setq THETA (RFL:GETSPIRALTHETA ENT))
   (setq R (/ (* (distance PLTST PST) (sin THETA)) (RFL:SPIRALFYR THETA)))
  )
 )
 R
)
