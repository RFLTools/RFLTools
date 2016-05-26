;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:GETSPIRALTHETA returns the spiral deflection for an entity
;
;
(defun RFL:GETSPIRALTHETA (ENT / ENTLIST ENTLIST2 LS PLT PLTST PST SPIRALLIST THETA)
 (setq SPIRALLIST (RFL:GETSPIRALDATA ENT))
 (if (= SPIRALLIST nil)
  (progn
   (setq THETA nil)
  )
  (progn
   (setq PLT (car SPIRALLIST))
   (setq PLTST (cadr SPIRALLIST))
   (setq PST (caddr SPIRALLIST))
   (setq THETA (abs (- (angle PST PLTST) (angle PLTST PLT))))
   (if (< THETA 0.0)
    (progn
     (setq THETA (+ THETA (* 2.0 pi)))
    )
   )
   (if (> THETA pi)
    (progn
     (setq THETA (- (* 2.0 pi) THETA))
    )
   )
  )
 )
 THETA
)
