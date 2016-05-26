;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:GETSPIRALTHETA2 returns the spiral deflection for given long pi and short tangent points
;
;
(defun RFL:GETSPIRALTHETA2 (PLT PLTST PST / ENTLIST ENTLIST2 LS THETA)
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
 THETA
)
