;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:GETSPIRALPI2 returns the spiral PI for given length long pi and short tangent points
;
;
(defun RFL:GETSPIRALPI2 (L PLT PLTST PST / A P P1 P2 THETA)
 (if (< L RFL:TOLFINE)
  (setq P PLTST)
  (progn
   (setq P1 (RFL:SPIRALXY2 (list L 0.0) PLT PLTST PST))
   (setq A (RFL:GETSPIRALA2 PLT PLTST PST))
   (setq THETA (/ (* L L) (* A A) 2.0))
   (if (> (sin (- (angle PLTST PST) (angle PLT PLTST))) 0.0)
    (setq THETA (+ (angle PLT PLTST) THETA))
    (setq THETA (- (angle PLT PLTST) THETA))
   )
   (setq P2 (list (+ (car P1) (cos THETA)) (+ (cadr P1) (sin THETA))))
   (setq P (inters P1 P2 PLTST PST nil))
  )
 )
 P
)
