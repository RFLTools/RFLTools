;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:GETSPIRALRADIUS returns the radius of the spiral data
;
;
(defun RFL:GETSPIRALRADIUS (L PLT PLTST PST / DIR LS R RMAX THETAMAX)
 (if (> (sin (- (angle PLTST PST) (angle PLT PLTST))) 0.0)
  (setq DIR 1.0)
  (setq DIR -1.0)
 )
 (setq R 0.0)
 (setq LS (RFL:GETSPIRALLS2 PLT PLTST PST))
 (setq RMAX (RFL:GETSPIRALR2 PLT PLTST PST))
 (if (< L RFL:TOLFINE)
  (progn
   (setq PS PLT)
   (setq THETA 0.0)
  )
  (progn
   (if (< L RFL:TOLFINE)
    (progn
     (setq R 0.0)
    )
    (progn
     (setq R (* RMAX (/ LS L)))
    )
   )
  )
 )
 (* DIR R)
)