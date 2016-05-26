;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:SPIRALXY2 returns the station and offset of the supplied point to the supplied spiral data
;
;
(defun RFL:SPIRALXY2 (P PLT PLTST PST / ANG ANG2 DIR L LS OFFSET PS PXY R RMAX THETAMAX X Y)
 (setq ANG (angle PLT PLTST))
 (if (> (sin (- (angle PLTST PST) (angle PLT PLTST))) 0.0)
  (setq DIR 1.0)
  (setq DIR -1.0)
 )
 (setq L (car P))
 (setq OFFSET (cadr P))
 (setq LS (RFL:GETSPIRALLS2 PLT PLTST PST))
 (setq THETAMAX (RFL:GETSPIRALTHETA2 PLT PLTST PST))
 (setq RMAX (RFL:GETSPIRALR2 PLT PLTST PST))
 (if (< L RFL:TOLFINE)
  (progn
   (setq PS PLT)
   (setq THETA 0.0)
  )
  (progn
   (setq THETA (* THETAMAX (expt (/ L LS) 2)))
   (if (< L RFL:TOLFINE)
    (progn
     (setq R 0.0)
     (setq X 0.0)
     (setq Y 0.0)
    )
    (progn
     (setq R (* RMAX (/ LS L)))
     (setq X (* R (RFL:SPIRALFXR THETA)))
     (setq Y (* DIR R (RFL:SPIRALFYR THETA)))
    )
   )
   (setq PS (list (+ (car PLT) (* X (cos ANG)) (* -1.0 Y (sin ANG)))
                  (+ (cadr PLT) (* X (sin ANG)) (* Y (cos ANG)))
            )
   )
  )
 )
 (setq ANG2 (+ ANG (* DIR THETA) (/ pi -2.0)))
 (setq PXY (list (+ (car PS) (* OFFSET (cos ANG2)))
                 (+ (cadr PS) (* OFFSET (sin ANG2)))
           )
 )

 PXY
)
