;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:SPIRALPOINTON Checks is the entered point is within the spiral limits
;
;
(defun RFL:SPIRALPOINTON (P PLT PLTST PST LO / A2 ALPHA F1 F2 FCTN GETR PX PY
                                               RMAX SPIRALDIRECTION
                                               THETA1 THETA2 THETAMAX)
 (defun GETR (VAL)
  (if (< (abs VAL) RFL:TOLFINE)
   (eval 0.0)
   (sqrt (/ A2 VAL 2.0))
  )
 )
 (defun FCTN (VAL)
  (if (< (abs VAL) RFL:TOLFINE)
   PX
   (+ (* (- PX (* (GETR VAL) (RFL:SPIRALFXR VAL))) (cos VAL))
         (* SPIRALDIRECTION (- PY (* SPIRALDIRECTION (GETR VAL) (RFL:SPIRALFYR VAL))) (sin VAL))
   )
  )
 )
 (if (> (sin (- (angle PLTST PST) (angle PLT PLTST))) 0.0)
  (setq SPIRALDIRECTION 1.0)
  (setq SPIRALDIRECTION -1.0)
 )
 (setq ALPHA (angle PLT PLTST))
 (setq PX (+ (* (- (cadr P) (cadr PLT)) (sin ALPHA)) (* (- (car P) (car PLT)) (cos ALPHA))))
 (setq PY (- (* (- (cadr P) (cadr PLT)) (cos ALPHA)) (* (- (car P) (car PLT)) (sin ALPHA))))
 (setq THETAMAX (RFL:GETSPIRALTHETA2 PLT PLTST PST))
 (setq RMAX (RFL:GETSPIRALR2 PLT PLTST PST))
 (setq A2 (* 2.0 RMAX RMAX THETAMAX))
 (setq THETA1 (/ (* LO LO) A2 2.0))
 (setq THETA2 THETAMAX)
 (setq F1 (FCTN THETA1))
 (setq F2 (FCTN THETA2))
 (if (> (* F1 F2) RFL:TOLFINE)
  0
  1
 )
)
