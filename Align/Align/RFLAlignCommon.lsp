;
;
;     Program written by Robert Livingston, 2016-05-26
;
;     Collection of common routines, functions and constants
;
;
(setq RFL:TOL 0.000001
      RFL:TOLFINE 1e-16
)
(defun RFL:ANGLE3P (P1 P2 P3 / ANG)
 (setq ANG (- (angle P2 P1) (angle P2 P3)))
 (if (< ANG 0.0) (setq ANG (* -1.0 ANG)))
 (if (> ANG pi) (setq ANG (- (* 2.0 pi) ANG)))
 (eval ANG)
)
(defun RFL:FACT (N / F)
 (setq F 1)
 (while (> N 0)
  (setq F (* F N))
  (setq N (- N 1))
 )
 F
)
(defun RFL:SIGN (X)
 (if (< X 0.0)
  -1.0
  1.0
 )
)
(defun RFL:TAN (X)
 (/ (sin X) (cos X))
)

