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
(defun RFL:CENTER (P1 P2 BULGE / ANG ATOTAL CHORD D R X Y)
 (setq ATOTAL (* 4.0 (atan (abs BULGE))))
 (setq CHORD (distance P1 P2))
 (if (< (abs BULGE) RFL:TOLFINE)
  nil
  (progn
   (setq R (/ CHORD (* 2 (sin (/ ATOTAL 2)))))
   (setq ANG (angle P1 P2))
   (setq D (distance P1 P2))
   (setq X (/ D 2.0))
   (setq Y (* (sqrt (- (* R R) (* X X))) (RFL:SIGN BULGE) (RFL:SIGN (- (abs BULGE) 1.0))))
   (list (+ (+ (car P1) (* X (cos ANG))) (* Y (sin ANG)))
         (- (+ (cadr P1) (* X (sin ANG))) (* Y (cos ANG)))
   )
  )
 )
)
(defun RFL:RADIUS (P1 P2 BULGE / ATOTAL CHORD)
 (setq ATOTAL (* 4.0 (atan (abs BULGE))))
 (setq CHORD (distance P1 P2))
 (if (< (abs BULGE) RFL:TOLFINE)
  nil
  (/ CHORD (* 2 (sin (/ ATOTAL 2))))
 )
)
