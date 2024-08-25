;
;
;     Program written by Robert Livingston, 2024-08-23
;
;     RFL:INTERSLC returns the intersection points between a line and a circle ((X1 Y1) (X2 Y2) (XC YC) R)
;
;
(defun RFL:INTERSLC (P1 P2 PC R / ANG ANGB D R T0 X X1 X2 XC Y Y1 Y2 YC)
 (setq X1 (car P1))
 (setq Y1 (cadr P1))
 (setq X2 (car P2))
 (setq Y2 (cadr P2))
 (setq XC (car PC))
 (setq YC (cadr PC))
 (setq ANGB (angle P1 P2)) ; Base Angle
 (setq ANG (angle P1 PC))
 (setq D (distance P1 PC))
 (setq Y (* D (sin (- ANG ANGB))))
 (if (> (abs Y) R)
  nil
  (progn
   (setq X (* D (cos (- ANG ANGB))))
   (setq T0 (sqrt (- (* R R) (* Y Y))))
   (list (list (+ (car P1) (* (- X T0) (cos ANGB)))
               (+ (cadr P1) (* (- X T0) (sin ANGB)))
         )
         (list (+ (car P1) (* (+ X T0) (cos ANGB)))
               (+ (cadr P1) (* (+ X T0) (sin ANGB)))
         )
   )
  )
 )
)