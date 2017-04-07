;
;
;     Program written by Robert Livingston, 2013-05-01
;
;     C:OUTSWING Calculates the inswing for radius R and wheelbase D
;
;
(defun C:OUTSWING (/ D DR R)
 (setq D (getdist "\nVehicle 'D' : "))
 (setq DR (getdist "\nVehicle 'Dr' : "))
 (while (/= nil (setq R (getdist "\nRadius 'R' : ")))
  (princ (strcat "\nOutSwing = " (rtos (RFL:OUTSWING D DR R)) "\n"))
 )
)