;
;
;     Program written by Robert Livingston, 2013-05-01
;
;     C:INSWING Calculates the inswing for radius R and wheelbase D
;
;
(defun C:INSWING (/ D R)
 (setq D (getdist "\nVehicle 'D' : "))
 (while (/= nil (setq R (getdist "\nRadius 'R' : ")))
  (princ (strcat "\nInSwing = " (rtos (RFL:INSWING D R)) "\n"))
 )
)
