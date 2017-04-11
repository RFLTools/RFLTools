;
;
;     Program written by Robert Livingston, 2017-04-11
;
;     C:ESTRADIUS estimates the segmented alignments radius at a given point
;
;
(defun C:ESTRADIUS (/ CMDECHO R RFL:ESTRADIUSDIST RFL:ESTRADIUSNUMPOINTS STA TMP)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 
 (setq R nil)
 
 (if (setq TMP (getdist "\nMaximum distance from point for estimation <30.0> : "))
  (setq RFL:ESTRADIUSDIST TMP)
  (setq RFL:ESTRADIUSDIST 30.0)
 )
 (if (or (= nil (setq TMP (getint "\nMaximum number of points for estimation <10> : ")))
         (> TMP 2)
     )
  (setq RFL:ESTRADIUSNUMPOINTS TMP)
  (setq RFL:ESTRADIUSNUMPOINTS 10)
 )
 (while (setq STA (car (RFL:STAOFF (getpoint "\nSelect point : "))))
  (if (setq R (RFL:ESTRADIUS STA))
   (if (< R 0.0)
    (princ (strcat "\nRadius = " (rtos (abs R)) " Right\n"))
    (princ (strcat "\nRadius = " (rtos (abs R)) " Left\n"))
   )
  )
 )
 (setvar "CMDECHO" CMDECHO)
 R
)
