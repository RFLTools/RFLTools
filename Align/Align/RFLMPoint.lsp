;
;
;   Program written by Robert Livingston, 99/09/10
;
;   RFL:MPOINT is a routine that returns a set of points from one station/offset to another station/offset
;          (note - ALIGN must be loaded and an alignment must be set.  Also recommended to turn off your osnaps)
;
;
(defun RFL:MPOINT2 (STATION1 OFFSET1 STATION2 OFFSET2 CMAX / C OFFSETINC P STATIONINC)
 (if (and (/= RFL:ALIGNLIST nil) (/= RFL:XY nil))
  (progn
   (setq C 0)
   (while (< C (+ CMAX 1))
    (setq P (RFL:XY (list (+ STATION1 (* (- STATION2 STATION1) (/ (float C) (float CMAX))))
                          (+ OFFSET1 (* (- OFFSET2 OFFSET1) (/ (float C) (float CMAX))))
                    )
            )
    )
    (print P)
    (command "_NON" P)
    (setq C (+ C 1))
   )
  )
 )
)
(defun RFL:MPOINT (/ C CMAX CMDECHO OFFSET1 OFFSET2 STATION1 STATION2)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (if (and (/= RFL:ALIGNLIST nil) (/= RFL:XY nil))
  (progn
   (setq STATION1 (getreal "\nEnter start station : "))
   (setq OFFSET1 (getreal "\nEnter start offset : "))
   (setq STATION2 (getreal "\nEnter end station : "))
   (setq OFFSET2 (getreal "\nEnter end offset : "))
   (setq CMAX (getint "\nEnter number of steps : "))
   (RFL:MPOINT2 STATION1 OFFSET1 STATION2 OFFSET2 CMAX)
  )
 )
 (setvar "CMDECHO" CMDECHO)
)
