;
;
;     Program written by Robert Livingston, 01/06/26
;
;     MAREA is a routine for calculating the total area and total length of a selected set of polylines
;
;
(defun C:MAREA (/ C CMDECHO ENT ENTLIST ENTSET PCOUNT P3DCOUNT SUMAREA SUMLENGTH STR)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (setq ENTSET (ssget))
 (if (/= ENTSET nil)
  (progn
   (setq SUMAREA 0.0 SUMLENGTH 0.0 PCOUNT 0 P3DCOUNT 0 C 0)
   (while (< C (sslength ENTSET))
    (setq ENT (ssname ENTSET C))
    (setq ENTLIST (entget ENT))
    (if (= (cdr (assoc 0 ENTLIST)) "LWPOLYLINE")
     (progn
      (setq PCOUNT (+ PCOUNT 1))
      (command "._AREA" "O" ENT)
      (setq SUMAREA (+ SUMAREA (getvar "AREA")))
      (setq SUMLENGTH (+ SUMLENGTH (getvar "PERIMETER")))
     )
     (if (= (cdr (assoc 0 ENTLIST)) "POLYLINE")
      (if (/= 0 (logand (cdr (assoc 70 ENTLIST)) 8))
       (progn
        (setq P3DCOUNT (+ P3DCOUNT 1))
       )
       (progn
        (setq PCOUNT (+ PCOUNT 1))
        (command "._AREA" "O" ENT)
        (setq SUMAREA (+ SUMAREA (getvar "AREA")))
        (setq SUMLENGTH (+ SUMLENGTH (getvar "PERIMETER")))
       )
      )
     )
    )
    (setq C (+ C 1))
   )
;   (textscr)
   (princ "\n\n\n\n\n\n\n\n\n\n")
   (setq STR (strcat "\nTotal objects selected      : " (itoa (sslength ENTSET))))
   (setq STR (strcat STR "\nTotal 3DPolylines selected  : " (itoa P3DCOUNT)))
   (setq STR (strcat STR "\nTotal LW/Polylines selected : " (itoa PCOUNT)))
   (setq STR (strcat STR "\n"))
   (setq STR (strcat STR "\nTotal area for LW/Polylines   : " (rtos SUMAREA)))
   (setq STR (strcat STR "\nTotal length for LW/Polylines : " (rtos SUMLENGTH)))
   (princ STR)
   (princ "\n\n")
   (alert STR)
  )
 )

 (setvar "CMDECHO" CMDECHO)
)