;
;
;     Program written by Robert Livingston, 2017-04-10
;
;     C:DRAWMID draws a polyline midpoint between two selected polylines
;
;
(defun C:DRAWMID (/ 3D CMDECHO ENT1 ENT2 ENTLIST1 ENTLIST2 P PLIST1 PLIST2 PLISTM ORTHOMODE OSMODE)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq ORTHOMODE (getvar "ORTHOMODE"))
 (setvar "ORTHOMODE" 0)
 (setq OSMODE (getvar "OSMODE"))
 (setvar "OSMODE" 0)

 (setq PLISTM nil)
 (if (setq ENT1 (car (entsel "\nSelect first polyline : ")))
  (if (setq PLIST1 (RFL:GETPLIST ENT1))
   (if (setq ENT2 (car (entsel "\nSelect second polyline : ")))
    (if (setq PLIST2 (RFL:GETPLIST ENT2))
     (setq PLISTM (RFL:MIDPLIST PLIST1 PLIST2))
    )
   )
  )
 )
 (if PLISTM
  (progn
   (setq ENTLIST1 (entget ENT1)
         ENTLIST2 (entget ENT2)
   )
   (if (or (= (float (/ (cdr (assoc 70 ENTLIST1)) 2 2 2 2)) (/ (cdr (assoc 70 ENTLIST1)) 16.0))
           (= (float (/ (cdr (assoc 70 ENTLIST2)) 2 2 2 2)) (/ (cdr (assoc 70 ENTLIST2)) 16.0))
       )
    (command "._PLINE")
    (command "._3DPOLY")
   )
   (foreach P PLISTM (command P))
   (command "")
  )
 )
 (setvar "CMDECHO" CMDECHO)
 (setvar "ORTHOMODE" ORTHOMODE)
 (setvar "OSMODE" OSMODE)
 T
)