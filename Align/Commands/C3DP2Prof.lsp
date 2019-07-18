;
;
;   Program written by Robert Livingston, 01/03/05
;
;   3DP2PROF is a routine for drawing 3D polyline control lines on a profile
;
;
(defun C:3DP2PROF (/ C CLAYER CMDECHO ENT ENTLIST LAYER LR ORTHOMODE OSMODE P S)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq CLAYER (getvar "CLAYER"))
 (setq ORTHOMODE (getvar "ORTHOMODE"))
 (setvar "ORTHOMODE" 0)
 (setq OSMODE (getvar "OSMODE"))
 (setvar "OSMODE" 0)

 (if (= nil RFL:ALIGNLIST)
  (princ "\n***** Alignment not set! *****\n")
  (progn
   (RFL:PROFDEF)
   (while (/= (setq ENT (car (entsel "\nSelect 3D polyline : "))) nil)
    (setq ENTLIST (entget ENT))
    (if (/= (cdr (assoc 0 ENTLIST)) "POLYLINE")
     (princ "\n***** Not a polyline! *****\n")
     (progn
      (if (= 0 (logand (cdr (assoc 70 ENTLIST)) 8))
       (princ "\n***** Not a 3D polyline! *****\n")
       (progn
        (command "._PLINE")
        (setq ENT (entnext ENT))
        (setq ENTLIST (entget ENT))
        (while (/= (cdr (assoc 0 ENTLIST)) "SEQEND")
         (setq P (cdr (assoc 10 ENTLIST)))
         (setq S (RFL:STAOFF P))
         (if (/= S nil)
          (command (RFL:PROFPOINT (nth 0 S) (nth 2 P)))
         )
         (setq ENT (entnext ENT))
         (setq ENTLIST (entget ENT))
        )
        (command "")
       )
      )
     )
    ) 
   )
  )
 )

 (setvar "CMDECHO" CMDECHO)
 (setvar "CLAYER" CLAYER)
 (setvar "ORTHOMODE" ORTHOMODE)
 (setvar "OSMODE" OSMODE)
)