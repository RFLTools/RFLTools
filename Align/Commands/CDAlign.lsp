;
;
;   Program written by Robert Livingston, 98/06/11
;
;   C:DALIGN draws the current alignment
;
;
(defun C:DALIGN (/ AL ANGBASE ANGDIR CMDECHO OSMODE REP SFLAG STEP STEPSTA)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq OSMODE (getvar "OSMODE"))
 (setvar "OSMODE" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 0)

 (command "._UNDO" "M")
 (command "._UCS" "W")

 (if (/= nil RFL:ALIGNLIST)
  (progn
   (setq AL RFL:ALIGNLIST)
   (setq SFLAG 0)
   (while (/= AL nil)
    (if (listp (last (car AL)))
     (progn
      (setq SFLAG 1)
     )
    )
    (setq AL (cdr AL))
   )
   (if (= SFLAG 0)
    (RFL:DRAWALIGN)
    (RFL:DRAWALIGN2)
   )
  )
  (princ "\n*** ALIGNMENT NOT SET ***\n")
 )

 (command "._UCS" "P")
 (setvar "CMDECHO" CMDECHO)
 (setvar "OSMODE" OSMODE)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
)