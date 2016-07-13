;
;
;   Program written by Robert Livingston, 98/06/11
;
;   C:DALIGNOS draws the current alignment at a specified offset
;
;
(defun C:DALIGNOS (/ AL ANGBASE ANGDIR CMDECHO OS OSMODE REP SFLAG)
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
   (setq OS (getreal "\nEnter offset (-ve = left, +ve = right) : "))
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
    (RFL:DRAWALIGNOS OS)
    (RFL:DRAWALIGNOS2 OS)
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
