;
;
;   Program written by Robert Livingston, 99/11/15
;
;   DPROF draws the current profile
;
;
(defun C:DPROF (/ ANGBASE ANGDIR CMDECHO OSMODE REP STEP STEPSTA)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq OSMODE (getvar "OSMODE"))
 (setvar "OSMODE" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 0)

 (if (/= nil PVILIST)
  (progn
   (RFL:PROFDEF)
   (initget "Yes No")
   (setq REP (getkword "\nDraw profile (<Yes>/No) :"))
   (if (= REP nil) (setq REP "Yes"))
   (if (= REP "Yes")
    (progn
     (initget "Yes No")
     (setq REP (getkword "\nErase current profile entities (Yes/<No>) :"))
     (if (= REP nil) (setq REP "No"))
     (if (= REP "Yes") (command "._ERASE" (ssget "X" (list (cons 8 (cdr (assoc "PLAYER" RFL:PROFDEFLIST))))) ""))
     (RFL:DRAWPROF PVILIST)
    )
   )
   (initget "Yes No")
   (setq REP (getkword "\nCircle high/low points (Yes/<No>) :"))
   (if (= REP nil) (setq REP "No"))
   (if (= REP "Yes") (RFL:PROFHIGHLOW 1.0))
  )
  (progn
   (princ "\n*** PROFILE NOT SET - RUN GPROF OR RPROF ***\n")
  )
 )

 (setvar "CMDECHO" CMDECHO)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
)
