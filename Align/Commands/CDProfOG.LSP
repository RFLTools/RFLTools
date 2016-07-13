;
;
;   Program written by Robert Livingston, 99/11/15
;
;   DPROFOG draws on the current layer the current OG profile defined in RFL:OGLIST
;
;
(defun C:DPROFOG (/ ANGBASE ANGDIR CMDECHO OSMODE C)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq OSMODE (getvar "OSMODE"))
 (setvar "OSMODE" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 0)

 (if (/= nil RFL:OGLIST)
  (progn
   (RFL:PROFDEF)
   (setq C 0)
   (command "._PLINE")
   (while (< C (length RFL:OGLIST))
    (command (RFL:PROFPOINT (nth 0 (nth C RFL:OGLIST)) (nth 1 (nth C RFL:OGLIST))))
    (setq C (+ C 1))
   )
   (command "")
  )
  (progn
   (princ "\n*** OG PROFILE NOT SET ***\n")
  )
 )

 (setvar "CMDECHO" CMDECHO)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
)
