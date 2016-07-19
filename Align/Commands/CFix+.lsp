;
;
;     Program written by Robert Livingston, 2015-03-13
;
;     FIX+ modifies a text entity to adjust it's '+' to align with its insertion point.
;
;
(defun C:FIX+ (/ *error* ANGBASE ANGDIR ATTREQ CMDECHO ENT ORTHOMODE OSMODE P P1 TB TBL TBR W WL WR W+)
 (setq ATTREQ (getvar "ATTREQ"))
 (setvar "ATTREQ" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0.0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 1)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq OSMODE (getvar "OSMODE"))
 (setvar "OSMODE" 0)
 (setq ORTHOMODE (getvar "ORTHOMODE"))
 (setvar "ORTHOMODE" 0)

 (defun *error* (msg)
  (setvar "ATTREQ" ATTREQ)
  (setvar "ANGBASE" ANGBASE)
  (setvar "ANGDIR" ANGDIR)
  (setvar "CMDECHO" CMDECHO)
  (setvar "OSMODE" OSMODE)
  (setvar "ORTHOMODE" ORTHOMODE)
  (print msg)
 )

 (while (/= nil (setq ENT (car (entsel))))
  (RFL:FIX+ ENT)
 )
 
 (setvar "ATTREQ" ATTREQ)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
 (setvar "CMDECHO" CMDECHO)
 (setvar "OSMODE" OSMODE)
 (setvar "ORTHOMODE" ORTHOMODE)
)
