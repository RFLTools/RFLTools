;
;
;     Program written by Robert Livingston, 10-04-30
;
;     INSERTSPOT reads the C3D surface and inserts a spotelevation block with the elevation read from the surface
;
;
(defun C:INSERTSPOT (/ *error* ANGBASE ANGDIR ATTREQ C CMAX CMDECHO DIMZIN ENT ENTLIST GETFROMLIST OBSURFACE OBENTITIES OSMODE)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq ATTREQ (getvar "ATTREQ"))
 (setq DIMZIN (getvar "DIMZIN"))
 (setvar "DIMZIN" 0)
 (setq OSMODE (getvar "OSMODE"))
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0.0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 0)

 (defun *error* (msg)
  (setvar "CMDECHO" CMDECHO)
  (setvar "ATTREQ" ATTREQ)
  (setvar "DIMZIN" DIMZIN)
  (setvar "OSMODE" OSMODE)
  (setvar "ANGBASE" ANGBASE)
  (setvar "ANGDIR" ANGDIR)
  (princ msg)
  (setq *error* nil)
 )

 (command "._UNDO" "M")

 (if (= nil VLAX-CREATE-OBJECT) (vl-load-com))

 (princ "\nSelect spot elevation block (<return> for previous block) :")
 (setq ENT (car (entsel)))
 (if (= ENT nil)
  (progn
   (setq RFL:INSERTSPOTBLOCK "SPOTELEVATION")
   (setq RFL:INSERTSPOTSCALE 1.0)
   (if (= nil (tblsearch "BLOCK" RFL:INSERTSPOTBLOCK))
    (RFL:MAKEENT RFL:INSERTSPOTBLOCK)
   )
  )
  (progn
   (setq ENTLIST (entget ENT))
   (setq RFL:INSERTSPOTBLOCK (cdr (assoc 2 ENTLIST)))
   (setq RFL:INSERTSPOTSCALE (cdr (assoc 41 ENTLIST)))
  )
 )

 (setq OBSURFACE nil)
 
 (setq OBSURFACE (RFL:GETC3DSURFACE))
 
 (if (/= nil OBSURFACE)
  (progn
   (while (/= nil (setq P (getpoint "\nSurface point : ")))
    (setq ELEV (vlax-invoke-method OBSURFACE "FindElevationAtXY" (car P) (cadr P)))
    (setvar "ATTREQ" 0)
    (setvar "OSMODE" 0)
    (command "INSERT" RFL:INSERTSPOTBLOCK "_NON" P RFL:INSERTSPOTSCALE RFL:INSERTSPOTSCALE "")
    (setvar "ATTREQ" 1)
    (setvar "OSMODE" OSMODE)
    (setq ENT (entlast))
    (setq ENTLIST (entget ENT))
    (if (= 1 (cdr (assoc 66 ENTLIST)))
     (progn
      (setq ENT2 (entnext ENT))
      (setq ENTLIST (entget ENT2))
      (while (/= "SEQEND" (cdr (assoc 0 ENTLIST)))
       (if (or (= "ELEV" (cdr (assoc 2 ENTLIST))) (= "ELEVATION" (cdr (assoc 2 ENTLIST))))
        (setq ENTLIST (subst (cons 1 (rtos ELEV)) (assoc 1 ENTLIST) ENTLIST))
       )
       (entmod ENTLIST)
       (setq ENT2 (entnext ENT2))
       (setq ENTLIST (entget ENT2))
      )
      (entupd ENT)
     )
    )
   )
  )
 )

 (setvar "CMDECHO" CMDECHO)
 (setvar "ATTREQ" ATTREQ)
 (setvar "DIMZIN" DIMZIN)
 (setvar "OSMODE" OSMODE)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
)