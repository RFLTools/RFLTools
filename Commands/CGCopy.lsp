;
;
;     Program written by Robert Livingston, 2022-10-14
;
;     C:GCOPY extracts a ground surface from a C3D profile to a polyline
;
;
(defun C:GCOPY (/ C CMAX ENDSTATION ENDELEVATION ENT ENTITY ENTLIST N1 N2 OBENTITIES OBPROFILE ORTHOMODE OSMODE P STARTELEVATION STARTSTATION)
 (command "._UNDO" "M")
 (setq ORTHOMODE (getvar "ORTHOMODE"))
 (setvar "ORTHOMODE" 0)
 (setq OSMODE (getvar "OSMODE"))
 (setvar "OSMODE" 0)
 (defun *error* (msg)
  (setvar "ORTHOMODE" ORTHOMODE)
  (setvar "OSMODE" OSMODE)
  (print msg)
 )
 (princ "\nSelect section view (or RFL Profile Block).")
 (if (RFL:PROFDEF)
  (if (setq ENT (car (entsel "\nSelect profile ground line : ")))
   (progn
    (setq ENTLIST (entget ENT))
    (if (/= "AECC_PROFILE" (cdr (assoc 0 ENTLIST)))
     (princ "\n*** Not a C3D Profile ***")
     (progn
      (setq OBPROFILE (vlax-ename->vla-object ENT))
      (setq STARTSTATION (vlax-get-property OBPROFILE "StartingStation"))
      (setq STARTELEVATION (vlax-invoke-method OBPROFILE "ElevationAt" STARTSTATION))
      ;(setq RFL:PVILIST (list (list STARTSTATION STARTELEVATION "L" 0.0)))
      (setq RFL:OGLIST (list (list STARTSTATION STARTELEVATION)))
      (setq ENDSTATION (vlax-get-property OBPROFILE "EndingStation"))
      (setq ENDELEVATION (vlax-invoke-method OBPROFILE "ElevationAt" ENDSTATION))
      (setq OBENTITIES (vlax-get-property OBPROFILE "Entities"))
      (setq CMAX (vlax-get-property OBENTITIES "Count"))
      (setq C 0)
      (while (< C CMAX)
       (setq ENTITY (vlax-invoke-method OBENTITIES "Item" C))
       (setq RFL:OGLIST (append RFL:OGLIST
                                (list (list (vlax-get-property ENTITY "EndStation")
                                            (vlax-get-property ENTITY "EndElevation")
                                      )
                                )
                        )
       )
       (setq C (1+ C))
      )
      (if (> (length RFL:OGLIST) 1)
;       (C:DPROFOG)
       (progn
        (setq RFL:OGLIST (vl-sort RFL:OGLIST (function (lambda (N1 N2) (< (car N1) (car N2))))))
        (command "._PLINE")
        (foreach P RFL:OGLIST
         (command (RFL:PROFPOINT (car P) (cadr P)))
        )
        (command "")
       )
      )
     )
    )
   )
  )
 )
 (setvar "ORTHOMODE" ORTHOMODE)
 (setvar "OSMODE" OSMODE)
 nil
)