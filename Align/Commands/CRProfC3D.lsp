;
;
;     Program written by Robert Livingston, 11-03-09
;
;     RPROF3D reads the profile from a selected C3D profile
;     NOTE - Must be using C3D, will not work in straight AutoCAD
;     NOTE - Works for type 1 and type 3 vertical curves
;
;
(defun C:RPROFC3D (/ *error* C CMAX CMDECHO ENDELEVATION ENDSTATION ENT ENTITY ENTITYNEXT ENTLIST OBPROFILE OBENTITIES
                     PVISTATION PVIELEVATION PVILENGTH STARTELEVATION STARTSTATION TYPE)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (if (= nil vlax-create-object) (vl-load-com))

 (defun *error* (msg)
  (setvar "CMDECHO" CMDECHO)
  (setq *error* nil)
  (princ msg)
 )

 (defun GETPVISTATION ()
  (setq PVISTATION (vlax-get-property ENTITY "PVIStation"))
 )

 (setq ENT (car (entsel "\nSelect C3D profile : ")))
 (setq ENTLIST (entget ENT))
 (if (/= "AECC_PROFILE" (cdr (assoc 0 ENTLIST)))
  (princ "\n*** Not a C3D Profile ***")
  (progn
   (setq OBPROFILE (vlax-ename->vla-object ENT))
   (setq STARTSTATION (vlax-get-property OBPROFILE "StartingStation"))
   (setq STARTELEVATION (vlax-invoke-method OBPROFILE "ElevationAt" STARTSTATION))
   (setq RFL:PVILIST (list (list STARTSTATION STARTELEVATION "L" 0.0)))
   (setq ENDSTATION (vlax-get-property OBPROFILE "EndingStation"))
   (setq ENDELEVATION (vlax-invoke-method OBPROFILE "ElevationAt" ENDSTATION))
   (setq OBENTITIES (vlax-get-property OBPROFILE "Entities"))
   (setq CMAX (vlax-get-property OBENTITIES "Count"))
   (setq C 0)
   (while (< C CMAX)
    (setq ENTITY (vlax-invoke-method OBENTITIES "Item" C))
    (if (= (+ C 1) CMAX) (setq ENTITYNEXT nil) (setq ENTITYNEXT (vlax-invoke-method OBENTITIES "Item" (+ C 1))))
    (cond
     ((= 1 (vlax-get-property ENTITY "Type"))
      (progn
       (if (/= ENTITYNEXT nil)
        (if (= (vlax-get-property ENTITYNEXT "Type") 1)
         (progn
          (setq PVISTATION (vlax-get-property ENTITY "EndStation"))
          (setq PVIELEVATION (vlax-get-property ENTITY "EndElevation"))
          (setq PVILENGTH 0.0)
          (setq RFL:PVILIST (append RFL:PVILIST (list (list PVISTATION PVIELEVATION "L" PVILENGTH))))
         )
        )
       )
      )
     )
     ((= 3 (vlax-get-property ENTITY "Type"))
      (progn
       (setq PVISTATION (vlax-get-property ENTITY "PVIStation"))
       (setq PVIELEVATION (vlax-get-property ENTITY "PVIElevation"))
       (setq PVILENGTH (vlax-get-property ENTITY "Length"))
       (setq RFL:PVILIST (append RFL:PVILIST (list (list PVISTATION PVIELEVATION "L" PVILENGTH))))
      )
     )
    )
    (setq C (1+ C))
   )
   (setq RFL:PVILIST (append RFL:PVILIST (list (list ENDSTATION ENDELEVATION "L" 0.0))))
  )
 )

 (setvar "CMDECHO" CMDECHO)
)