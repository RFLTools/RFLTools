;
;
;     Program written by Robert Livingston, 2016-07-19
;
;     RFL:RPROFC3D is a utility for reading a C3D profile and setting RFL:PVILIST
;     NOTE - Must be using C3D, will not work in straight AutoCAD
;     NOTE - Works for type 1 and type 3 vertical curves
;
;
(defun RFL:RPROFC3D (ENT / C CMAX CMDECHO ENDELEVATION ENDSTATION ENTITY ENTITYNEXT ENTLIST N1 N2 OBALIGNMENT OBPROFILE OBPROFILES OBENTITIES
                           PVISTATION PVIELEVATION PVILENGTH STARTELEVATION STARTSTATION TMP)
 (if (= nil vlax-create-object) (vl-load-com))
 
 (defun GETPVISTATION ()
  (setq PVISTATION (vlax-get-property ENTITY "PVIStation"))
 )
 
 (setq RFL:PVILIST nil)
 
 (setq OBPROFILE nil)
 (if ENT
  (if (= (type ENT) 'VLA-OBJECT)
   (setq OBPROFILE ENT)
   (progn
    (setq ENTLIST (entget ENT))
    (if (/= "AECC_PROFILE" (cdr (assoc 0 ENTLIST)))
     (princ "\n*** Not a C3D Profile ***")
     (progn
      (setq OBPROFILE (vlax-ename->vla-object ENT))
     )
    )
   )
  )
  (progn
   (setq OBALIGNMENT (RFL:GETC3DALIGNMENT))
   (setq OBPROFILE (RFL:GETC3DPROFILE OBALIGNMENT))
  )
 )
 (if OBPROFILE
  (progn
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
 
 
 (if RFL:PVILIST
  (progn
   ; Sorting
   (setq RFL:PVILIST (vl-sort RFL:PVILIST (function (lambda (N1 N2) (< (car N1) (car N2))))))
   ; Removing extra PVIs
   (setq TMP (list (car RFL:PVILIST))
         RFL:PVILIST (cdr RFL:PVILIST)
   )
   (while (cdr RFL:PVILIST)
    (if (and (> (abs (- (caar RFL:PVILIST) (car (last TMP)))) RFL:TOL)
             (> (abs (- (caadr RFL:PVILIST) (caar RFL:PVILIST))) RFL:TOL)
             (> (abs (- (/ (- (cadar RFL:PVILIST) (cadr (last TMP))) (- (caar RFL:PVILIST) (car (last TMP))))
                        (/ (- (cadadr RFL:PVILIST) (cadar RFL:PVILIST)) (- (caadr RFL:PVILIST) (caar RFL:PVILIST)))
                     )
                )
                RFL:TOL
             )
        )
     (setq TMP (append TMP (list (car RFL:PVILIST))))
    )
    (setq RFL:PVILIST (cdr RFL:PVILIST))
   )
   (setq TMP (append TMP RFL:PVILIST))
   (setq RFL:PVILIST TMP)
  )
 )
 
 
 RFL:PVILIST
)