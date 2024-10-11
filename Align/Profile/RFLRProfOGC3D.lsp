;
;
;     Program written by Robert Livingston, 2024-10-11
;
;     RFL:RPROFOGC3D is a utility for reading a C3D OG profile and setting RFL:OGLIST
;     NOTE - Must be using C3D, will not work in straight AutoCAD
;
;
(defun RFL:RPROFOGC3D (ENT / C CMAX CMDECHO ENDELEVATION ENDSTATION ENTLIST OBENTITY OBPROFILE OBENTITIES STARTELEVATION STARTSTATION)
 (if (= nil vlax-create-object) (vl-load-com))
 
 (setq RFL:OGLIST nil)
 
 (setq OBPROFILE nil)
 (if (= (type ENT) 'VLA-OBJECT)
  (setq OBPROFILE ENT)
  (progn
   (setq ENTLIST (entget ENT))
   (if (= "AECC_PROFILE" (cdr (assoc 0 ENTLIST)))
    (progn
     (setq OBPROFILE (vlax-ename->vla-object ENT))
    )
    (princ "\n*** Not a C3D Profile ***")
   )
  )
 )
 (if OBPROFILE
  (progn
   (setq OBENTITIES (vlax-get-property OBPROFILE "Entities"))
   (setq CMAX (vlax-get-property OBENTITIES "Count"))
   (setq C 0)
   (while (< C CMAX)
    (setq OBENTITY (vlax-invoke-method OBENTITIES "Item" C))
    (setq STARTSTATION (vlax-get-property OBENTITY "StartStation"))
    (setq STARTELEVATION (vlax-get-property OBENTITY "StartElevation"))
    (setq RFL:OGLIST (append RFL:OGLIST (list (list STARTSTATION STARTELEVATION))))
    (setq C (1+ C))
   )
   (setq ENDSTATION (vlax-get-property OBENTITY "EndStation"))
   (setq ENDELEVATION (vlax-get-property OBENTITY "EndElevation"))
   (setq RFL:OGLIST (append RFL:OGLIST (list (list ENDSTATION ENDELEVATION))))
  )
 )
 RFL:OGLIST   
)