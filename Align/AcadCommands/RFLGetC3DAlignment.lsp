;
;
;     Program written by Robert Livingston
;
;
(defun RFL:GETC3DALIGNMENT (/ ENT ENTLIST GETFROMLIST OBALIGNMENT)
 (defun GETFROMLIST (/ *acad* ACADACTIVEDOCUMENT ACADPROD ACADVER C3DOBJECT C3DDOC C3DALIGNS C CMAX C3DALIGN)
  (textscr)
  (princ "\n")
  (setq ACADPROD (strcat "AeccXUiLand.AeccApplication." (RFL:ACADVER)))
  (setq *acad* (vlax-get-acad-object))
  (setq C3DOBJECT (vla-getinterfaceobject *acad* ACADPROD))
  (setq C3DDOC (vla-get-activedocument C3DOBJECT))
  (setq C3DALIGNS (vlax-get C3DDOC 'alignmentssiteless))
  (setq CMAX (vlax-get-property C3DALIGNS "Count"))
  (setq C 0)
  (while (< C CMAX)
   (setq C3DALIGN (vlax-invoke-method C3DALIGNS "Item" C))
   (setq C (+ C 1))
   (princ (strcat (itoa C) " - " (vlax-get-property C3DALIGN "DisplayName") "\n"))
  )
  (if (setq C (getint "Enter alignment number : "))
   (setq OBALIGNMENT (vlax-invoke-method C3DALIGNS "Item" (- C 1)))
   nil
  )
  (graphscr)
 )
 (setq OBALIGNMENT nil)
 (setq ENT (car (entsel "\nSelect C3D alignment (<return> to choose from list) : ")))
 (if (= nil ENT)
  (GETFROMLIST)
  (progn
   (setq ENTLIST (entget ENT))
   (if (/= "AECC_ALIGNMENT" (cdr (assoc 0 ENTLIST)))
    (princ "\n*** Not a C3D Alignment ***")
    (setq OBALIGNMENT (vlax-ename->vla-object ENT))
   )
  )
 )
 OBALIGNMENT
)
