;
;
;     Program written by Robert Livingston 2020-05-28
;
;
(defun RFL:GETC3DPROFILE (/ ENT ENTLIST GETFROMLIST OBALIGNMENT OBPROFILE)
 (setq OBPROFILE nil)
 (defun GETFROMLIST (/ *acad* ACADACTIVEDOCUMENT ACADPROD ACADVER C3DOBJECT C3DDOC C3DPROFILE C3DPROFILES C CMAX C3DALIGN)
  (textscr)
  (princ "\n")
;  (setq ACADPROD (strcat "AeccXUiLand.AeccApplication." (RFL:ACADVER)))
;  (setq *acad* (vlax-get-acad-object))
;  (setq C3DOBJECT (vla-getinterfaceobject *acad* ACADPROD))
;  (setq C3DDOC (vla-get-activedocument C3DOBJECT))
;  (setq C3DALIGNS (vlax-get C3DDOC 'alignmentssiteless))
  (setq C3DPROFILES (vlax-get-property OBALIGNMENT "Profiles"))
  (if (= (setq CMAX (vlax-get-property C3DPROFILES "Count")) 0)
   (princ "\nNo profiles exist for alignment.")
   (progn
    (setq C 0)
    (while (< C CMAX)
     (setq C3DPROFILE (vlax-invoke-method C3DPROFILES "Item" C))
     (setq C (+ C 1))
     (princ (strcat (itoa C) " - " (vlax-get-property C3DPROFILE "DisplayName") "\n"))
    )
    (if (setq C (getint "Enter profile number : "))
     (setq OBPROFILE (vlax-invoke-method C3DPROFILES "Item" (- C 1)))
     nil
    )
    (graphscr)
   )
  )
 )
 (if (setq OBALIGNMENT (RFL:GETC3DALIGNMENT))
  (progn
   (GETFROMLIST)
  )
 )
 OBPROFILE
)
