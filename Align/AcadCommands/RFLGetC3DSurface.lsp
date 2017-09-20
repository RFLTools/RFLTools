;
;
;     Program written by Robert Livingston
;
;
(defun RFL:GETC3DSURFACE (/ ENT ENTLIST GETFROMLIST OBSURFACE)
 (defun GETFROMLIST (/ *acad* ACADPROD C3DOBJECT C3DDOC C3DSURFS C CMAX C3DSURF)
  (textscr)
  (princ "\n")
  (setq ACADPROD (strcat "AeccXUiLand.AeccApplication." (RFL:ACADVER)))
  (setq *acad* (vlax-get-acad-object))
  (setq C3DOBJECT (vla-getinterfaceobject *acad* ACADPROD))
  (setq C3DDOC (vla-get-activedocument C3DOBJECT))
  (setq C3DSURFS (vlax-get C3DDOC 'surfaces))
  (setq CMAX (vlax-get-property C3DSURFS "Count"))
  (setq C 0)
  (while (< C CMAX)
   (setq C3DSURF (vlax-get-property C3DSURFS "Item" C))
   (setq C (+ C 1))
   (princ (strcat (itoa C) " - " (vlax-get-property C3DSURF "DisplayName") "\n"))
  )
  (if (setq C (getint "Enter surface number : "))
   (setq OBSURFACE (vlax-get-property C3DSURFS "Item" (- C 1)))
   nil
  )
  (graphscr)
 )

 (setq OBSURFACE nil)

 (setq ENT (car (entsel "\nSelect C3D surface or <return> to select from list : ")))
 (if (= nil ENT)
  (GETFROMLIST)
  (progn
   (setq ENTLIST (entget ENT))
   (if (/= "AECC_TIN_SURFACE" (cdr (assoc 0 ENTLIST)))
    (if (/= "AECC_GRID_SURFACE" (cdr (assoc 0 ENTLIST)))
     (princ "\n*** Not a C3D Surface ***")
     (setq OBSURFACE (vlax-ename->vla-object ENT))
    )
    (setq OBSURFACE (vlax-ename->vla-object ENT))
   )
  )
 )
 OBSURFACE
)
