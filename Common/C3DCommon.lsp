;
;
;     Program written by Robert Livingston
;
;     C3DCOMMON.LSP is a collection of common C3D utilities
;
;
(defun RFL:ACADVER (/ ACADPROD)
 (if (= nil vlax-machine-product-key)
  (setq ACADPROD (vlax-product-key))
  (setq ACADPROD (vlax-machine-product-key))
 )
 (cond ((vl-string-search "\\R17.1\\" ACADPROD)
        "5.0"
       )
       ;;2008
       ((vl-string-search "\\R17.2\\" ACADPROD)
        "6.0"
       )
       ;;2009
       ((vl-string-search "\\R18.0\\" ACADPROD)
        "7.0"
       )
       ;;2010
       ((vl-string-search "\\R18.1\\" ACADPROD)
        "8.0"
       )
       ;;2011
       ((vl-string-search "\\R18.2\\" ACADPROD)
        "9.0"
       )
       ;;2012
       ((vl-string-search "\\R19.0\\" ACADPROD)
        "10.0"
       )
       ;;2013
       ((vl-string-search "\\R19.1\\" ACADPROD)
        "10.3"
       )
       ;;2014
       ((vl-string-search "\\R20.0\\" ACADPROD)
        "10.4"
       )
       ;;2015
       ((vl-string-search "\\R20.1\\" ACADPROD)
        "10.5"
       )
       ;;2016
       ((vl-string-search "\\R21.0\\" ACADPROD)
        "11.0"
       )
       ;;2017
 )
)
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
  (setq C (getint "Enter alignment number : "))
  (setq OBALIGNMENT (vlax-invoke-method C3DALIGNS "Item" (- C 1)))
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
  (setq C (getint "Enter surface number : "))
  (setq OBSURFACE (vlax-get-property C3DSURFS "Item" (- C 1)))
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
(defun RFL:GETSURFACELINE (P1 P2 OBSURFACE / C CATCHERROR OGLINE OGLINELIST VARLIST)
 (setq OGLINE nil)
 (setq VARLIST (list OBSURFACE "SampleElevations" (car P1) (cadr p1) (car P2) (cadr p2)))
 (setq OGLINE (vl-catch-all-apply 'vlax-invoke-method VARLIST))
 (if (not (vl-catch-all-error-p OGLINE))
  (if (/= nil OGLINE)
   (if (/= 0 (vlax-variant-type OGLINE))
    (progn
     (setq OGLINELIST nil)
     (setq OGLINE (vlax-variant-value OGLINE))
     (setq C (vlax-safearray-get-l-bound OGLINE 1))
     (while (<= C (vlax-safearray-get-u-bound OGLINE 1))
      (setq OGLINELIST (append OGLINELIST (list (list (vlax-safearray-get-element OGLINE C)
                                                      (vlax-safearray-get-element OGLINE (+ C 1))
                                                      (vlax-safearray-get-element OGLINE (+ C 2))))))
      (setq C (+ C 3))
     )
    )
   )
  )
 )
 OGLINELIST
)
(defun RFL:GETSURFACEPOINT (P OBSURFACE / VARLIST)
 (setq VARLIST (list OBSURFACE "FindElevationAtXY" (car P) (cadr P)))
 (setq Z (vl-catch-all-apply 'vlax-invoke-method VARLIST))
 (if (vl-catch-all-error-p Z)
  nil
  Z
 )
)
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
  (setq C (getint "Enter alignment number : "))
  (setq OBALIGNMENT (vlax-invoke-method C3DALIGNS "Item" (- C 1)))
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
(defun RFL:GETSECTIONSET (STASTART STAEND SWATH STEP OBSURFACE RFL:ALIGNLIST / P1 P2 PLIST SECTIONSET STA SLIST)
 (princ "\nGetting sections : ")
 (setq STA STASTART)
 (while (<= STA STAEND)
  (princ (strcat "\n" (RFL:STATXT STA) "..."))
  (setq P1 (RFL:XY (list STA (/ SWATH -2.0))))
  (setq P2 (RFL:XY (list STA (/ SWATH 2.0))))
  (if (and (/= nil P1) (/= nil P2))
   (progn
    (setq PLIST (RFL:GETSURFACELINE P1 P2 OBSURFACE))
    (setq SLIST nil)
    (foreach NODE PLIST
     (progn
      (setq P (list (car NODE) (cadr NODE)))
      (setq SLIST (append SLIST (list (list (- (distance P1 P) (/ SWATH 2.0)) (last NODE)))))
     )
    )
    (setq SECTIONSET (append SECTIONSET (list (list STA SLIST))))
   )
  )
  (setq STA (+ STA STEP))
 )
 SECTIONSET
)
