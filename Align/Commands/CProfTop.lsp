;
;
;     Program written by Robert Livingston, 2022-01-05
;
;     PROFTOP is a set of utilities for creating BCMoTI profile top quantities
;
;
(defun RFL:GETMATERIALSXML (INFILENAME / AeccEarthworkXMLLIST AlignmentLIST API-ERROR ATTRIBUTES CROSSSECTS MATERIALLIST OB OBMATERIALLIST OBSECTIONLIST OBXMLDOC TEMP1 TEMP2 TEMP3 TEMP4 TEMP5 TEMP6 TEMP7 TEMP8 XMLDOC XML-GET-CHILDLIST XML-GET-CHILDREN XML-GET-DOCUMENT)
 (defun API-ERROR (func lst bool / trap)
  (cond ((vl-catch-all-error-p (setq trap (vl-catch-all-apply func lst)))
         (if bool (princ (strcat "\nVLISP XML Error: " (vl-catch-all-error-message trap))))
         (setq trap nil)
        )
  )
  trap
 )
 (defun XML-GET-DOCUMENT (file XMLDoc)
  (if (findfile file)
   (progn
    (set XMLDoc (vlax-create-object "MSXML2.DOMDocument.3.0")) ;;create XML-DOM pipeline
    (vlax-put-property (eval XMLDoc) "async" :vlax-false)
    (cond ((api-error 'vlax-invoke-method (list (eval XMLDoc) "Load" file) T) ;; Load Project File into XML-DOM pipeline
           (eval XMLDoc)
          )
    )
   )
   (alert "\nXML Document could not be found.")
  )
 )
 (defun XML-GET-CHILDREN (oXML parentName / return)
  (cond ((/= parentName nil)
         (if (vlax-invoke-method (XML-Get-Parent oXML parentName) 'hasChildNodes)
          (setq return (vlax-get-property (XML-Get-Parent oXML parentName) 'childNodes))
         )
        )
        (T
         (if (vlax-invoke-method oXML 'hasChildNodes) (setq return (vlax-get-property oXML 'childNodes)))
        )
  )
  return
 )
 (defun XML-GET-CHILDLIST (oXML / collection child lst)
  (cond ((vlax-invoke-method oXML 'hasChildNodes)
         (setq collection (XML-Get-Children oXML nil))
         (while (setq child (vlax-invoke-method collection 'nextNode))
          (setq lst (if lst (cons child lst) (list child)))
         )
         (reverse lst)
        )
   (princ "\nObject has no children")
  )
 )
 (vl-load-com)
 (setq XMLDOC (vlax-create-object "MSXML2.DOMDocument.3.0"))
 (vlax-put-property (eval XMLDOC) "async" :vlax-false)
 (setq OBXMLDOC (XML-GET-DOCUMENT INFILENAME 'XMLDOC))
 (setq TEMP1 (car (XML-GET-CHILDLIST OBXMLDOC)))                               ; XML Version / Encoding
; (setq TEMP2 (cadr (XML-GET-CHILDLIST OBXMLDOC)))                              ; AeccEarthworkXML
 (setq TEMP2 (last (XML-GET-CHILDLIST OBXMLDOC)))                              ; AeccEarthworkXML
 (setq AeccEarthworkXMLLIST (XML-GET-CHILDLIST TEMP2))                         ; 
 (setq TEMP3 (nth 0 AeccEarthworkXMLLIST))                                     ; Project
 (setq TEMP4 (nth 1 AeccEarthworkXMLLIST))                                     ; Application
 (setq TEMP5 (nth 2 AeccEarthworkXMLLIST))                                     ; Settings
 (setq TEMP6 (nth 3 AeccEarthworkXMLLIST))                                     ; Alignment
 (setq AlignmentLIST (XML-GET-CHILDLIST TEMP6))                                ; 
 (setq CROSSSECTS (car AlignmentLIST))                                         ; CrossSects
 (setq OBSECTIONLIST (XML-GET-CHILDLIST CROSSSECTS))                           ; Section List, first is Material
 (setq TEMP8 (nth 0 OBSECTIONLIST))                                            ; 
 (setq OBMATERIALLIST (XML-GET-CHILDLIST TEMP8))                               ; Material List
 (setq MATERIALLIST nil)
 (foreach OB OBMATERIALLIST
  (setq ATTRIBUTES (vlax-get-property OB "attributes"))
  (setq MATERIALLIST (append MATERIALLIST (list (vlax-get-property (vlax-get-property ATTRIBUTES "item" 0) "text"))))
 )
 MATERIALLIST
)
(defun RFL:GETSECTIONSXML (INFILENAME / AeccEarthworkXMLLIST AlignmentLIST API-ERROR AREACUT AREAFILL ATTRIBUTES C CROSSSECTS MATERIALLIST MATERIALAREA MATERIALNAME MATERIALSECTIONLIST OB OBMATERIALSECTIONLIST OBSECTIONLIST OBXMLDOC SECTIONLIST SECTNO STA TEMP1 TEMP2 TEMP3 TEMP4 TEMP5 TEMP6 TEMP7 TEMP8 TEMP9 XMLDOC XML-GET-CHILDREN XML-GET-CHILDLIST XML-GET-DOCUMENT)
 ; Thanks to Columbia at TheSwamp, https://www.theswamp.org/index.php?topic=525.msg277319#msg277319
 (defun API-ERROR (func lst bool / trap)
  (cond ((vl-catch-all-error-p (setq trap (vl-catch-all-apply func lst)))
         (if bool (princ (strcat "\nVLISP XML Error: " (vl-catch-all-error-message trap))))
         (setq trap nil)
        )
  )
  trap
 )
 (defun XML-GET-DOCUMENT (file XMLDoc)
  (if (findfile file)
   (progn
    (set XMLDoc (vlax-create-object "MSXML2.DOMDocument.3.0")) ;;create XML-DOM pipeline
    (vlax-put-property (eval XMLDoc) "async" :vlax-false)
    (cond ((api-error 'vlax-invoke-method (list (eval XMLDoc) "Load" file) T) ;; Load Project File into XML-DOM pipeline
           (eval XMLDoc)
          )
    )
   )
   (alert "\nXML Document could not be found.")
  )
 )
 (defun XML-GET-CHILDREN (oXML parentName / return)
  (cond ((/= parentName nil)
         (if (vlax-invoke-method (XML-Get-Parent oXML parentName) 'hasChildNodes)
          (setq return (vlax-get-property (XML-Get-Parent oXML parentName) 'childNodes))
         )
        )
        (T
         (if (vlax-invoke-method oXML 'hasChildNodes) (setq return (vlax-get-property oXML 'childNodes)))
        )
  )
  return
 )
 (defun XML-GET-CHILDLIST (oXML / collection child lst)
  (cond ((vlax-invoke-method oXML 'hasChildNodes)
         (setq collection (XML-Get-Children oXML nil))
         (while (setq child (vlax-invoke-method collection 'nextNode))
          (setq lst (if lst (cons child lst) (list child)))
         )
         (reverse lst)
        )
   (princ "\nObject has no children")
  )
 )
 (vl-load-com)
 (setq XMLDOC (vlax-create-object "MSXML2.DOMDocument.3.0"))
 (vlax-put-property (eval XMLDOC) "async" :vlax-false)
 ;(setq INFILENAME (getfiled "Select XML file for input" "" "xml" 2))
 (setq OBXMLDOC (XML-GET-DOCUMENT INFILENAME 'XMLDOC))
 (setq TEMP1 (car (XML-GET-CHILDLIST OBXMLDOC)))                               ; XML Version / Encoding
; (setq TEMP2 (cadr (XML-GET-CHILDLIST OBXMLDOC)))                              ; AeccEarthworkXML
 (setq TEMP2 (last (XML-GET-CHILDLIST OBXMLDOC)))                              ; AeccEarthworkXML
 (setq AeccEarthworkXMLLIST (XML-GET-CHILDLIST TEMP2))                         ; 
 (setq TEMP3 (nth 0 AeccEarthworkXMLLIST))                                     ; Project
 (setq TEMP4 (nth 1 AeccEarthworkXMLLIST))                                     ; Application
 (setq TEMP5 (nth 2 AeccEarthworkXMLLIST))                                     ; Settings
 (setq TEMP6 (nth 3 AeccEarthworkXMLLIST))                                     ; Alignment
 (setq AlignmentLIST (XML-GET-CHILDLIST TEMP6))                                ; 
 (setq CROSSSECTS (car AlignmentLIST))                                         ; CrossSects
 (setq OBSECTIONLIST (XML-GET-CHILDLIST CROSSSECTS))                           ; Section List, first is Material
 (setq TEMP8 (nth 0 OBSECTIONLIST))                                            ; 
 (setq OBMATERIALLIST (XML-GET-CHILDLIST TEMP8))                               ; Material List
 (setq MATERIALLIST nil)
 (foreach OB OBMATERIALLIST
  (setq ATTRIBUTES (vlax-get-property OB "attributes"))
  (setq MATERIALLIST (append MATERIALLIST (list (vlax-get-property (vlax-get-property ATTRIBUTES "item" 0) "text"))))
 )
 (setq OBSECTIONLIST (cdr OBSECTIONLIST))
 (setq SECTIONLIST nil)
 (foreach OB OBSECTIONLIST
  (setq ATTRIBUTES (vlax-get-property OB "attributes"))
  (setq C (1- (vlax-get-property ATTRIBUTES "length")))
  (setq SECTNO nil STA nil AREACUT nil AREAFILL nil)
  (while (>= C 0)
   (cond ((= (vlax-get-property (vlax-get-property ATTRIBUTES "item" C) "name") "number")
          (setq SECTNO (atoi (vlax-get-property (vlax-get-property ATTRIBUTES "item" C) "text")))
         )
         ((= (vlax-get-property (vlax-get-property ATTRIBUTES "item" C) "name") "sta")
          (setq STA (atof (vlax-get-property (vlax-get-property ATTRIBUTES "item" C) "text")))
         )
         ((= (vlax-get-property (vlax-get-property ATTRIBUTES "item" C) "name") "areaCut")
          (setq AREACUT (atof (vlax-get-property (vlax-get-property ATTRIBUTES "item" C) "text")))
         )
         ((= (vlax-get-property (vlax-get-property ATTRIBUTES "item" C) "name") "areaFill")
          (setq AREAFILL (atof (vlax-get-property (vlax-get-property ATTRIBUTES "item" C) "text")))
         )
   )
   (setq C (1- C))
  )
  (setq TEMP9 (car (XML-GET-CHILDLIST OB)))
  (setq MATERIALSECTIONLIST nil)
  (setq OBMATERIALSECTIONLIST (XML-GET-CHILDLIST TEMP9))
  (foreach OB2 OBMATERIALSECTIONLIST
   (setq ATTRIBUTES (vlax-get-property OB2 "attributes"))
   (setq C (1- (vlax-get-property ATTRIBUTES "length")))
   (setq MATERIALNAME nil MATERIALAREA nil)
   (while (>= C 0)
    (cond ((= (vlax-get-property (vlax-get-property ATTRIBUTES "item" C) "name") "name")
           (setq MATERIALNAME (vlax-get-property (vlax-get-property ATTRIBUTES "item" C) "text"))
          )
          ((= (vlax-get-property (vlax-get-property ATTRIBUTES "item" C) "name") "area")
           (setq MATERIALAREA (atof (vlax-get-property (vlax-get-property ATTRIBUTES "item" C) "text")))
          )
    )
    (setq C (1- C))
   )
   (setq MATERIALSECTIONLIST (append MATERIALSECTIONLIST (list (list MATERIALNAME MATERIALAREA))))
  )
  (setq SECTIONLIST (append SECTIONLIST (list (list SECTNO STA AREACUT AREAFILL MATERIALSECTIONLIST))))
 )
 (vlax-release-object XMLDOC)
 SECTIONLIST
)
(defun RFL:GETMATAREA (MAT MATLIST / AREA TMP)
 (setq AREA nil)
 (foreach TMP MATLIST
  (if (= (strcase MAT) (strcase (car TMP))) (setq AREA (cadr TMP)))
 )
 AREA
)
(defun RFL:GETAREA (SECTIONLIST STA / A AREA C CUT CUT1 CUT2 FILL FILL1 FILL2 MAT MATLIST MATLIST1 MATLIST2 NODE1 NODE2 STA1 STA2 TMP)
 (setq AREA nil)
 (if (and SECTIONLIST (>= STA (cadr (car SECTIONLIST))) (<= STA (cadr (last SECTIONLIST))))
  (progn
   (setq C 0)
   (while (and SECTIONLIST (< C (length SECTIONLIST)) (> STA (cadr (nth C SECTIONLIST)))) (setq C (1+ C)))
   (if (= C 0)
    (setq STA1 (cadr (setq NODE1 (nth 0 SECTIONLIST))))
    (setq STA1 (cadr (setq NODE1 (nth (1- C) SECTIONLIST))))
   )
   (if (= C (length SECTIONLIST))
    (setq STA2 (cadr (setq NODE2 (last SECTIONLIST))))
    (setq STA2 (cadr (setq NODE2 (nth C SECTIONLIST))))
   )
   (setq CUT1 (caddr NODE1)
         FILL1 (cadddr NODE1)
         MATLIST1 (last NODE1)
         CUT2 (caddr NODE2)
         FILL2 (cadddr NODE2)
         MATLIST2 (last NODE2)
   )
   (if (= STA1 STA2)
    (setq CUT CUT1 FILL FILL1 MATLIST MATLIST1)
    (progn
     (setq CUT (+ CUT1 (* (- CUT2 CUT1) (/ (- STA STA1) (- STA2 STA1)))))
     (setq FILL (+ FILL1 (* (- FILL2 FILL1) (/ (- STA STA1) (- STA2 STA1)))))
     (setq MATLIST nil)
     (foreach MAT MATLIST1
      (if (setq TMP (RFL:GETMATAREA (car MAT) MATLIST2))
       (progn
        (setq A (+ (cadr MAT) (* (- TMP (cadr MAT)) (/ (- STA STA1) (- STA2 STA1)))))
        (setq MATLIST (append MATLIST (list (list (car MAT) A))))
       )
      )
     )
    )
   )
   (setq AREA (list CUT FILL MATLIST))
  )
 )
 AREA
)
(defun RFL:GETVOLUME (SECTIONLIST STA1 STA2 / AREA AREA1 AREA2 CUTVOL FILLVOL LASTNODE MAT MATLIST MATLIST1 MATLIST2 NODE TMP X V VLIST VOLUME)
 (setq VOLUME nil)
 (if (and SECTIONLIST STA1 STA2 (< STA1 STA2) (>= STA1 (cadr (car SECTIONLIST))) (<= STA2 (cadr (last SECTIONLIST))))
  (if (and (setq AREA1 (RFL:GETAREA SECTIONLIST STA1)) (setq AREA2 (RFL:GETAREA SECTIONLIST STA2)))
   (progn
    (while (and SECTIONLIST (> STA1 (cadr (car SECTIONLIST)))) (setq SECTIONLIST (cdr SECTIONLIST)))
    (setq SECTIONLIST (reverse SECTIONLIST))
    (while (and SECTIONLIST (< STA2 (cadr (car SECTIONLIST)))) (setq SECTIONLIST (cdr SECTIONLIST)))
    (setq SECTIONLIST (reverse SECTIONLIST))
    (setq AREA1 (append (list 0 STA1) AREA1))
    (setq AREA2 (append (list 0 STA2) AREA2))
    (if SECTIONLIST
     (setq SECTIONLIST (append (list AREA1) SECTIONLIST (list AREA2)))
     (setq SECTIONLIST (append (list AREA1) (list AREA2)))
    )
    (setq LASTNODE (car SECTIONLIST))
    (setq SECTIONLIST (cdr SECTIONLIST))
    (while SECTIONLIST
     (setq NODE (car SECTIONLIST))
     (setq SECTIONLIST (cdr SECTIONLIST))
     (setq X (- (cadr NODE) (cadr LASTNODE)))
     (setq CUTVOL (* X (/ (+ (nth 2 LASTNODE) (nth 2 NODE)) 2.0)))
     (setq FILLVOL (* X (/ (+ (nth 3 LASTNODE) (nth 3 NODE)) 2.0)))
     (setq MATLIST1 (nth 4 LASTNODE))
     (setq MATLIST2 (nth 4 NODE))
     (setq MATLIST nil)
     (foreach MAT MATLIST1
      (if (setq TMP (RFL:GETMATAREA (car MAT) MATLIST2))
       (progn
        (setq V (* X (/ (+ (cadr MAT) TMP) 2.0)))
        (setq MATLIST (append MATLIST (list (list (car MAT) V))))
       )
      )
     )
     (if VOLUME
      (progn
       (setq VLIST nil)
       (foreach MAT (nth 2 VOLUME)
        (if (setq TMP (RFL:GETMATAREA (car MAT) MATLIST))
         (setq VLIST (append VLIST (list (list (car MAT) (+ (cadr MAT) TMP)))))
        )
       )
       (setq VOLUME (list (+ CUTVOL (nth 0 VOLUME))
                          (+ FILLVOL (nth 1 VOLUME))
                          VLIST
                    )
       )
      )
      (setq VOLUME (list CUTVOL
                         FILLVOL
                         MATLIST
                   )
      )
     )
     (setq LASTNODE NODE)
    )
   )
  )
 )
 VOLUME
)
(defun RFL:GETMATVOLUME (MATERIAL SECTIONLIST STA1 STA2 / NODE TMP VOLUME VOLUMELIST)
 (setq VOLUME nil)
 (if (setq VOLUMELIST (RFL:GETVOLUME SECTIONLIST STA1 STA2))
  (cond ((= (strcase MATERIAL) "CUT")
         (setq VOLUME (car VOLUMELIST))
        )
        ((= (strcase MATERIAL) "FILL")
         (setq VOLUME (cadr VOLUMELIST))
        )
        (T
         (foreach NODE (last VOLUMELIST)
          (if (= (strcase (car NODE)) (strcase MATERIAL)) (setq VOLUME (cadr NODE)))
         )
        )
        
  )
 )
 VOLUME
)
(defun C:UPDATEXMLVOLUME (/ ENTSET)
 (command "._UNDO" "M")
 (setq ENTSET (ssget))
 (RFL:UPDATEXMLVOLUME ENTSET)
 nil
)
(defun RFL:UPDATEXMLVOLUME (ENTSET /  C ENT INFILENAME MATERIAL SECTIONLIST STA1 STA2 TMP VOLUME VOLUMELIST)
 (defun GETMATVOLUME (MATLIST MATERIAL / NODE VOLUME)
  (setq VOLUME nil)
  (foreach NODE MATLIST
   (if (= (strcase (car NODE)) (strcase MATERIAL)) (setq VOLUME (cadr NODE)))
  )
  VOLUME
 )
 (if ENTSET
  (progn
   (setq STA1 nil)
   (setq STA2 nil)
   (setq INFILENAME nil)
   (setq MATERIAL nil)
   (setq C 0)
   (while (< C (sslength ENTSET))
    (setq ENT (ssname ENTSET C))
    (if (/= INFILENAME (RFL:GETATTVALUE ENT "FILE"))
     (progn
      (setq INFILENAME (RFL:GETATTVALUE ENT "FILE"))
      (if (findfile INFILENAME)
       (setq SECTIONLIST (RFL:GETSECTIONSXML INFILENAME))
       (setq SECTIONLIST nil)
      )
     )
    )
    (if SECTIONLIST
     (if (and (setq STA1 (RFL:GETATTVALUE ENT "START"))
              (setq STA2 (RFL:GETATTVALUE ENT "END"))
              (setq MATERIAL (RFL:GETATTVALUE ENT "MATERIAL"))
         )
      (progn
       (setq STA1 (atof STA1))
       (setq STA2 (atof STA2))
       (if (setq VOLUMELIST (RFL:GETVOLUME SECTIONLIST STA1 STA2))
        (cond ((= (strcase MATERIAL) "CUT")
               (RFL:PUTATTVALUE ENT "VOLUME" (rtos (car VOLUMELIST) 2 0))
              )
              ((= (strcase MATERIAL) "FILL")
               (RFL:PUTATTVALUE ENT "VOLUME" (rtos (cadr VOLUMELIST) 2 0))
              )
              ((setq TMP (GETMATVOLUME (last VOLUMELIST) MATERIAL))
               (RFL:PUTATTVALUE ENT "VOLUME" (rtos TMP 2 0))
              )
              (T
               (RFL:PUTATTVALUE ENT "VOLUME" "?")
              )
        )
       )
      )
     )
    )
    (setq C (1+ C))
   )
  )
 )
 nil
)
(defun C:UPDATEXMLFILENAME (/ C ENT ENTSET INFILENAME)
 (if (setq INFILENAME (getfiled "Select XML file for input" "" "xml" 2))
  (progn
   (setq C 0)
   (setq ENTSET (ssget))
   (while (< C (sslength ENTSET))
    (setq ENT (ssname ENTSET C))
    (RFL:PUTATTVALUE ENT "FILE" INFILENAME)
    (setq C (1+ C))
   )
  )
 )
 nil
)