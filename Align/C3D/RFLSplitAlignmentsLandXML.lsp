;
;
;     Program written by Robert Livingston, 2024-11-20
;
;     RFL:SPLITALIGNMENTSLANDXML creates multiple XML files based on the input XML
;
;          Thanks to Jeff Mishler (jeffm@quuxsoft.com) Aug. 2020
;           and VovKa at https://www.theswamp.org/index.php?topic=33065.0
;
;
(defun C:SPLITALIGNMENTSLANDXML (/ INFILENAME)
 (setq INFILENAME (getfiled "Select an XML file" "" "xml" 2))
 (RFL:SPLITALIGNMENTSLANDXML INFILENAME)
 nil
)
(defun RFL:SPLITALIGNMENTSLANDXML (INFILENAME / *error* C CMAX DOC NAME NAMELIST NEWDOC NODE NODELIST NODES OUTFILENAME)
;(defun RFL:SPLITALIGNMENTSLANDXML (INFILENAME)
 (setq NAMELIST nil)
 (if (and INFILENAME
          (setq DOC (vlax-create-object "MSXML.DOMDocument"))
          (not (vlax-put DOC "async" 0))
          (if (= (vlax-invoke DOC "load" INFILENAME) -1)
           T
           (prompt
	        (strcat "\nError: "
		     (vlax-get (vlax-get DOC "parseError") "reason")
	        )
	       )
          )
          (= (vlax-get DOC "readyState") 4)
     )
  (progn
   (setq NODES (vlax-invoke DOC "GetElementsByTagName" "Alignment"))
   (setq C 0)
   (setq CMAX (vlax-get NODES "Length"))
   (while (< C CMAX)
    (setq NODE (vlax-get-property NODES "Item" C))
    (setq NAMELIST (append NAMELIST (list (vlax-get (vlax-get-property (vlax-get-property NODE "Attributes") "Item" 0) "Value"))))
    (setq C (1+ C))
   )
   (vlax-release-object DOC)
   (foreach NAME NAMELIST
    (progn
     (setq OUTFILENAME (strcat (getenv "UserProfile") "\\Documents\\" NAME ".xml"))
     (setq NEWDOC (vlax-create-object "MSXML.DOMDocument"))
     (vlax-invoke-method NEWDOC "Load" INFILENAME)
     (setq NODELIST nil)
     (setq NODES (vlax-invoke-method NEWDOC "GetElementsByTagName" "Alignment"))
     (setq C 0)
     (setq CMAX (vlax-get NODES "Length"))
     (while (< C CMAX)
      (setq NODE (vlax-get-property NODES "Item" C))
      (if (/= NAME (vlax-get (vlax-get-property (vlax-get-property NODE "Attributes") "Item" 0) "Value"))
       (setq NODELIST (append NODELIST (list NODE)))
      )
      (setq C (1+ C))
     )
     (foreach NODE NODELIST
      (progn
       (vlax-invoke-method (vlax-get NODE "parentNode") "removeChild" NODE)
      )
     )
     (vlax-invoke-method NEWDOC "Save" OUTFILENAME)
     (vlax-release-object NEWDOC)
    )
   )
  )
 )
 (print (strcat "!!! Alignment files located in " (getenv "UserProfile") "\\Documents\\" " !!!"))
 nil
)