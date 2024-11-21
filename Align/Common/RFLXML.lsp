;
;
;   Program written by Robert Livingston, 2024-11-20
;
;   RFL:XML is a collection of routines to work with XML files.
;
;   Credit to VovKa at https://www.theswamp.org/index.php?topic=33065.0 for ideas and sample code.
;
;
;(setq INFILENAME (getfiled "Select an XML file" "" "xml" 2))
(defun RFL:READXML (INFILENAME / *error* DOC OUTLIST)
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
  (setq OUTLIST (RFL:XMLGETCHILDNODES (vlax-get DOC "firstChild")))
 )
 (and DOC (vlax-release-object DOC))
 (gc)
 OUTLIST
)
(defun RFL:XMLGETCHILDNODES (NODE / )
 (if NODE
  (if (= (vlax-get NODE "nodeType") 3)
   (vlax-get NODE "nodeValue")
   (cons (list (vlax-get NODE "nodeName")
               (RFL:XMLGETATTRIBUTES NODE)
               (RFL:XMLGETCHILDNODES (vlax-get NODE "firstChild"))
         )
         (RFL:XMLGETCHILDNODES (vlax-get NODE "nextSibling"))
   )
  )
 )
)
(defun RFL:XMLGETATTRIBUTES (NODE / ATTRIBUTES ATTRIBUTE OUTLIST)
 (if (setq ATTRIBUTES (vlax-get NODE "attributes"))
  (progn
   (while (setq ATTRIBUTE (vlax-invoke ATTRIBUTES "nextNode"))
    (setq OUTLIST (cons (cons (vlax-get ATTRIBUTE "nodeName")
                              (vlax-get ATTRIBUTE "nodeValue")
                        )
                        OUTLIST
                  )
    )
    (vlax-release-object ATTRIBUTE)
   )
   (vlax-release-object ATTRIBUTES)
   (reverse OUTLIST)
  )
 )
)
