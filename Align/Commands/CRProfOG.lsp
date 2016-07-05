;
;
;   Program written by Robert Livingston, 98/05/13
;
;   C:RPROFOG reads an OG vertical alignment from file and sets the global variable OGLIST
;
;
(defun C:RPROFOG (/ CMDECHO INFILENAME)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (setq INFILENAME (getfiled "Select an OG Vertical Alignment File" "" "vrt" 2))
 (RFL:RPROFOG INFILENAME)
 (setvar "CMDECHO" CMDECHO)
)
