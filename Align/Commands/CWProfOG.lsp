;
;
;   Program written by Robert Livingston, 98/05/13
;
;   C:WPROFOG writes a vertical alignment to file
;
;
(defun C:WPROFOG (/ C CMDECHO OUTFILE OUTFILENAME)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (if (= OGLIST nil)
  (princ "\n*** NO OG EXISTS - USE GPROFOG ***\n")
  (progn
   (setq OUTFILENAME (getfiled "Select a Vertical OG File" "" "vrt" 1))
   (RFL:WPROFOG OUTFILENAME)
  )
 )

 (setvar "CMDECHO" CMDECHO)
)
