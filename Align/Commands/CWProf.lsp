;
;
;   Program written by Robert Livingston, 98/05/13
;
;   C:WPROF writes a vertical alignment to file
;
;
(defun C:WPROF (/ CMDECHO OUTFILENAME)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (if (= (vl-registry-read "HKEY_CURRENT_USER\\rflAlignDirectory") nil)
  (vl-registry-write "HKEY_CURRENT_USER\\rflAlignDirectory" "" "")
 )
 (if (= PVILIST nil)
  (princ "\n*** NO VERTICAL EXISTS - USE RPROF OR GPROF ***\n")
  (progn
   (setq OUTFILENAME (getfiled "Select a Vertical Alignment File" (vl-registry-read "HKEY_CURRENT_USER\\rflAlignDirectory") "vrt" 1))
   (RFL:WPROF OUTFILENAME)
  )
 )
 (setvar "CMDECHO" CMDECHO)
)
