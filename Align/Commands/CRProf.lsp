;
;
;   Program written by Robert Livingston, 98/05/13
;
;   C:RPROF reads a vertical alignment from file
;
;
(defun C:RPROF (/ CMDECHO INFILENAME)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (if (= (vl-registry-read "HKEY_CURRENT_USER\\rflAlignDirectory") nil)
  (vl-registry-write "HKEY_CURRENT_USER\\rflAlignDirectory" "" "")
 )
 (setq INFILENAME (getfiled "Select a Vertical Alignment File" (vl-registry-read "HKEY_CURRENT_USER\\rflAlignDirectory") "vrt" 2))
 (RFL:RPROF INFILENAME)
 (setvar "CMDECHO" CMDECHO)
)
