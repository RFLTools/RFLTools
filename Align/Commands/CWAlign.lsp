;
;
;   Program written by Robert Livingston, 98/06/11
;
;   WALIGN writes a horizontal alignment to file
;
;
(defun C:WALIGN (/ CMDECHO OUTFILENAME)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (if (= (vl-registry-read "HKEY_CURRENT_USER\\rflAlignDirectory") nil)
  (vl-registry-write "HKEY_CURRENT_USER\\rflAlignDirectory" "" "")
 )
 (if (= RFL:ALIGNLIST nil)
  (princ "\n*** NO ALIGNMENT EXISTS - USE RALIGN OR GALIGN ***\n")
  (progn
   (setq OUTFILENAME (getfiled "Select a Horizontal Alignment File" (vl-registry-read "HKEY_CURRENT_USER\\rflAlignDirectory") "hor" 1))
   (RFL:WALIGN OUTFILENAME)
  )
 )
 (setvar "CMDECHO" CMDECHO)
)
