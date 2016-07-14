;
;
;   Program written by Robert Livingston, 99/10/08
;
;   WSUPER writes the superelevation to file
;
;
(defun C:WSUPER (/ CMDECHO OUTFILENAME)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (if (= (vl-registry-read "HKEY_CURRENT_USER\\rflAlignDirectory") nil)
  (vl-registry-write "HKEY_CURRENT_USER\\rflAlignDirectory" "" "")
 )
 (if (= SUPERLIST nil)
  (princ "\n*** NO SUPERELEVATION EXISTS - USE RSUPER OR GSUPER ***\n")
  (progn
   (setq OUTFILENAME (getfiled "Select a Superelevation File" (vl-registry-read "HKEY_CURRENT_USER\\rflAlignDirectory") "e" 1))
   (RFL:WSUPER OUTFILENAME)
  )
 )
 (setvar "CMDECHO" CMDECHO)
 nil
)
