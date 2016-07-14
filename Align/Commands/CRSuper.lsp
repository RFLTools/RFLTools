;
;
;   Program written by Robert Livingston, 99/10/08
;
;   RSUPER reads the Superelevation from file
;
;
(defun C:RSUPER (/ CMDECHO INFILENAME)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (if (= (vl-registry-read "HKEY_CURRENT_USER\\rflAlignDirectory") nil)
  (vl-registry-write "HKEY_CURRENT_USER\\rflAlignDirectory" "" "")
 )
 (setq INFILENAME (getfiled "Select a Superelevation File" (vl-registry-read "HKEY_CURRENT_USER\\rflAlignDirectory") "e" 2))
 (RFL:RSUPER INFILENAME)
 (setvar "CMDECHO" CMDECHO)
 nil
)
