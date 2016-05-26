;
;
;   Program written by Robert Livingston, 98/06/11
;
;   RALIGN reads a horizontal alignment from file
;
;
(defun C:RALIGN (/ ANGBASE ANGDIR CMDECHO INFILENAME)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 0)

 (if (= (vl-registry-read "HKEY_CURRENT_USER\\rflAlignDirectory") nil)
  (vl-registry-write "HKEY_CURRENT_USER\\rflAlignDirectory" "" "")
 )
 (setq INFILENAME (getfiled "Select a Horizontal Alignment File" (vl-registry-read "HKEY_CURRENT_USER\\rflAlignDirectory") "hor" 2))
 (RFL:RALIGN INFILENAME)
 (setvar "CMDECHO" CMDECHO)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
)
