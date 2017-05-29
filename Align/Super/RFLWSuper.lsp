;
;
;   Program written by Robert Livingston, 99/10/08
;
;   RFL:WSUPER writes the superelevation to file
;
;
(defun RFL:WSUPER (OUTFILENAME / C OUTFILE)
 (if (/= OUTFILENAME nil)
  (progn
   (if (/= ".E" (strcase (substr OUTFILENAME (- (strlen OUTFILENAME) 1))))
    (setq OUTFILENAME (strcat OUTFILENAME ".e"))
   )
   (vl-registry-write "HKEY_CURRENT_USER\\rflAlignDirectory" "" (strcat (vl-filename-directory OUTFILENAME) "\\"))
   (setq OUTFILE (open OUTFILENAME "w"))
   (princ "#RFL SUPERELEVATION FILE\n" OUTFILE)
   (setq C 0)
   (while (< C (length RFL:SUPERLIST))
    (princ (rtos (nth 0 (nth C RFL:SUPERLIST)) 2 16) OUTFILE)
    (princ "\n" OUTFILE)
    (princ (rtos (nth 1 (nth C RFL:SUPERLIST)) 2 16) OUTFILE)
    (princ "\n" OUTFILE)
    (princ (rtos (nth 2 (nth C RFL:SUPERLIST)) 2 16) OUTFILE)
    (princ "\n" OUTFILE)
    (setq C (+ C 1))
   )
   (princ "#END DEFINITION\n" OUTFILE)
   (close OUTFILE)
   T
  )
  nil
 )
)
