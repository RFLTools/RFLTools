;
;
;   Program written by Robert Livingston, 98/05/13
;
;   RFL:WPROF writes a vertical alignment to file
;
;
(defun RFL:WPROF (OUTFILENAME / C OUTFILE)
 (if (/= OUTFILENAME nil)
  (progn
   (if (/= ".VRT" (strcase (substr OUTFILENAME (- (strlen OUTFILENAME) 3))))
    (setq OUTFILENAME (strcat OUTFILENAME ".VRT"))
   )
   (vl-registry-write "HKEY_CURRENT_USER\\rflAlignDirectory" "" (strcat (vl-filename-directory OUTFILENAME) "\\"))
   (setq C 0)
   (while (and (= nil (setq OUTFILE (open OUTFILENAME "w"))) (< C 5))
    (setq C (+ C 1))
    (princ (strcat "\nProblem openning file for writing : " (itoa C)))
   )
   (if (= nil OUTFILE)
    (alert (strcat "Error openning file for writing : " OUTFILENAME))
    (progn
     (princ "#RFL VERTICAL ALIGNMENT FILE\n" OUTFILE)
     (setq C 0)
     (while (< C (length RFL:PVILIST))
      (princ (rtos (nth 0 (nth C RFL:PVILIST)) 2 16) OUTFILE)
      (princ "\n" OUTFILE)
      (princ (rtos (nth 1 (nth C RFL:PVILIST)) 2 16) OUTFILE)
      (princ "\n" OUTFILE)
      (princ (nth 2 (nth C RFL:PVILIST)) OUTFILE)
      (princ "\n" OUTFILE)
      (princ (rtos (nth 3 (nth C RFL:PVILIST)) 2 16) OUTFILE)
      (princ "\n" OUTFILE)
      (setq C (+ C 1))
     )
     (princ "#END DEFINITION\n" OUTFILE)
     (close OUTFILE)
    )
   )
  )
 )
)
