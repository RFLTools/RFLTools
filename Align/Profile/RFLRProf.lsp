;
;
;   Program written by Robert Livingston, 98/05/13
;
;   RPROF reads a vertical alignment from file INFILENAME and sets the global variable RFL:PVILIST
;
;
(defun RFL:RPROF (INFILENAME / INFILE INLINE PVIENT PVISET STA ELEV LR VAL)
 (if (/= INFILENAME nil) (setq INFILENAME (findfile INFILENAME)))
 (if (/= INFILENAME nil)
  (progn
   (vl-registry-write "HKEY_CURRENT_USER\\rflAlignDirectory" "" (strcat (vl-filename-directory INFILENAME) "\\"))
   (setq INFILE (open INFILENAME "r"))
   (setq RFL:PVILIST nil)
   (setq INLINE (read-line INFILE))
   (if (/= INLINE "#RFL VERTICAL ALIGNMENT FILE")
    (progn
     (princ "\n*** FILE NOT FORMATTED CORRECTLY ***\n")
    )
    (progn
     (setq INLINE (read-line INFILE))
     (while (and (/= nil INLINE) (/= INLINE "#END DEFINITION"))
      (setq STA (atof INLINE))
      (setq INLINE (read-line INFILE))
      (setq ELEV (atof INLINE))
      (setq INLINE (read-line INFILE))
      (setq LR INLINE)
      (setq INLINE (read-line INFILE))
      (setq VAL (atof INLINE))
      (setq INLINE (read-line INFILE))
      (setq RFL:PVILIST (append RFL:PVILIST (list (list STA ELEV LR VAL))))
     )
    )
   )
   (close INFILE)
  )
 )
)
