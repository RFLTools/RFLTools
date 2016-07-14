;
;
;   Program written by Robert Livingston, 99/10/08
;
;   RFL:RSUPER reads the Superelevation from file
;
;
(defun RFL:RSUPER (INFILENAME / INFILE INLINE STA SUPERLEFT SUPERRIGHT)
 (if (/= INFILENAME nil) (setq INFILENAME (findfile INFILENAME)))
 (if (/= INFILENAME nil)
  (progn
   (vl-registry-write "HKEY_CURRENT_USER\\rflAlignDirectory" "" (strcat (vl-filename-directory INFILENAME) "\\"))
   (setq INFILE (open INFILENAME "r"))
   (setq RFL:SUPERLIST nil)
   (setq INLINE (read-line INFILE))
   (if (/= INLINE "#RFL SUPERELEVATION FILE")
    (progn
     (princ "\n*** FILE NOT FORMATTED CORRECTLY ***\n")
    )
    (progn
     (setq INLINE (read-line INFILE))
     (while (and (/= nil INLINE) (/= INLINE "#END DEFINITION"))
      (setq STA (atof INLINE))
      (setq INLINE (read-line INFILE))
      (setq SUPERLEFT (atof INLINE))
      (setq INLINE (read-line INFILE))
      (setq SUPERRIGHT (atof INLINE))
      (setq INLINE (read-line INFILE))
      (setq RFL:SUPERLIST (append RFL:SUPERLIST (list (list STA SUPERLEFT SUPERRIGHT))))
     )
    )
   )
   (close INFILE)
   T
  )
  nil
 )
)
