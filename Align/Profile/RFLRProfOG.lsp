;
;
;   Program written by Robert Livingston, 98/05/13
;
;   RFL:RPROFOG reads a vertical alignment from file INFILENAME and sets the global variable OGLIST
;
;
(defun RFL:RPROFOG (INFILENAME / INFILE INLINE PVIENT PVISET STA ELEV LR VAL)
 (if (/= INFILENAME nil) (setq INFILENAME (findfile INFILENAME)))
 (if (/= INFILENAME nil)
  (progn
   (setq INFILE (open INFILENAME "r"))
   (setq OGLIST nil)
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
      (setq OGLIST (append OGLIST (list (list STA ELEV))))
     )
    )
   )
   (close INFILE)
  )
 )
)