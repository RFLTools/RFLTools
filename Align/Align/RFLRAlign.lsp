;
;
;   Program written by Robert Livingston, 98/06/11
;
;   RFL:RALIGN reads a horizontal alignment from the specifiedfile
;
;
(defun RFL:RALIGN (INFILENAME / ANGBASE ANGDIR CMDECHO INFILE INLINE LO P1X P1Y P2X P2Y
                                PLTX PLTY PLTSTX PLTSTY PSTX PSTY BULGE)
 (if (/= INFILENAME nil) (setq INFILENAME (findfile INFILENAME)))
 (if (/= INFILENAME nil)
  (progn
   (vl-registry-write "HKEY_CURRENT_USER\\rflAlignDirectory" "" (strcat (vl-filename-directory INFILENAME) "\\"))
   (setq INFILE (open INFILENAME "r"))
   (setq ALIGNLIST nil)
   (setq INLINE (read-line INFILE))
   (if (/= INLINE "#RFL HORIZONTAL ALIGNMENT FILE")
    (progn
     (princ "\n*** FILE NOT FORMATTED CORRECTLY ***\n")
    )
    (progn
     (setq INLINE (read-line INFILE))
     (while (and (/= nil INLINE) (/= INLINE "#END DEFINITION"))
      (setq STA (atof INLINE))
      (setq INLINE (read-line INFILE))
      (setq P1X (atof INLINE))
      (setq INLINE (read-line INFILE))
      (setq P1Y (atof INLINE))
      (setq INLINE (read-line INFILE))
      (setq P2X (atof INLINE))
      (setq INLINE (read-line INFILE))
      (setq P2Y (atof INLINE))
      (setq INLINE (read-line INFILE))
      (if (= INLINE "SPIRAL")
       (progn
        (setq INLINE (read-line INFILE))
        (setq PLTX (atof INLINE))
        (setq INLINE (read-line INFILE))
        (setq PLTY (atof INLINE))
        (setq INLINE (read-line INFILE))
        (setq PLTSTX (atof INLINE))
        (setq INLINE (read-line INFILE))
        (setq PLTSTY (atof INLINE))
        (setq INLINE (read-line INFILE))
        (setq PSTX (atof INLINE))
        (setq INLINE (read-line INFILE))
        (setq PSTY (atof INLINE))
        (setq INLINE (read-line INFILE))
        (setq LO (atof INLINE))
        (setq BULGE (list (list PLTX PLTY) (list PLTSTX PLTSTY) (list PSTX PSTY) LO))
       )
       (progn
        (setq BULGE (atof INLINE))
       )
      )
      (setq INLINE (read-line INFILE))
      (setq ALIGNLIST (append ALIGNLIST (list (list STA (list P1X P1Y) (list P2X P2Y) BULGE))))
     )
    )
   )
   (close INFILE)
  )
 )
)