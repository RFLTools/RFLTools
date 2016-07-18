;
;
;   Program written by Robert Livingston, 2008-11-04
;
;   RFL:RPROFOGB reads a vertical OG profile from a RFLAlign Block
;
;
(defun RFL:RPROFOGB (BLKENT / ELEV ENT ENTLIST INLINE LR STA VAL)
 (setq RFL:OGLIST nil)
 (setq ENT (entnext BLKENT))
 (setq ENTLIST (entget ENT))
 (while (/= "OG" (cdr (assoc 2 ENTLIST)))
  (setq ENT (entnext ENT))
  (setq ENTLIST (entget ENT))
 )
 (setq INLINE (cdr (assoc 1 ENTLIST)))
 (setq ENT (entnext ENT))
 (setq ENTLIST (entget ENT))
 (if (/= INLINE "#RFL VERTICAL ALIGNMENT FILE")
  (progn
   (princ "\n*** FILE NOT FORMATTED CORRECTLY ***\n")
  )
  (progn
   (setq INLINE (cdr (assoc 1 ENTLIST)))
   (setq ENT (entnext ENT))
   (setq ENTLIST (entget ENT))
   (while (and (/= nil INLINE) (/= INLINE "#END DEFINITION"))
    (setq STA (atof INLINE))
    (setq INLINE (cdr (assoc 1 ENTLIST)))
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
    (setq ELEV (atof INLINE))
    (setq INLINE (cdr (assoc 1 ENTLIST)))
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
    (setq LR INLINE)
    (setq INLINE (cdr (assoc 1 ENTLIST)))
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
    (setq VAL (atof INLINE))
    (setq INLINE (cdr (assoc 1 ENTLIST)))
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
    (setq RFL:OGLIST (append RFL:OGLIST (list (list STA ELEV))))
   )
  )
 )
)
