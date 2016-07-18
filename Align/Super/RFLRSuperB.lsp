;
;
;   Program written by Robert Livingston, 2008-11-04
;
;   RFL:RSUPERB reads the Superelevation from a RFLAlign Block
;
;
(defun RFL:RSUPERB (BLKENT / ENT ENTLIST INLINE STA SUPERLEFT SUPERRIGHT)
 (setq RFL:SUPERLIST nil)
 (setq ENT (entnext BLKENT))
 (setq ENTLIST (entget ENT))
 (while (/= "E" (cdr (assoc 2 ENTLIST)))
  (setq ENT (entnext ENT))
  (setq ENTLIST (entget ENT))
 )
 (setq INLINE (cdr (assoc 1 ENTLIST)))
 (setq ENT (entnext ENT))
 (setq ENTLIST (entget ENT))
 (if (/= INLINE "#RFL SUPERELEVATION FILE")
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
    (setq SUPERLEFT (atof INLINE))
    (setq INLINE (cdr (assoc 1 ENTLIST)))
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
    (setq SUPERRIGHT (atof INLINE))
    (setq INLINE (cdr (assoc 1 ENTLIST)))
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
    (setq RFL:SUPERLIST (append RFL:SUPERLIST (list (list STA SUPERLEFT SUPERRIGHT))))
   )
  )
 )
)
