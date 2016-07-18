;
;
;   Program written by Robert Livingston, 2008/11/04
;
;   RFL:WSUPERB writes the superelevation to a RFLALIGN Block
;
;
(defun RFL:WSUPERB (BLKENT / BLKENTNEW BLKENTLIST C ENT ENTLIST ENTN)
 (entmake)
 (setq BLKENTLIST (entget BLKENT))
 (setq BLKENTNEW (entmake BLKENTLIST))
 (setq ENT (entnext BLKENT))
 (setq ENTLIST (entget ENT))
 (while (/= "SEQEND" (cdr (assoc 0 ENTLIST)))
  (if (= "E" (cdr (assoc 2 ENTLIST)))
   (progn
    (setq ENTLIST (subst (cons 1 "#RFL SUPERELEVATION FILE") (assoc 1 ENTLIST) ENTLIST))
    (entmake ENTLIST)
    (setq C 0)
    (while (< C (length RFL:SUPERLIST))
     (setq ENTLIST (subst (cons 70 1) (assoc 70 ENTLIST) ENTLIST))
     (setq ENTLIST (subst (cons 1 (rtos (nth 0 (nth C RFL:SUPERLIST)) 2 16)) (assoc 1 ENTLIST) ENTLIST))
     (entmake ENTLIST)
     (setq ENTLIST (subst (cons 1 (rtos (nth 1 (nth C RFL:SUPERLIST)) 2 16)) (assoc 1 ENTLIST) ENTLIST))
     (entmake ENTLIST)
     (setq ENTLIST (subst (cons 1 (rtos (nth 2 (nth C RFL:SUPERLIST)) 2 16)) (assoc 1 ENTLIST) ENTLIST))
     (entmake ENTLIST)
     (setq C (+ C 1))
    )
    (setq ENTLIST (subst (cons 1 "#END DEFINITION") (assoc 1 ENTLIST) ENTLIST))
    (entmake ENTLIST)
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
    (while (= "E" (cdr (assoc 2 ENTLIST)))
     (setq ENT (entnext ENT))
     (setq ENTLIST (entget ENT))
    )
   )
   (progn
    (entmake ENTLIST)
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
   )
  )
 )
 (entmake ENTLIST)
 (entdel BLKENT)
 (setq BLKENTNEW (entlast))
)
