;
;
;     Program written by Robert Livingston, 2008-11-04
;
;     RFL:RABKILL removes alignment definition lists from RFLALIGN blocks
;
;
(defun RFL:RABKILL (BLKENT NODE / BLKENTNEW BLKENTLIST ENT ENTLIST ENTN)
 (entmake)
 (setq BLKENTLIST (entget BLKENT))
 (setq BLKENTNEW (entmake BLKENTLIST))
 (setq ENT (entnext BLKENT))
 (setq ENTLIST (entget ENT))
 (while (/= "SEQEND" (cdr (assoc 0 ENTLIST)))
  (if (= NODE (cdr (assoc 2 ENTLIST)))
   (progn
    (setq ENTLIST (subst (cons 1 "N/A") (assoc 1 ENTLIST) ENTLIST))
    (entmake ENTLIST)
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
    (while (= NODE (cdr (assoc 2 ENTLIST)))
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
