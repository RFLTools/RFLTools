;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:GETSPIRALDATA returns the extended spiral data for an entity (reverse engineered DCA spiral)
;
;
(defun RFL:GETSPIRALDATA (ENT / ENTLIST ENTLIST2 PLT PLTST PST LS SPIRALLIST TMP)
 (setq ENTLIST2 (cdr (assoc -3 (entget ENT '("*")))))
 (setq ENTLIST (cdr (assoc "DCA_FIGURE_XENT" ENTLIST2)))
 (if (or (= ENTLIST nil) (= (assoc 1011 ENTLIST) nil))
  (progn
   (setq SPIRALLIST nil)
  )
  (progn
   (while (/= (car (car ENTLIST)) 1011)
    (setq ENTLIST (cdr ENTLIST))
   )
   (setq PLT (cdr (car ENTLIST)))
   (setq PLT (list (car PLT) (cadr PLT)))
   (setq ENTLIST (cdr ENTLIST))
   (while (/= (car (car ENTLIST)) 1011)
    (setq ENTLIST (cdr ENTLIST))
   )
   (setq PLTST (cdr (car ENTLIST)))
   (setq PLTST (list (car PLTST) (cadr PLTST)))
   (setq ENTLIST (cdr ENTLIST))
   (while (/= (car (car ENTLIST)) 1011)
    (setq ENTLIST (cdr ENTLIST))
   )
   (setq PST (cdr (car ENTLIST)))
   (setq PST (list (car PST) (cadr PST)))
   (while (/= (car (car ENTLIST)) 1040)
    (setq ENTLIST (cdr ENTLIST))
   )
   (setq LS (cdr (car ENTLIST)))
   (setq ENTLIST (cdr ENTLIST))
   (if (< (distance PLT PLTST) (distance PST PLTST))
    (progn
     (setq TMP PST)
     (setq PST PLT)
     (setq PLT TMP)
    )
   )
   (setq SPIRALLIST (list PLT PLTST PST LS))
  )
 )
 SPIRALLIST
)
