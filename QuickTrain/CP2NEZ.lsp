;
;
;     Program written by Robert Livingston, 2013-05-01
;
;     C:P2NEZ Updates the X/Y/Z attributes of a selected point blocc to its insertion point
;
;
(defun C:P2NEZ (/ ENT ENT2 ENTLIST P)
 (setq ENT (car (entsel)))
 (setq ENTLIST (entget ENT))
 (setq P (cdr (assoc 10 ENTLIST)))
 (if (= 1 (cdr (assoc 66 ENTLIST)))
  (progn
   (setq ENT2 (entnext ENT))
   (setq ENTLIST (entget ENT2))
   (while (= "ATTRIB" (cdr (assoc 0 ENTLIST)))
    (cond ((= "E" (cdr (assoc 2 ENTLIST)))
           (setq ENTLIST (subst (cons 1 (rtos (car P) 2 8)) (assoc 1 ENTLIST) ENTLIST))
          )
          ((= "N" (cdr (assoc 2 ENTLIST)))
           (setq ENTLIST (subst (cons 1 (rtos (cadr P) 2 8)) (assoc 1 ENTLIST) ENTLIST))
          )
          ((= "Z" (cdr (assoc 2 ENTLIST)))
           (setq ENTLIST (subst (cons 1 (rtos (caddr P) 2 8)) (assoc 1 ENTLIST) ENTLIST))
          )
    )
    (entmod ENTLIST)
    (entupd ENT2)
    (setq ENT2 (entnext ENT2))
    (setq ENTLIST (entget ENT2))
   )
   (entupd ENT)
  )
 )
)
