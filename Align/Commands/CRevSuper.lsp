;
;
;     Program written by Robert Livingston, 2018-06-12
;
;     C:REVSUPER swaps left/right values on superelevation blocks
;
;
(defun C:REVSUPER (/ ENT ENT2 ENTLIST SUPERLEFT SUPERRIGHT TMP)
 (while (setq ENT (car (entsel "\nSelect superelevation block : ")))
  (setq ENTLIST (entget ENT))
  (if (and (= (strcase (cdr (assoc 2 ENTLIST))) "SUPER")
           (= (cdr (assoc 66 ENTLIST)) 1)
      )
   (progn
    (setq ENT2 (entnext ENT))
    (setq ENTLIST (entget ENT2))
    (setq SUPERLEFT nil)
    (setq SUPERRIGHT nil)
    (while (/= (cdr (assoc 0 ENTLIST)) "SEQEND")
     (if (= (strcase (cdr (assoc 2 ENTLIST))) "LEFT")
      (setq SUPERLEFT (cdr (assoc 1 ENTLIST)))
     )
     (if (= (strcase (cdr (assoc 2 ENTLIST))) "RIGHT")
      (setq SUPERRIGHT (cdr (assoc 1 ENTLIST)))
     )
     (setq ENT2 (entnext ENT2))
     (setq ENTLIST (entget ENT2))
    )
    (if (and SUPERLEFT SUPERRIGHT)
     (progn
      (setq ENT2 (entnext ENT))
      (setq ENTLIST (entget ENT2))
      (while (/= (cdr (assoc 0 ENTLIST)) "SEQEND")
       (if (= (strcase (cdr (assoc 2 ENTLIST))) "LEFT")
        (progn
         (setq ENTLIST (subst (cons 1 SUPERRIGHT) (assoc 1 ENTLIST) ENTLIST))
         (entmod ENTLIST)
         (entupd ENT2)
         (entupd ENT)
        )
       )
       (if (= (strcase (cdr (assoc 2 ENTLIST))) "RIGHT")
        (progn
         (setq ENTLIST (subst (cons 1 SUPERLEFT) (assoc 1 ENTLIST) ENTLIST))
         (entmod ENTLIST)
         (entupd ENT2)
         (entupd ENT)
        )
       )
       (setq ENT2 (entnext ENT2))
       (setq ENTLIST (entget ENT2))
      )
     )
    )
   )
  )
 )
 T
)