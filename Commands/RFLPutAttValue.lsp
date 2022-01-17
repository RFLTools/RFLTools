;
;
;     Program written by Robert Livingston, 2022-01-17
;
;     RFL:PUTATTVALUE writes the value (string) to the specified TAG of the provided ENT;
;
;
(defun RFL:PUTATTVALUE (ENT TAG ATTVALUE / ENT2 ENTLIST FLAG)
 (setq FLAG nil)
 (setq ENTLIST (entget ENT))
 (setq ENT2 ENT)
 (if (and (= (cdr (assoc 0 ENTLIST)) "INSERT")
          (= (cdr (assoc 66 ENTLIST)) 1)
     )
  (while (and (/= (cdr (assoc 0 ENTLIST)) "SEQEND")
              (/= (cdr (assoc 2 ENTLIST)) TAG)
         )
   (setq ENT (entnext ENT))
   (setq ENTLIST (entget ENT))
   (if (and (assoc 2 ENTLIST) (= (strcase (cdr (assoc 2 ENTLIST))) (strcase TAG)))
    (progn
     (setq FLAG T)
     (setq ENTLIST (subst (cons 1 ATTVALUE) (assoc 1 ENTLIST) ENTLIST))
     (entmod ENTLIST)
     (entupd ENT2)
    )
   )
  )
 )
 (if FLAG
  (entupd ENT2)
  nil
 )
)
