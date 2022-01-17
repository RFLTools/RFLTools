;
;
;     Program written by Robert Livingston, 2022-01-17
;
;     RFL:GETATTVALUE retreives the value (string) of the specified TAG of the provided ENT;
;
;
(defun RFL:GETATTVALUE (ENT TAG / ATTVALUE ENTLIST)
 (setq ATTVALUE nil)
 (setq ENTLIST (entget ENT))
 (if (and (= (cdr (assoc 0 ENTLIST)) "INSERT")
          (= (cdr (assoc 66 ENTLIST)) 1)
     )
  (while (and (/= (cdr (assoc 0 ENTLIST)) "SEQEND")
              (/= (cdr (assoc 2 ENTLIST)) TAG)
         )
   (setq ENT (entnext ENT))
   (setq ENTLIST (entget ENT))
   (if (and (assoc 2 ENTLIST) (= (strcase (cdr (assoc 2 ENTLIST))) (strcase TAG)))
    (setq ATTVALUE (cdr (assoc 1 ENTLIST)))
   )
  )
 )
 ATTVALUE
)
