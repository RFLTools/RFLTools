;
;
;     Program written by Robert Livingston, 2002/02/04
;
;     Z2ATT replaces the ELEV attribute with the 'Z' of a selected point
;
;
(defun C:Z2ATT (/ CMDECHO ENT ENTLIST TXT)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (while (/= nil (setq ENT (getpoint "\nSelect point : ")))
  (if (= nil ENT)
   (princ "\n!!! No point selected !!!")
   (progn
    (setq TXT (rtos (nth 2 ENT) 2 3))
    (if (/= nil (setq ENT (car (entsel "\nSelect BLOCK entity : "))))
     (progn
      (setq ENTLIST (entget ENT))
      (if (/= "INSERT" (cdr (assoc 0 ENTLIST)))
       (princ "\n!!! Not a block entity !!!")
       (if (= (cdr (assoc 66 ENTLIST)) 1)
        (while (/= "SEQEND" (cdr (assoc 0 ENTLIST)))
         (if (or (= "ELEV" (strcase (cdr (assoc 2 ENTLIST)))) (= "ELEVATION" (strcase (cdr (assoc 2 ENTLIST)))))
          (progn
           (setq ENTLIST (subst (cons 1 TXT) (assoc 1 ENTLIST) ENTLIST))
           (entmod ENTLIST)
           (entupd ENT)
          )
         )
         (setq ENT (entnext ENT))
         (setq ENTLIST (entget ENT))
        )
       )
      )
     )
    )
   )
  )
 )

 (setvar "CMDECHO" CMDECHO)
)
