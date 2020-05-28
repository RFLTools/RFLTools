;
;
;     Program written by Robert Livingston, 2020-05-28
;
;     SPOT2POINT draws AutoCAD points at the insertion point if SPOTELEVATION blocks and sets the Z to the ELEV attribute
;
;
(defun C:SPOT2POINT (/ C ENT ENTLIST ENTSET P Z)
 (command "._UNDO" "M")
 (if (setq ENTSET (ssget))
  (progn
   (setq C 0)
   (while (< C (sslength ENTSET))
    (setq Z nil)
    (setq ENT (ssname ENTSET C))
    (setq ENTLIST (entget ENT))
    (if (and (= (cdr (assoc 0 ENTLIST)) "INSERT")
             (or (= (strcase (cdr (assoc 2 ENTLIST)) "SPOTELEVATION"))
                 (= (strcase (cdr (assoc 2 ENTLIST)) "SPOTELEVATION2"))
             )
        )
     (progn
      (setq P (cdr (assoc 10 ENTLIST)))
      (setq ENT (entnext ENT))
      (setq ENTLIST (entget ENT))
      (while (= (cdr (assoc 0 ENTLIST)) "ATTRIB")
       (if (or (= (strcase (cdr (assoc 2 ENTLIST))) "ELEV")
               (= (strcase (cdr (assoc 2 ENTLIST))) "ELEVATION")
               (= (strcase (cdr (assoc 2 ENTLIST))) "Z")
           )
        (progn
         (setq Z (atof (cdr (assoc 1 ENTLIST))))
         (if (= Z 0.0) (setq Z nil))
        )
       )
       (setq ENT (entnext ENT))
       (setq ENTLIST (entget ENT))
      )
     )
    )
    (if Z
     (entmake (list (cons 0 "POINT")
                    (list 10 (car p) (cadr P) Z)
              )
     )
    )
    (setq C (1+ C))
   )
  )
 )
 T
)