;
;
;     Program written by Robert Livingston, 2019-10-09
;
;     CHTRANS changes all the entities within a block to a selected tansparency
;
;
(defun C:CHTRANS (/ TRANSPARENCY ENT)
 (while (setq ENT (car (entsel "\nSelect block : ")))
  (if (= "INSERT" (cdr (assoc 0 (entget ENT))))
   (progn
    (setq TRANSPARENCY nil)
    (while (or (= TRANSPARENCY nil)
               (< TRANSPARENCY 0)
               (> TRANSPARENCY 100)
           )
     (setq TRANSPARENCY (getint "Enter transperancey for block (0-100, 0=ByLayer, 100=ByBlock) <0> : "))
     (if (= nil TRANSPARENCY) (setq TRANSPARENCY 0))
     (if (= TRANSPARENCY 0) (setq TRANSPARENCY "ByLayer"))
     (if (= TRANSPARENCY 100) (setq TRANSPARENCY "ByBlock"))
    )
    (RFL:CHTRANS ENT TRANSPARENCY)
   )
  )
 )
)
(defun RFL:CHTRANS (ENT TRANSPARENCY / ENT2 ENTLIST ENTLIST2 ENTOBJ ENTNAME)
 (if (= "INSERT" (cdr (assoc 0 (setq ENTLIST (entget ENT)))))
  (progn
   (setq ENTNAME (cdr (assoc 2 ENTLIST)))
   (setq ENTLIST2 (tblsearch "BLOCK" ENTNAME))
   (setq ENT2 (cdr (assoc -2 ENTLIST2)))
   (while (/= nil ENT2)
    (setq ENTOBJ (vlax-ename->vla-object ENT2))
    (vla-put-entitytransparency ENTOBJ TRANSPARENCY)
;    (setq ENTLIST2 (entget ENT2))
;    (if (= nil (assoc 440 ENTLIST2))
;     (setq ENTLIST2 (append ENTLIST2 (list (cons 440 TRANSPARENCY))))
;     (setq ENTLIST2 (subst (cons 440 TRANSPARENCY) (assoc 440 ENTLIST2) ENTLIST2))
;    )
;    (entmod ENTLIST2)
    (setq ENT2 (entnext ENT2))
   )
   (entupd ENT)
  )
 )
 nil
)