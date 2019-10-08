;
;
;     Program written by Robert Livingston, 2016-10-11
;
;     CHCOLOUR changes all the entities withing a block to a selected color
;
;
(defun C:CHCOLOUR (/ COLOUR ENT)
 (while (setq ENT (car (entsel "\nSelect block : ")))
  (if (= "INSERT" (cdr (assoc 0 (entget ENT))))
   (progn
    (setq COLOUR nil)
    (while (or (= COLOUR nil)
               (< COLOUR 0)
               (> COLOUR 256)
           )
     (setq COLOUR (getint "Enter colour for block (0-256, 0=ByBlock, 256=ByLayer) <0> : "))
     (if (= nil COLOUR) (setq COLOUR 0))
    )
    (RFL:CHCOLOUR ENT COLOUR)
   )
  )
 )
)
(defun RFL:CHCOLOUR (ENT COLOUR / ENT2 ENTLIST ENTLIST2 ENTNAME)
 (if (= "INSERT" (cdr (assoc 0 (setq ENTLIST (entget ENT)))))
  (progn
   (setq ENTNAME (cdr (assoc 2 ENTLIST)))
   (setq ENTLIST2 (tblsearch "BLOCK" ENTNAME))
   (setq ENT2 (cdr (assoc -2 ENTLIST2)))
   (while (/= nil ENT2)
    (setq ENTLIST2 (entget ENT2))
    (if (= nil (assoc 62 ENTLIST2))
     (setq ENTLIST2 (append ENTLIST2 (list (cons 62 COLOUR))))
     (setq ENTLIST2 (subst (cons 62 COLOUR) (assoc 62 ENTLIST2) ENTLIST2))
    )
    (entmod ENTLIST2)
    (setq ENT2 (entnext ENT2))
   )
   (entupd ENT)
  )
 )
 nil
)