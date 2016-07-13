;
;
;   Program written by Robert Livingston, 98/06/12
;
;   RFL:DRAWALIGN draws the current alignmnet for alignments without spirals
;
;
;
(defun RFL:DRAWALIGN (/ ALLIST ALENT ENTLIST)
 (setq ALLIST RFL:ALIGNLIST)
 (entmake)
 (setq ENTLIST (list (cons 0 "POLYLINE")
                     (cons 66 1)))
 (entmake ENTLIST)
 (while (/= ALLIST nil)
  (setq ALENT (car ALLIST))
  (setq ALLIST (cdr ALLIST))
  (setq ENTLIST (list (cons 0 "VERTEX")
                      (append (list 10) (nth 1 ALENT))
                      (cons 42 (nth 3 ALENT))
                )
  )
  (entmake ENTLIST)
  (if (= ALLIST nil)
   (progn
    (setq ENTLIST (list (cons 0 "VERTEX")
                        (append (list 10) (nth 2 ALENT))
                  )
    )
    (entmake ENTLIST)
   )
  )
 )
 (setq ENTLIST (list (cons 0 "SEQEND")))
 (entmake ENTLIST)
 (command "._convert" "P" "S" (entlast) "")
)