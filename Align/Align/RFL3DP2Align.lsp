;
;
;     Program written by Robert Livingston, 2001/01/11
;
;     RFL:3DP2ALIGN converts a 3d polyline to horizontal and vertical alignments
;
;
(defun RFL:3DP2ALIGN (ENT STA / ENTLIST L P1 P2)
 (if (/= nil ENT)
  (progn
   (setq ENTLIST (entget ENT))
   (if (/= (cdr (assoc 0 ENTLIST)) "POLYLINE")
    (princ "\n*****  Not a polyline!  *****")
    (progn
     (if (= (float (/ (cdr (assoc 70 ENTLIST)) 2 2 2 2))
            (/ (cdr (assoc 70 ENTLIST)) 16.0))
      (princ "\n*****  Not a 3d polyline!  *****")
      (progn
       (setq ENT (entnext ENT))
       (setq ENTLIST (entget ENT))
       (setq P2 (cdr (assoc 10 ENTLIST)))
       (setq RFL:PVILIST (list (list STA
                                 (nth 2 P2)
                                 "L"
                                 0.0
                           )
                     )
       )
       (setq RFL:ALIGNLIST nil)
       (setq ENT (entnext ENT))
       (setq ENTLIST (entget ENT))
       (while (/= (cdr (assoc 0 ENTLIST)) "SEQEND")
        (setq P1 P2)
        (setq P2 (cdr (assoc 10 ENTLIST)))
        (setq L (distance (list (nth 0 P1) (nth 1 P1)) (list (nth 0 P2) (nth 1 P2))))
        (setq RFL:ALIGNLIST (append RFL:ALIGNLIST (list (list STA
                                                              (list (nth 0 P1) (nth 1 P1))
                                                              (list (nth 0 P2) (nth 1 P2))
                                                              0.0
                                                        )
                                                  )
                            )
        )
        (setq STA (+ STA L))
        (setq RFL:PVILIST (append RFL:PVILIST (list (list STA
                                                          (nth 2 P2)
                                                          "L"
                                                          0.0
                                                    )
                                              )
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