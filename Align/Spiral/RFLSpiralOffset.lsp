;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:SPIRALOFFSET returns the offset of an offset spiral
;
;
(defun RFL:SPIRALOFFSET (ENT / ENT2 ENTLIST OS P P1 P2 PLT PLTST PST SDATA)
 (if (= (setq SDATA (RFL:GETSPIRALDATA ENT)) nil)
  nil
  (progn
   (setq ENTLIST (entget ENT))
   (if (= (cdr (assoc 0 ENTLIST)) "POLYLINE")
    (progn
     (setq ENT2 (entnext ENT))
     (setq ENTLIST (entget ENT2))
     (setq P1 (cdr (assoc 10 ENTLIST)))
     (while (/= "SEQEND" (cdr (assoc 0 ENTLIST)))
      (setq P2 (cdr (assoc 10 ENTLIST)))
      (setq ENT2 (entnext ENT2))
      (setq ENTLIST (entget ENT2))
     )
    )
    (progn
     (setq P1 (cdr (assoc 10 ENTLIST)))
     (setq P2 (cdr (assoc 10 (reverse ENTLIST))))
    )
   )
   (setq PLT (car SDATA))
   (setq PLTST (cadr SDATA))
   (setq PST (caddr SDATA))
   (if (< (distance PST P1) (distance PST P2))
    (setq P P1)
    (setq P P2)
   )
   (setq OS (distance PST P))

   (setq OS (* OS
               -1.0
               (RFL:SIGN (sin (- (angle PLTST PST) (angle PLT PLTST))))
               (RFL:SIGN (- (sin (- (angle PLTST P) (angle PLT PLTST)))
                            (sin (- (angle PLTST PST) (angle PLT PLTST)))
                         )
               )
            )
   )
  )
 )
)
