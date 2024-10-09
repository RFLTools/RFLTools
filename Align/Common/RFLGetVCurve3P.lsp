;
;
;     Program written by Robert Livingston, 2024-10-09
;
;     RFL:GETVCURVE3P returns a list of 3 points (P1 VPI P3) and optional VEXAG for a selected vertical curve entity
;
;
(defun RFL:GETVCURVE3P (/ ENT ENTLIST NODE P P1 P2 P3 PVI RFL:PROFDEFLIST RFL:PVILIST STA)
 (setq P1 nil P2 nil P3 nil VEXAG nil)
 (setq ENT (entsel "\nSelect vertical curve : "))
 (setq P (cadr ENT))
 (setq ENT (car ENT))
 (if (/= nil ENT)
  (progn
   (setq ENTLIST (entget ENT))
   (if (= "POLYLINE" (cdr (assoc 0 ENTLIST)))
    (progn
     (setq ENT (entnext ENT))
     (setq ENTLIST (entget ENT))
     (setq P1 (cdr (assoc 10 ENTLIST)))
     (setq ENT (entnext ENT))
     (setq ENTLIST (entget ENT))
     (setq P2 (cdr (assoc 10 ENTLIST)))
     (if (/= nil P2)
      (progn
       (setq ENT (entnext ENT))
       (setq ENTLIST (entget ENT))
       (setq P3 (cdr (assoc 10 ENTLIST)))
       (if (/= nil P3)
        (progn
         (setq ENT (entnext ENT))
         (setq ENTLIST (entget ENT))
         (while (/= "SEQEND" (cdr (assoc 0 ENTLIST)))
          (setq P2 P3)
          (setq P3 (cdr (assoc 10 ENTLIST)))
          (setq ENT (entnext ENT))
          (setq ENTLIST (entget ENT))
         )
        )
       )
      )
     )
    )
    (if (= "AECC_PROFILE" (cdr (assoc 0 (entget ENT))))
     (progn
      (setq RFL:PVILIST (RFL:RPROFC3D ENT))
      (if (setq RFL:PROFDEFLIST (RFL:PROFDEF))
       (progn
        (setq VEXAG (cdr (assoc "VEXAG" RFL:PROFDEFLIST)))
        (setq STA (car (RFL:VPP P)))
        (setq PVI (car RFL:PVILIST))
        (foreach NODE RFL:PVILIST
         (if (< (abs (- STA (car NODE))) (abs (- STA (car PVI)))) (setq PVI NODE))
        )
        (if (> (abs (* (- STA (car PVI)) 2.0)) (cadddr PVI))
         (princ "\n!Point not on vertical curve!\n")
         (progn
          (setq P1 (RFL:PROFPOINT (- (car PVI) (/ (cadddr PVI) 2.0)) (RFL:ELEVATION (- (car PVI) (/ (cadddr PVI) 2.0)))))
          (setq P2 (RFL:PROFPOINT (car PVI) (cadr PVI)))
          (setq P3 (RFL:PROFPOINT (+ (car PVI) (/ (cadddr PVI) 2.0)) (RFL:ELEVATION (+ (car PVI) (/ (cadddr PVI) 2.0)))))
         )
        )
       )
      )
     )
    )
   )
  )
 )
 (if (and P1 P2 P3)
  (if VEXAG
   (list P1 P2 P3 VEXAG)
   (list P1 P2 P3)
  )
 )
)