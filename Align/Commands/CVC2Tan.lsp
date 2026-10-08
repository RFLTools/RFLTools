;
;
;     Program written by Robert Livingston, 2022-01-28
;
;     VC2TAN draws lines tangent to two vertical curves.
;
;     Revised 2026-10-08 (Hermes):
;       * solved in closed form (both common tangents) instead of by fixed-
;         point iteration, so the result is deterministic and never "spins",
;       * the curves are treated as continuous parabolas; a common tangent is
;         drawn when it touches an existing curve (a point of contact lies on
;         a drawn curve); if exactly ONE tangent touches, it alone is drawn;
;         if BOTH touch (either may be valid) or NEITHER touches, both are
;         drawn - lesser grade first, so that ERASE with the Last option
;         removes the steeper tangent,
;       * guards degenerate input (a straight "curve", a zero horizontal run).
;
(defun C:VC2TAN ( / PLIST P1 P2 P3
                    X1 Y1 X2 Y2 X3 Y3
                    G1 G2
                    EA EB EC ELO EHI
                    FA FB FC FLO FHI
                    QA QB QC DISC R MROOT1 MROOT2
                    M R1 R2 CX1 CY1 CX2 CY2
                    REC ALLLIST TLIST EPS )

 (setq EPS 1e-7)

 (princ "\nSelect 'from' vertical curve :")
 (if (setq PLIST (RFL:GETVCURVE3P))
  (progn
   (setq P1 (car PLIST) P2 (cadr PLIST) P3 (caddr PLIST))
   (if (and P1 P2 P3)
    (progn
     (setq X1 (nth 0 P1) Y1 (nth 1 P1)
           X2 (nth 0 P2) Y2 (nth 1 P2)
           X3 (nth 0 P3) Y3 (nth 1 P3))
     (if (or (equal X1 X2 EPS) (equal X2 X3 EPS) (equal X1 X3 EPS))
      (princ "\n'From' vertical curve has a zero horizontal run.")
      (progn
       (setq G1 (/ (- Y2 Y1) (- X2 X1)))
       (setq G2 (/ (- Y3 Y2) (- X3 X2)))
       (setq EA (/ (- G2 G1) (* (- X3 X1) 2.0)))
       (setq EB (/ (- (* G1 X3) (* G2 X1)) (- X3 X1)))
       (setq EC (- Y1 (+ (* EA X1 X1) (* EB X1))))
       (setq ELO (min X1 X3) EHI (max X1 X3))
       (if (< (abs EA) 1e-12)
        (princ "\n'From' vertical curve is straight (no curvature).")
        (progn
         (princ "\nSelect 'to' vertical curve :")
         (if (setq PLIST (RFL:GETVCURVE3P))
          (progn
           (setq P1 (car PLIST) P2 (cadr PLIST) P3 (caddr PLIST))
           (if (and P1 P2 P3)
            (progn
             (setq X1 (nth 0 P1) Y1 (nth 1 P1)
                   X2 (nth 0 P2) Y2 (nth 1 P2)
                   X3 (nth 0 P3) Y3 (nth 1 P3))
             (if (or (equal X1 X2 EPS) (equal X2 X3 EPS) (equal X1 X3 EPS))
              (princ "\n'To' vertical curve has a zero horizontal run.")
              (progn
               (setq G1 (/ (- Y2 Y1) (- X2 X1)))
               (setq G2 (/ (- Y3 Y2) (- X3 X2)))
               (setq FA (/ (- G2 G1) (* (- X3 X1) 2.0)))
               (setq FB (/ (- (* G1 X3) (* G2 X1)) (- X3 X1)))
               (setq FC (- Y1 (+ (* FA X1 X1) (* FB X1))))
               (setq FLO (min X1 X3) FHI (max X1 X3))
               (if (< (abs FA) 1e-12)
                (princ "\n'To' vertical curve is straight (no curvature).")
                (progn
                 ;; A common tangent has one grade M; it touches curve i at
                 ;;   x = (M - Bi) / (2 Ai).
                 ;; Equating the two tangent lines gives  QA*M^2 + QB*M + QC = 0
                 (setq QA (- EA FA))
                 (setq QB (* 2.0 (- (* FA EB) (* EA FB))))
                 (setq QC (- (* EA FB FB) (* FA EB EB) (* 4.0 EA FA (- FC EC))))
                 (setq MROOT1 nil MROOT2 nil)
                 (if (< (abs QA) 1e-12)
                  (if (>= (abs QB) 1e-12)
                   (setq MROOT1 (/ (- QC) QB))
                  )
                  (progn
                   (setq DISC (- (* QB QB) (* 4.0 QA QC)))
                   (if (>= DISC 0.0)
                    (progn
                     (setq R (sqrt DISC))
                     (setq MROOT1 (/ (+ (- QB) R) (* 2.0 QA)))
                     (setq MROOT2 (/ (- (- QB) R) (* 2.0 QA)))
                    )
                   )
                  )
                 )
                 ;; order the roots: lesser grade first, steeper second
                 (setq R1 nil R2 nil)
                 (cond
                  ((and MROOT1 MROOT2)
                   (if (< (abs MROOT1) (abs MROOT2))
                    (setq R1 MROOT1 R2 MROOT2)
                    (setq R1 MROOT2 R2 MROOT1)
                   )
                  )
                  (MROOT1 (setq R1 MROOT1))
                  (MROOT2 (setq R1 MROOT2))
                 )
                 ;; classify each root: does it touch a drawn curve?
                 (setq ALLLIST nil TLIST nil)
                 (foreach M (list R1 (if (equal R1 R2 EPS) nil R2))
                  (if M
                   (progn
                    (setq CX1 (/ (- M EB) (* 2.0 EA)))
                    (setq CY1 (+ (* EA CX1 CX1) (* EB CX1) EC))
                    (setq CX2 (/ (- M FB) (* 2.0 FA)))
                    (setq CY2 (+ (* FA CX2 CX2) (* FB CX2) FC))
                    (setq REC (list M CX1 CY1 CX2 CY2))
                    (setq ALLLIST (append ALLLIST (list REC)))
                    (if (or (<= (- ELO (* EPS (- EHI ELO))) CX1 (+ EHI (* EPS (- EHI ELO))))
                            (<= (- FLO (* EPS (- FHI FLO))) CX2 (+ FHI (* EPS (- FHI FLO))))
                        )
                     (setq TLIST (append TLIST (list REC)))
                    )
                   )
                  )
                 )
                 ;; draw the touching tangent(s); if none touch, draw both
                 (if (null TLIST)
                  (setq TLIST ALLLIST)
                 )
                 (foreach REC TLIST
                  (entmake (list (cons 0 "LINE")
                                 (list 10 (nth 1 REC) (nth 2 REC) 0.0)
                                 (list 11 (nth 3 REC) (nth 4 REC) 0.0)
                           )
                  )
                 )
                 (if (null ALLLIST)
                  (princ "\nNo real common tangent exists between these curves.")
                 )
                )
               )
              )
             )
            )
           )
          )
         )
        )
       )
      )
     )
    )
   )
  )
 )
 (princ)
)
