;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:FITSPIRALAA Fits a reverse engineered DCA spiral between two arcs
;
;    Rewritten 2026-10-08 (Hermes):
;      * real arc-to-arc solution (the previous body was a copy of
;        RFL:FITSPIRALLA and could not work on two arcs),
;      * the spiral is the tail [LO, LS] of a full spiral whose end radius is
;        the INNER arc's radius; the tail start (at length LO) sits on the OUTER
;        arc, so LO = LS * R_inner / R_outer (no "straight" end),
;      * one hand is drawn at a time; the user accepts it or cycles to the
;        other hand (getkword, default No / <return>),
;      * on accept the two input arcs are trimmed/extended to the spiral ends
;        (entmod/entupd): the piece lying under the spiral is removed and the
;        arc that is kept runs AWAY from the new spiral,
;      * requires the smaller arc to lie entirely inside the larger arc,
;      * reports why nothing is drawn (arcs overlap / spiral delta too large).
;
(defun RFL:FITSPIRALAA (ENT1 ENT2 / ENTLIST1 ENTLIST2 ELOUT ELIN EOUT EIN C1 C2 R1 R2
                                       OC ORAD IC IRAD DX DY D RHO MAXDEFL
                                       TH TLOW THIGH WMAG THL WX WY
                                       DIR LS LO PHI CPHI SPHI K1X K1Y K2X K2Y
                                       TX TY PEX PEY SEX SEY PST SPT PLT AEND
                                       DAX DAY DBX DBY CRS PX PY TT PLTST
                                       AT DS DE WDX WDY SPRLN ANS OK FOUND )
 (setq ENTLIST1 (entget ENT1))
 (setq ENTLIST2 (entget ENT2))
 (setq C1 (cdr (assoc 10 ENTLIST1)) C1 (list (car C1) (cadr C1)) R1 (cdr (assoc 40 ENTLIST1)))
 (setq C2 (cdr (assoc 10 ENTLIST2)) C2 (list (car C2) (cadr C2)) R2 (cdr (assoc 40 ENTLIST2)))
 (if (> R1 R2)
  (setq ORAD R1 OC C1 IRAD R2 IC C2 EOUT ENT1 EIN ENT2)
  (setq ORAD R2 OC C2 IRAD R1 IC C1 EOUT ENT2 EIN ENT1)
 )
 (setq ELOUT (entget EOUT))
 (setq ELIN (entget EIN))
 (setq DX (- (car IC) (car OC)) DY (- (cadr IC) (cadr OC)))
 (setq D (distance OC IC))
 (setq MAXDEFL pi)
 (if (>= D (- ORAD IRAD))
  (princ (strcat "\nRFL:FITSPIRALAA - the smaller arc is not inside the larger arc"
                 " (centre distance " (rtos D 2 3) " >= " (rtos (- ORAD IRAD) 2 3) "),"
                 " so no spiral can join them."
         )
  )
  (progn
   (setq RHO (/ IRAD ORAD))
   ;; ---- solve theta_max with |W(theta)| = D   (|W| decreases with theta) ----
   (setq FOUND nil TLOW 0.0 THIGH 0.0)
   (while (and (not FOUND) (< THIGH MAXDEFL))
    (setq TLOW THIGH)
    (setq THIGH (+ THIGH 0.05))
    (setq THL (* THIGH RHO RHO))
    (setq WX (- (* IRAD (RFL:SPIRALKR THIGH)) (* ORAD (RFL:SPIRALKR THL))))
    (setq WY (- (* IRAD (+ (RFL:SPIRALFYR THIGH) (cos THIGH)))
                (* ORAD (+ (RFL:SPIRALFYR THL) (cos THL)))))
    (setq WMAG (sqrt (+ (* WX WX) (* WY WY))))
    (if (< WMAG D) (setq FOUND T))
   )
   (if (not FOUND)
    (princ (strcat "\nRFL:FITSPIRALAA - the spiral delta is too large for these arcs"
                   " (a fit needs more than " (rtos MAXDEFL 2 3) " rad of turn),"
                   " so nothing was drawn."
           )
    )
    (progn
     (while (> (- THIGH TLOW) 1.0e-10)
      (setq TH (/ (+ TLOW THIGH) 2.0))
      (setq THL (* TH RHO RHO))
      (setq WX (- (* IRAD (RFL:SPIRALKR TH)) (* ORAD (RFL:SPIRALKR THL))))
      (setq WY (- (* IRAD (+ (RFL:SPIRALFYR TH) (cos TH)))
                  (* ORAD (+ (RFL:SPIRALFYR THL) (cos THL)))))
      (setq WMAG (sqrt (+ (* WX WX) (* WY WY))))
      (if (< WMAG D) (setq THIGH TH) (setq TLOW TH))
     )
     (setq TH (/ (+ TLOW THIGH) 2.0))
     (setq LS (* 2.0 TH IRAD) LO (* LS RHO))
     (setq THL (* TH RHO RHO))
     ;; ---- draw one hand at a time; cycle until the user accepts ----
     (setq OK nil DIR 1.0)
     (while (not OK)
      (setq K1X (* ORAD (RFL:SPIRALKR THL)))
      (setq K1Y (* DIR ORAD (+ (RFL:SPIRALFYR THL) (cos THL))))
      (setq K2X (* IRAD (RFL:SPIRALKR TH)))
      (setq K2Y (* DIR IRAD (+ (RFL:SPIRALFYR TH) (cos TH))))
      (setq WX (- K2X K1X) WY (- K2Y K1Y))
      (setq PHI (- (angle (list 0.0 0.0) (list DX DY)) (angle (list 0.0 0.0) (list WX WY))))
      (setq CPHI (cos PHI) SPHI (sin PHI))
      (setq TX (- (car OC) (- (* K1X CPHI) (* K1Y SPHI))))
      (setq TY (- (cadr OC) (+ (* K1X SPHI) (* K1Y CPHI))))
      (setq PLT (list TX TY))
      ;; end of the tail = the INNER arc tangency point (PST)
      (setq PEX (* IRAD (RFL:SPIRALFXR TH)) PEY (* DIR IRAD (RFL:SPIRALFYR TH)))
      (setq PST (list (+ TX (- (* PEX CPHI) (* PEY SPHI)))
                      (+ TY (+ (* PEX SPHI) (* PEY CPHI)))
                )
      )
      ;; start of the tail = the OUTER arc tangency point (SPT)
      (setq SEX (* ORAD (RFL:SPIRALFXR THL)) SEY (* DIR ORAD (RFL:SPIRALFYR THL)))
      (setq SPT (list (+ TX (- (* SEX CPHI) (* SEY SPHI)))
                      (+ TY (+ (* SEX SPHI) (* SEY CPHI)))
                )
      )
      (setq AEND (+ PHI (* DIR TH)))
      (setq DAX CPHI DAY SPHI DBX (cos AEND) DBY (sin AEND))
      (setq CRS (- (* DAX DBY) (* DAY DBX)))
      (if (> (abs CRS) 1.0e-12)
       (progn
        (setq PX (- (car PST) (car PLT)) PY (- (cadr PST) (cadr PLT)))
        (setq TT (/ (- (* PX DBY) (* PY DBX)) CRS))
        (setq PLTST (list (+ (car PLT) (* TT DAX)) (+ (cadr PLT) (* TT DAY))))
        (RFL:DRAWSPIRAL PLT PLTST PST LO 0.0)
        (setq SPRLN (entlast))
        (initget "Yes No")
        (setq ANS (getkword "\nAccept this spiral? [Yes/No] <No> : "))
        (if (null ANS) (setq ANS "No"))
        (if (= ANS "Yes")
         (progn
          (setq OK T)
          ;; ---- trim/extend the OUTER arc to the tail start (SPT) ----
          ;; keep the piece that runs AWAY from the new spiral: the spiral
          ;; leaves SPT heading DS, so the arc must leave SPT heading DS+pi.
          (setq AT (angle OC SPT))
          (setq WX (/ (- (car SPT) (car OC)) ORAD) WY (/ (- (cadr SPT) (cadr OC)) ORAD))
          (setq DS (+ PHI (* DIR THL)) WDX (- (cos DS)) WDY (- (sin DS)))
          (if (> (- (* WX WDY) (* WY WDX)) 0.0)
           (setq ELOUT (subst (cons 50 AT) (assoc 50 ELOUT) ELOUT))
           (setq ELOUT (subst (cons 51 AT) (assoc 51 ELOUT) ELOUT))
          )
          (entmod ELOUT)
          (entupd EOUT)
          ;; ---- trim/extend the INNER arc to the tail end (PST) ----
          ;; the spiral arrives at PST heading DE, so the arc must leave PST
          ;; heading DE (same direction, continuing away from the spiral).
          (setq AT (angle IC PST))
          (setq WX (/ (- (car PST) (car IC)) IRAD) WY (/ (- (cadr PST) (cadr IC)) IRAD))
          (setq DE (+ PHI (* DIR TH)) WDX (cos DE) WDY (sin DE))
          (if (> (- (* WX WDY) (* WY WDX)) 0.0)
           (setq ELIN (subst (cons 50 AT) (assoc 50 ELIN) ELIN))
           (setq ELIN (subst (cons 51 AT) (assoc 51 ELIN) ELIN))
          )
          (entmod ELIN)
          (entupd EIN)
         )
         (progn
          (entdel SPRLN)
          (setq DIR (- DIR))
         )
        )
       )
       (progn
        (princ "\nRFL:FITSPIRALAA - spiral delta is too large (the two tangents are parallel), so nothing was drawn.")
        (setq OK T)
       )
      )
     )
    )
   )
  )
 )
 (princ)
)
