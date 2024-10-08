;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:FITSPIRALLL Fits a reverse engineered DCA spiral between two lines
;
;
(defun RFL:FITSPIRALLL (ENT1 ENT2 LS1 R LS2 / ANG ANG1 ANG2 DELTA DIR ENTLIST1 ENTLIST2
                                              P P1 P2 P3 P4 PLT1 PLT2 PLTST1 PLTST2 PST1 PST2
                                              R T1 T2 THETA1 THETA2 VAL1 VAL2)
 (setq ENTLIST1 (entget ENT1))
 (setq ENTLIST2 (entget ENT2))
 (setq P1 (cdr (assoc 10 ENTLIST1)))
 (setq P1 (list (car P1) (cadr P1)))
 (setq P2 (cdr (assoc 11 ENTLIST1)))
 (setq P2 (list (car P2) (cadr P2)))
 (setq P3 (cdr (assoc 10 ENTLIST2)))
 (setq P3 (list (car P3) (cadr P3)))
 (setq P4 (cdr (assoc 11 ENTLIST2)))
 (setq P4 (list (car P4) (cadr P4)))
 (setq P (inters P1 P2 P3 P4 nil))
 (setq ANG (RFL:ANGLE3P (if (> (distance P1 P) (distance P2 P)) P1 P2)
                        P
                        (if (> (distance P3 P) (distance P4 P)) P3 P4)
           )
 )
 (if (> (distance P1 P) (distance P2 P))
  (setq ANG1 (angle P P1))
  (setq ANG1 (angle P P2))
 )
 (if (> (distance P3 P) (distance P4 P))
  (setq ANG2 (angle P P3))
  (setq ANG2 (angle P P4))
 )
 (if (> (sin (- ANG2 (+ ANG1 pi))) 0.0)
  (setq DIR 1.0)
  (setq DIR -1.0)
 )
 (if (= R 0.0)
  (progn
   (setq DELTA 0.0)
   (setq THETA1 (/ (- pi ANG) (+ 1.0 (/ LS2 LS1))))
   (setq THETA2 (/ (- pi ANG) (+ 1.0 (/ LS1 LS2))))
   (setq R (/ LS1 (* 2.0 THETA1)))
  )
  (progn
   (setq THETA1 (/ LS1 (* 2.0 R)))
   (setq THETA2 (/ LS2 (* 2.0 R)))
   (setq DELTA (- (- pi ANG) (+ THETA1 THETA2)))
  )
 )
 (if (>= DELTA 0.0)
  (progn
   (setq VAL1 (* R (- (+ (* (RFL:SPIRALFYR THETA1) (sin ANG))
                            (cos (+ DELTA THETA2 (/ pi -2.0)))
                         )
                      (sin THETA2)
                   )
              )
   )
   (setq VAL2 (* R (- (+ (* (RFL:SPIRALFYR THETA2) (sin ANG))
                            (cos (+ DELTA THETA1 (/ pi -2.0)))
                         )
                      (sin THETA1)
                   )
              )
   )
   (setq T1 (/ (+ (* VAL1 (cos ANG))
                  VAL2
               )
               (expt (sin ANG) 2)
            )
   )
   (setq T2 (/ (+ (* VAL2 (cos ANG))
                  VAL1
               )
               (expt (sin ANG) 2)
            )
   )
   (setq PLT1 (list (+ (car P)
                       (* (+ T1
                             (* R (RFL:SPIRALFXR THETA1))
                          )
                          (cos ANG1)
                       )
                    )
                    (+ (cadr P)
                       (* (+ T1
                             (* R (RFL:SPIRALFXR THETA1))
                          )
                          (sin ANG1)
                       )
                    )
              )
   )
   (setq PLTST1 (list (+ (car P)
                         (* (+ T1
                               (if (> THETA1 0.0 )
                                (* R (/ (RFL:SPIRALFYR THETA1) (RFL:TAN THETA1)))
                                0.0
                               )
                            )
                            (cos ANG1)
                         )
                      )
                      (+ (cadr P)
                         (* (+ T1
                               (if (> THETA1 0.0 )
                                (* R (/ (RFL:SPIRALFYR THETA1) (RFL:TAN THETA1)))
                                0.0
                               )
                            )
                            (sin ANG1)
                         )
                      )
                )
   )
   (setq PST1 (list (+ (car P)
                       (* T1
                          (cos ANG1)
                       )
                       (* 1.0
                          DIR
                          (* R (RFL:SPIRALFYR THETA1))
                          (sin ANG1)
                       )
                    )
                    (+ (cadr P)
                       (* T1
                          (sin ANG1)
                       )
                       (* -1.0
                          DIR
                          (* R (RFL:SPIRALFYR THETA1))
                          (cos ANG1)
                       )
                    )
              )
   )
   (if (> THETA1 0.0)
    (RFL:DRAWSPIRAL PLT1 PLTST1 PST1 0.0 0.0)
   )
   (if (> (distance P1 P) (distance P2 P))
    (setq ENTLIST1 (subst (list 11 (car PLT1) (cadr PLT1)) (assoc 11 ENTLIST1) ENTLIST1))
    (setq ENTLIST1 (subst (list 10 (car PLT1) (cadr PLT1)) (assoc 10 ENTLIST1) ENTLIST1))
   )
   (entmod ENTLIST1)
   (entupd ENT1)
   (setq PLT2 (list (+ (car P)
                       (* (+ T2
                             (* R (RFL:SPIRALFXR THETA2))
                          )
                          (cos ANG2)
                       )
                    )
                    (+ (cadr P)
                       (* (+ T2
                             (* R (RFL:SPIRALFXR THETA2))
                          )
                          (sin ANG2)
                       )
                    )
              )
   )
   (setq PLTST2 (list (+ (car P)
                         (* (+ T2
                               (if (> THETA2 0.0)
                                (* R (/ (RFL:SPIRALFYR THETA2) (RFL:TAN THETA2)))
                                0.0
                               )
                            )
                            (cos ANG2)
                         )
                      )
                      (+ (cadr P)
                         (* (+ T2
                               (if (> THETA2 0.0)
                                (* R (/ (RFL:SPIRALFYR THETA2) (RFL:TAN THETA2)))
                                0.0
                               )
                            )
                            (sin ANG2)
                         )
                      )
                )
   )
   (setq PST2 (list (+ (car P)
                       (* T2
                          (cos ANG2)
                       )
                       (* -1.0
                          DIR
                          (* R (RFL:SPIRALFYR THETA2))
                          (sin ANG2)
                       )
                    )
                    (+ (cadr P)
                       (* T2
                          (sin ANG2)
                       )
                       (* 1.0
                          DIR
                          (* R (RFL:SPIRALFYR THETA2))
                          (cos ANG2)
                       )
                    )
              )
   )
   (if (> THETA2 0.0)
    (RFL:DRAWSPIRAL PLT2 PLTST2 PST2 0.0 0.0)
   )
   (if (> (distance P3 P) (distance P4 P))
    (setq ENTLIST2 (subst (list 11 (car PLT2) (cadr PLT2)) (assoc 11 ENTLIST2) ENTLIST2))
    (setq ENTLIST2 (subst (list 10 (car PLT2) (cadr PLT2)) (assoc 10 ENTLIST2) ENTLIST2))
   )
   (entmod ENTLIST2)
   (entupd ENT2)
   (if (> DELTA 0.0)
    (if (= DIR 1.0)
     (command "._ARC" PST1 "E" PST2 "R" R)
     (command "._ARC" PST2 "E" PST1 "R" R)
    )
   )
  )
 )
)
