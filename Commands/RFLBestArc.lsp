;
;
;     Program written by Robert Livingston, 2015/03/16
;
;     RFL:BESTARC is a utility for finding best fit arc along a selected polyline
;
;
;;;(defun RFL:BESTARC (PLIST / ANG BULGE BULGEFIXED COUNT D P P1 P2 R REP SO TOL)
;;; (setq TOL 0.000000001)
;;; (defun SO (P P1 P2 BULGE / ANG ANG1 ANG2 CALCSUME2 D D1 D2 D11 D22 OFFSET PC R STA SUME2 SUME2T1 SUME2T2 SUME2T3 SUME2T4 SUME2T5 SUME2T6 TOL)
;;;  (setq TOL 0.000000001)
;;;  (if (< (abs BULGE) TOL)
;;;   (progn
;;;    (setq D (distance P1 P2))
;;;    (setq D1 (distance P1 P))
;;;    (setq D2 (distance P2 P))
;;;    (setq D11 (/ (+ (* D D)
;;;                    (- (* D1 D1)
;;;                       (* D2 D2)
;;;                    )
;;;                 )
;;;                 (* 2.0 D)
;;;              )
;;;    )
;;;    (setq STA D11)
;;;    (setq OFFSET (sqrt (abs (- (* D1 D1) (* D11 D11)))))
;;;    (setq ANG (- (angle P1 P2) (angle P1 P)))
;;;    (while (< ANG 0.0) (setq ANG (+ ANG (* 2.0 pi))))
;;;    (if (> ANG (/ pi 2.0)) (setq OFFSET (* OFFSET -1.0)))
;;;   )
;;;   (progn
;;;    (setq PC (RFL:CENTER P1 P2 BULGE))
;;;    (if (< BULGE 0.0)
;;;     (setq ANG1 (- (angle PC P1) (angle PC P)))
;;;     (setq ANG1 (- (angle PC P) (angle PC P2)))
;;;    )
;;;    (while (< ANG1 0.0) (setq ANG1 (+ ANG1 (* 2.0 pi))))
;;;    (setq R (RFL:RADIUS P1 P2 BULGE))
;;;    (setq STA (* R ANG1))
;;;    (setq OFFSET (- (distance PC P) R))
;;;    (if (< BULGE 0.0) (setq OFFSET (* -1.0 OFFSET)))
;;;   )
;;;  )
;;;  (list STA OFFSET)
;;; )
;;; (defun CALCSUME2 (P1 P2 BULGE PLIST / P SUME2)
;;;  (setq SUME2 0.0)
;;;  (foreach P PLIST
;;;   (setq SUME2 (+ SUME2 (expt (cadr (SO P P1 P2 BULGE)) 2)))
;;;  )
;;;  SUME2
;;; )
;;; 
;;; (setq BULGEFIXED 0)
;;; (setq R nil)
;;; (while (< BULGEFIXED 2)
;;;  (setq P1 (car PLIST))
;;;  (setq P2 (last PLIST))
;;;  (setq ANG (angle P1 P2))
;;;  (setq D (distance P1 P2))
;;;  (setq BULGE 0.0)
;;;  (setq SUME2 (CALCSUME2 P1 P2 BULGE PLIST))
;;;  (setq COUNT 0)
;;;  (while (> D TOL)
;;;   (if (= BULGEFIXED 1)
;;;    (progn
;;;     (setq BULGE (RFL:BULGE P1 P2 R))
;;;     (setq SUME2T5 nil)
;;;     (setq SUME2T6 nil)
;;;    )
;;;    (progn
;;;     (setq SUME2T5 (CALCSUME2 P1 P2 (+ BULGE (/ D 1000.0)) PLIST))
;;;     (setq SUME2T6 (CALCSUME2 P1 P2 (- BULGE (/ D 1000.0)) PLIST))
;;;    )
;;;   )
;;;   (setq SUME2T1 (CALCSUME2 (list (+ (car P1) (* D (sin ANG))) (- (cadr P1) (* D (cos ANG))))
;;;                            P2
;;;                            (if (= R nil)
;;;                             BULGE
;;;                             (RFL:BULGE (list (+ (car P1) (* D (sin ANG))) (- (cadr P1) (* D (cos ANG))))
;;;                                        P2
;;;                                        R
;;;                             )
;;;                            )
;;;                            PLIST
;;;                 )
;;;   )
;;;   (setq SUME2T2 (CALCSUME2 (list (- (car P1) (* D (sin ANG))) (+ (cadr P1) (* D (cos ANG))))
;;;                            P2
;;;                            (if (= R nil)
;;;                             BULGE
;;;                             (RFL:BULGE (list (- (car P1) (* D (sin ANG))) (+ (cadr P1) (* D (cos ANG))))
;;;                                        P2
;;;                                        R
;;;                             )
;;;                            )
;;;                            PLIST
;;;                 )
;;;   )
;;;   (setq SUME2T3 (CALCSUME2 P1 (list (+ (car P2) (* D (sin ANG))) (- (cadr P2) (* D (cos ANG))))
;;;                            (if (= R nil)
;;;                             BULGE
;;;                             (RFL:BULGE P1
;;;                                        (list (+ (car P2) (* D (sin ANG))) (- (cadr P2) (* D (cos ANG))))
;;;                                        R
;;;                             )
;;;                            )
;;;                            PLIST
;;;                 )
;;;   )
;;;   (setq SUME2T4 (CALCSUME2 P1 (list (- (car P2) (* D (sin ANG))) (+ (cadr P2) (* D (cos ANG))))
;;;                            (if (= R nil)
;;;                             BULGE
;;;                             (RFL:BULGE P1
;;;                                        (list (- (car P2) (* D (sin ANG))) (+ (cadr P2) (* D (cos ANG))))
;;;                                        R
;;;                             )
;;;                            )
;;;                            PLIST
;;;                 )
;;;   )
;;;   (if (= BULGEFIXED 1)
;;;    (setq SUME2 (min SUME2 SUME2T1 SUME2T2 SUME2T3 SUME2T4))
;;;    (setq SUME2 (min SUME2 SUME2T1 SUME2T2 SUME2T3 SUME2T4 SUME2T5 SUME2T6))
;;;   )
;;;   (cond ((= SUME2 SUME2T1) (setq P1 (list (+ (car P1) (* D (sin ANG))) (- (cadr P1) (* D (cos ANG))))))
;;;         ((= SUME2 SUME2T2) (setq P1 (list (- (car P1) (* D (sin ANG))) (+ (cadr P1) (* D (cos ANG))))))
;;;         ((= SUME2 SUME2T3) (setq P2 (list (+ (car P2) (* D (sin ANG))) (- (cadr P2) (* D (cos ANG))))))
;;;         ((= SUME2 SUME2T4) (setq P2 (list (- (car P2) (* D (sin ANG))) (+ (cadr P2) (* D (cos ANG))))))
;;;         ((= SUME2 SUME2T5) (setq BULGE (+ BULGE (/ D 1000.0))))
;;;         ((= SUME2 SUME2T6) (setq BULGE (- BULGE (/ D 1000.0))))
;;;         (T (setq D (/ D 2.0)))
;;;   )
;;;   (setq COUNT (+ COUNT 1))(if (= 10000 COUNT) (exit))
;;;  )
;;;  (setq BULGEFIXED (1+ BULGEFIXED))
;;;  (if (= BULGEFIXED 1)
;;;   (if (= nil (setq R (getreal (strcat "\nSolution found for R = " (rtos (* (RFL:SIGN BULGE) (RFL:RADIUS P1 P2 BULGE))) ", set new R (<return> for calc) = "))))
;;;    (setq BULGEFIXED (1+ BULGEFIXED))
;;;   )
;;;   (setq BULGEFIXED (1+ BULGEFIXED))
;;;  )
;;; )
;;; 
;;; (if (= R nil)
;;;  (list P1 P2 BULGE)
;;;  (list P1 P2 (RFL:BULGE P1 P2 R))
;;; )
;;;)
(defun RFL:BESTARC (PLIST / BULGE P1 P2 PC PCLIST R)
 (if (setq PCLIST (RFL:BESTCIRCLE PLIST))
  (progn
   (setq PC (car PCLIST) R (cadr PCLIST))
   (setq P1 (list (+ (car PC) (* R (cos (angle PC (car PLIST)))))
                  (+ (cadr PC) (* R (sin (angle PC (car PLIST)))))
            )
   )
   (setq P2 (list (+ (car PC) (* R (cos (angle PC (last PLIST)))))
                  (+ (cadr PC) (* R (sin (angle PC (last PLIST)))))
            )
   )
   (if (< (sin (- (angle P1 PC) (angle P1 P2))) 0.0)
    (setq R (* R -1.0))
   )
   (setq BULGE (RFL:BULGE P1 P2 R))
   
   
   (list P1 P2 BULGE)
  )
  nil
 )
)