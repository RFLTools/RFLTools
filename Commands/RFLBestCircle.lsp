;
;
;     Program written by Robert Livingston, 2015/03/16
;
;     RFL:BESTCIRCLE is a utility for finding best fit circle along a selected polyline or points
;
;
;;;(defun RFL:BESTCIRCLE (PLIST / C CALCE CALCSUME2 COMBLIST COUNT E P PC PCT NODE R RT STEP SUME2 SUME2T SUME2T1 SUME2T2 SUME2T3 SUME2T4 SUME2T5 SUME2T6)
;;; (defun CALCSUME2 (PC R PLIST / SUME2 NODE)
;;;  (setq SUME2 0.0)
;;;  (foreach NODE PLIST
;;;   (setq SUME2 (+ SUME2 (expt (abs (- (distance PC NODE) R)) 2)))
;;;  )
;;;  SUME2
;;; )
;;; (defun CALCE (PC R PLIST / E NODE TMP)
;;;  (setq E nil)
;;;  (foreach NODE PLIST
;;;   (progn
;;;    (setq TMP (abs (- (distance PC NODE) R)))
;;;    (if (= E nil)
;;;     (setq E TMP)
;;;     (if (> TMP E) (setq E TMP))
;;;    )
;;;   )
;;;  )
;;;  E
;;; )
;;; (if (= nil (listp PLIST))
;;;  nil
;;;  (if (< (length PLIST) 3)
;;;   nil
;;;   (if (= (length PLIST) 3)
;;;    (if (= nil (setq PC (RFL:CIRCLE3P (car PLIST) (cadr PLIST) (caddr PLIST))))
;;;     nil
;;;     (append PC (list 0.0))
;;;    )
;;;    (progn
;;;     (setq C 0)
;;;     (setq PC nil)
;;;     (setq R 0.0)
;;;     (if (> (length PLIST) 8)
;;;      (setq COMBLIST (list (list 1 (/ (length PLIST) 2) (length PLIST))))
;;;      (setq COMBLIST (RFL:COMB3 (length PLIST)))
;;;     )
;;;     (foreach NODE COMBLIST
;;;      (progn
;;;       (if (/= nil (setq P (RFL:CIRCLE3P (nth (- (car NODE) 1) PLIST) (nth (- (cadr NODE) 1) PLIST) (nth (- (caddr NODE) 1) PLIST))))
;;;        (progn
;;;         (setq C (+ C 1))
;;;         (if (= nil PC)
;;;          (setq PC (car P))
;;;          (setq PC (list (+ (car PC) (caar P)) (+ (cadr PC) (cadar P))))
;;;         )
;;;         (setq R (+ R (cadr P)))
;;;        )
;;;       )
;;;      )
;;;     )
;;;     (if (= C 0)
;;;      nil
;;;      (progn
;;;       (setq PC (list (/ (car PC) C) (/ (cadr PC) C)))
;;;       (setq R  (/ R C))
;;;       ; REGRESSION
;;;       (setq COUNT 0)
;;;       (setq STEP R)
;;;       (setq SUME2 (CALCSUME2 PC R PLIST))
;;;       (while (and (> STEP RFL:TOL)
;;;                   (< COUNT 10000)
;;;              )
;;;        (setq RT (+ R STEP))
;;;        (setq PCT PC)
;;;        (setq SUME2T1 (CALCSUME2 PCT RT PLIST))
;;;        (setq RT (- R STEP))
;;;        (setq PCT PC)
;;;        (setq SUME2T2 (CALCSUME2 PCT RT PLIST))
;;;        (setq RT R)
;;;        (setq PCT (list (+ (car PC) STEP) (cadr PC)))
;;;        (setq SUME2T3 (CALCSUME2 PCT RT PLIST))
;;;        (setq RT R)
;;;        (setq PCT (list (- (car PC) STEP) (cadr PC)))
;;;        (setq SUME2T4 (CALCSUME2 PCT RT PLIST))
;;;        (setq RT R)
;;;        (setq PCT (list (car PC) (+ (cadr PC) STEP)))
;;;        (setq SUME2T5 (CALCSUME2 PCT RT PLIST))
;;;        (setq RT R)
;;;        (setq PCT (list (car PC) (- (cadr PC) STEP)))
;;;        (setq SUME2T6 (CALCSUME2 PCT RT PLIST))
;;;        (setq SUME2 (min SUME2 SUME2T1 SUME2T2 SUME2T3 SUME2T4 SUME2T5 SUME2T6))
;;;        (cond ((= SUME2 SUME2T1) (setq R (+ R STEP)))
;;;              ((= SUME2 SUME2T2) (setq R (- R STEP)))
;;;              ((= SUME2 SUME2T3) (setq PC (list (+ (car PC) STEP) (cadr PC))))
;;;              ((= SUME2 SUME2T4) (setq PC (list (- (car PC) STEP) (cadr PC))))
;;;              ((= SUME2 SUME2T5) (setq PC (list (car PC) (+ (cadr PC) STEP))))
;;;              ((= SUME2 SUME2T6) (setq PC (list (car PC) (- (cadr PC) STEP))))
;;;              (T (setq STEP (/ STEP 2.0)))
;;;        )
;;;        (setq COUNT (+ COUNT 1))(if (= 10000 COUNT) (princ "\nMaximum itterations reached!\n"))
;;;       )
;;;       ; END REGRESSION
;;;       (list PC R (CALCE PC R PLIST))
;;;      )
;;;     )
;;;    )
;;;   )
;;;  )
;;; )
;;;)
;
; Uses formulae here:
;
; https://dtcenter.org/met/users/docs/write_ups/circle_fit.pdf
;
(defun RFL:BESTCIRCLE (PLIST / CALCE N P PAVE PC R SU SUU SUUU SUV SUVV SV SVV SVVV SVUU U UC ULIST X1 X2 Y1 Y2 Z1 Z2)
 (defun CALCE (PC R PLIST / E NODE TMP)
  (setq E nil)
  (foreach NODE PLIST
   (progn
    (setq TMP (abs (- (distance PC NODE) R)))
    (if (= E nil)
     (setq E TMP)
     (if (> TMP E) (setq E TMP))
    )
   )
  )
  E
 )
 (if (= nil (listp PLIST))
  nil
  (if (< (length PLIST) 3)
   nil
   (if (= (length PLIST) 3)
    (if (= nil (setq PC (RFL:CIRCLE3P (car PLIST) (cadr PLIST) (caddr PLIST))))
     nil
     (append PC (list 0.0))
    )
    (progn
     (setq N 0)
     (setq PAVE (list 0.0 0.0))
     (foreach P PLIST
      (progn
       (setq N (1+ N))
       (setq PAVE (list (+ (car PAVE) (car P))
                        (+ (cadr PAVE) (cadr P))
                  )
       )
      )
     )
     (setq PAVE (list (/ (car PAVE) N) (/ (cadr PAVE) N)))
     (setq ULIST nil)
     (foreach P PLIST
      (progn
       (setq ULIST (append ULIST (list (list (- (car P) (car PAVE))
                                             (- (cadr P) (cadr PAVE))
                                       )
                                 )
                   )
       )
      )
     )
     (setq SU 0.0 SUU 0.0 SUUU 0.0 SV 0.0 SVV 0.0 SVVV 0.0 SUV 0.0 SUVV 0.0 SVUU 0.0)
     (foreach U ULIST
      (progn
       (setq SU (+ SU (car U)))
       (setq SUU (+ SUU (expt (car U) 2)))
       (setq SUUU (+ SUUU (expt (car U) 3)))
       (setq SV (+ SV (cadr U)))
       (setq SVV (+ SVV (expt (cadr U) 2)))
       (setq SVVV (+ SVVV (expt (cadr U) 3)))
       (setq SUV (+ SUV (* (car U) (cadr U))))
       (setq SUVV (+ SUVV (* (car U) (expt (cadr U) 2))))
       (setq SVUU (+ SVUU (* (expt (car U) 2) (cadr U))))
      )
     )
     (setq X1 SUU Y1 SUV Z1 (/ (+ SUUU SUVV) 2.0) X2 SUV Y2 SVV Z2 (/ (+ SVVV SVUU) 2.0))
     (setq UC (list (/ (- (/ Z1 Y1) (/ Z2 Y2)) (- (/ X1 Y1) (/ X2 Y2)))
                    (/ (- (/ Z1 X1) (/ Z2 X2)) (- (/ Y1 X1) (/ Y2 X2)))
              )
     )
     (setq PC (list (+ (car UC) (car PAVE)) (+ (cadr UC) (cadr PAVE))))
     (setq R (sqrt (+ (expt (car UC) 2) (expt (cadr UC) 2) (/ (+ SUU SVV) N))))
     (list PC R (CALCE PC R PLIST))
    )
   )
  )
 )
)