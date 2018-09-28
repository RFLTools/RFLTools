;
;     Program written by Robert Livingston, 2018-09-28
;
;     RFL:BESTLINE computes the least squares best line for a series of points
;
;     Following this code: https://www.varsitytutors.com/hotmath/hotmath_help/topics/line-of-best-fit
;
;
(defun RFL:BESTLINE (PLIST / ANG ANG2 B CALCE D M N P P0 P1 PAVE SXY SXX U ULIST)
 (defun CALCE (P0 ANG PLIST / P Y YMAX)
  (setq YMAX nil)
  (foreach P PLIST
   (progn
    (setq Y (- (* (- (cadr P) (cadr P0)) (cos ANG))
               (* (- (car P) (car P0)) (sin ANG))
            )
    )
    (if (= YMAX nil)
     (setq YMAX (abs Y))
     (if (> (abs Y) YMAX)
      (setq YMAX (abs Y))
     )
    )
   )
  )
 )
 (if (< (length PLIST) 2)
  nil
  (progn
   (setq N 0)
   (setq PAVE (list 0.0 0.0))
   (foreach P PLIST
    (progn
     (setq PAVE (list (+ (car PAVE) (car P))
                      (+ (cadr PAVE) (cadr P))
                )
     )
     (setq N (1+ N))
    )
   )
   (setq PAVE (list (/ (car PAVE) N)
                    (/ (cadr PAVE) N)
              )
   )
   (setq ANG (angle (car PLIST) (last PLIST)))
   (setq ULIST nil)
   (foreach P PLIST
    (progn
     (setq ULIST (append ULIST (list (list (+ (* (- (car P) (car PAVE)) (cos ANG))
                                              (* (- (cadr P) (cadr PAVE)) (sin ANG))
                                           )
                                           (- (* (- (cadr P) (cadr PAVE)) (cos ANG))
                                              (* (- (car P) (car PAVE)) (sin ANG))
                                           )
                                     )
                               )
                 )
     )
    )
   )
   (setq SXX 0.0 SXY 0.0)
   (foreach U ULIST
    (progn
     (setq SXY (+ SXY (* (car U)(cadr U))))
     (setq SXX (+ SXX (expt (car U) 2)))
    )
   )
   (setq M (/ SXY SXX))
   (setq B (- (cadr PAVE) (* M (car PAVE))))
   (setq ANG2 (+ ANG (atan M)))
   (setq P (car PLIST))
   (setq D (+ (* (- (car P) (car PAVE)) (cos ANG2))
              (* (- (cadr P) (cadr PAVE)) (sin ANG2))
           )
   )
   (setq P0 (list (+ (car PAVE) (* D (cos ANG2)))
                  (+ (cadr PAVE) (* D (sin ANG2)))
            )
   )
   (setq P (last PLIST))
   (setq D (+ (* (- (car P) (car PAVE)) (cos ANG2))
              (* (- (cadr P) (cadr PAVE)) (sin ANG2))
           )
   )
   (setq P1 (list (+ (car PAVE) (* D (cos ANG2)))
                  (+ (cadr PAVE) (* D (sin ANG2)))
            )
   )
   (list P0 P1 (CALCE PAVE ANG2 PLIST))
  )
 )
)