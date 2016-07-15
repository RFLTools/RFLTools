;
;
;   Program written by Robert Livingston, 98/06/12
;
;   RFL:XY returns a list of (X Y) for a provided (STA OFFSET)
;
;
(if RFL:XY (princ "\nRFL:XY already loaded...")
(defun RFL:XY (P / ANG AL ALTMP C D DIST OFFSET P1 P2 PC POINT STA X Y TOL)
 (setq TOL 0.00000001)
 (defun POINT (P1 P2 BULGE L / A ATOTAL C CHORD LTOTAL P PC R SB X Y)
  (setq CHORD (distance P1 P2))
  (if (< (abs BULGE) TOL)
   (progn
    (list (+ (* (/ L CHORD) (- (car P2) (car P1))) (car P1))
          (+ (* (/ L CHORD) (- (cadr P2) (cadr P1))) (cadr P1)))
   )
   (progn
    (setq ATOTAL (* 4.0 (atan (abs BULGE))))
    (setq PC (RFL:CENTER P1 P2 BULGE))
    (setq R (RFL:RADIUS P1 P2 BULGE))
    (setq A (+ (angle PC P1) (* (RFL:SIGN BULGE) (/ L R))))
    (list (+ (car PC) (* R (cos A)))
          (+ (cadr PC) (* R (sin A))))
   )
  )
 )
 (defun DIST (P1 P2 BULGE / ATOTAL CHORD R)
  (if (listp BULGE)
   (progn
    (- (RFL:GETSPIRALLS2 (car BULGE) (cadr BULGE) (caddr BULGE)) (cadddr BULGE))
   )
   (progn
    (setq ATOTAL (* 4.0 (atan (abs BULGE))))
    (setq CHORD (distance P1 P2))
    (if (= 0.0 BULGE)
     (eval CHORD)
     (progn 
      (setq R (/ CHORD (* 2 (sin (/ ATOTAL 2)))))
      (* R ATOTAL)
     )
    )
   )
  )
 )
 (if (/= nil RFL:ALIGNLIST)
  (progn
   (setq STA (car P))
   (setq OFFSET (cadr P))
   (setq AL (last RFL:ALIGNLIST))
   (if (<= STA (+ (car AL) (DIST (cadr AL) (caddr AL) (cadddr AL))))
    (progn
     (setq AL (car RFL:ALIGNLIST))
     (setq ALTMP (cdr RFL:ALIGNLIST))
     (if (>= STA (car AL))
      (progn
       (while (> STA (+ (car AL) (DIST (cadr AL) (caddr AL) (cadddr AL))))
        (setq AL (car ALTMP))
        (setq ALTMP (cdr ALTMP))
       )
       (if (listp (cadddr AL))
        (progn
         (if (< (distance (caddr AL) (caddr (cadddr AL))) (distance (cadr AL) (caddr (cadddr AL))))
          (progn
           (setq P1 (RFL:SPIRALXY2 (list (+ (- STA
                                           (car AL)
                                        )
                                        (cadddr (cadddr AL))
                                     )
                                     OFFSET
                               )
                               (car (cadddr AL))
                               (cadr (cadddr AL))
                               (caddr (cadddr AL))
                    )
           )
          )
          (progn
           (setq P1 (RFL:SPIRALXY2 (list (- (RFL:GETSPIRALLS2 (car (cadddr AL))
                                                              (cadr (cadddr AL))
                                                              (caddr (cadddr AL))
                                            )
                                            (- STA
                                               (car AL)
                                            )
                                         )
                                         (* -1.0 OFFSET)
                                   )
                                   (car (cadddr AL))
                                   (cadr (cadddr AL))
                                   (caddr (cadddr AL))
                    )
           )
          )
         )
        )
        (progn
         (setq P2 (POINT (cadr AL) (caddr AL) (cadddr AL) (- STA (car AL))))
         (if (< (abs (cadddr AL)) TOL)
          (progn
           (setq ANG (angle (cadr AL) (caddr AL)))
           (setq D (distance (cadr AL) P2))
           (setq P1 (list (+ (+ (car (cadr AL)) (* D (cos ANG))) (* OFFSET (sin ANG)))
                          (- (+ (cadr (cadr AL)) (* D (sin ANG))) (* OFFSET (cos ANG)))
                    )
           )
          )
          (progn
           (setq PC (RFL:CENTER (cadr AL) (caddr AL) (cadddr AL)))
           (if (< (cadddr AL) 0.0)
            (setq ANG (angle P2 PC))
            (setq ANG (angle PC P2))
           )
           (setq P1 (list (+ (car P2) (* OFFSET (cos ANG)))
                          (+ (cadr P2) (* OFFSET (sin ANG)))
                    )
           )
          )
         )
        )
       )
      )
      (progn
       (princ "\n**** STATION OUT OF RANGE ****")
       nil
      )
     )
    )
    (progn
     (princ "\n**** STATION OUT OF RANGE ****")
     nil
    )
   )
  )
  (progn
   (princ "\n**** NO ALIGNMENT DEFINED ****")
   nil
  )
 )
)
)
