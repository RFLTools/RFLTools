;
;
;     Program written by Robert Livingston, 2014-04-30
;
;
(defun RFL:CENTER (P1 P2 BULGE / ANG ATOTAL CHORD D R X Y)
 (setq ATOTAL (* 4.0 (atan (abs BULGE))))
 (setq CHORD (distance P1 P2))
 (if (< (abs BULGE) TOL)
  (progn
   (eval nil)
  )
  (progn
   (setq R (/ CHORD (* 2 (sin (/ ATOTAL 2)))))
   (setq ANG (angle P1 P2))
   (setq D (distance P1 P2))
   (setq X (/ D 2.0))
   (setq Y (* (sqrt (- (* R R) (* X X))) (SIGN BULGE) (SIGN (- (abs BULGE) 1.0))))
   (list (+ (+ (car P1) (* X (cos ANG))) (* Y (sin ANG)))
         (- (+ (cadr P1) (* X (sin ANG))) (* Y (cos ANG)))
   )
  )
 )
)
