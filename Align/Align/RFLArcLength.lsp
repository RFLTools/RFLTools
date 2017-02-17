;
;
;     Program written by Robert Livingston, 98/06/12
;
;     RFL:ARCLENGTH returns the length of an arc defined by 2 points and a bulge
;
;
(defun RFL:ARCLENGTH (P1 P2 BULGE / ATOTAL CHORD R)
 (if (listp BULGE)
  (- (RFL:GETSPIRALLS2 (car BULGE) (cadr BULGE) (caddr BULGE)) (cadddr BULGE))
  (progn
   (setq ATOTAL (* 4 (atan (abs BULGE)))
         CHORD (distance P1 P2)
   )
   (if (= 0.0 BULGE)
    CHORD
    (progn 
     (setq R (/ CHORD (* 2 (sin (/ ATOTAL 2)))))
     (* R ATOTAL)
    )
   )
  )
 )
)
