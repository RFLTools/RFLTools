;
;
;     Program written by Robert Livingston, 2014-04-30
;
;
(defun RFL:RADIUS (P1 P2 BULGE / ATOTAL CHORD)
 (if (listp BULGE)
  (setq R (RFL:GETSPIRALR2 (nth 0 BULGE) (nth 1 BULGE) (nth 2 BULGE)))
  (progn
   (setq ATOTAL (* 4.0 (atan (abs BULGE))))
   (setq CHORD (distance P1 P2))
   (if (< (abs BULGE) TOL)
    (setq R nil)
    (setq R (/ CHORD (* 2 (sin (/ ATOTAL 2)))))
   )
  )
 )
 R
)
