;
;
;     Program written by Robert Livingston, 2014-04-30
;
;
(defun RFL:RADIUS (P1 P2 BULGE / ATOTAL CHORD)
 (setq ATOTAL (* 4.0 (atan (abs BULGE))))
 (setq CHORD (distance P1 P2))
 (if (< (abs BULGE) TOL)
  (progn
   (eval nil)
  )
  (progn
   ;(setq R (/ CHORD (* 2 (sin (/ ATOTAL 2)))))
   (/ CHORD (* 2 (sin (/ ATOTAL 2))))
  )
 )
)
