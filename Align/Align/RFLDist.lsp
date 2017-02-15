;
;
;   Program written by Robert Livingston, 99/12/03
;
;   RFL:DIST returns the length of an alignment entity
;
;
(defun RFL:DIST (P1 P2 BULGE / ATOTAL CHORD R)
 (if (listp BULGE)
  (progn
   (- (RFL:GETSPIRALLS2 (nth 0 BULGE) (nth 1 BULGE) (nth 2 BULGE)) (nth 3 BULGE))
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
