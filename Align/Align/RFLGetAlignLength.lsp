;
;
;     Program written by Robert Livingston, 2016/07/07
;
;     RFL:GETALIGNLENGTH returns the length the alignment defined by ALIGNLIST
;
;
(defun RFL:GETALIGNLENGTH (/ DIST)
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
 (if (= ALIGNLIST nil)
  (progn
   nil
  )
  (progn
   (- (+ (car (last ALIGNLIST))
         (DIST (cadr (last ALIGNLIST)) (caddr (last ALIGNLIST)) (cadddr (last ALIGNLIST)))
      )
      (car (car ALIGNLIST))
   )
  )
 )
)
