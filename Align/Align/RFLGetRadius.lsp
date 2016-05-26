;
;
;   Program written by Robert Livingston, 98/06/12
;
;   RFL:GETRADIUS returns the radius at a specified station
;
;
;
(defun RFL:GETRADIUS (STA / AL C DIR R)
 (if (/= nil ALIGNLIST)
  (progn
   (setq AL (last ALIGNLIST))
   (if (<= STA (+ (car AL) (RFL:ARCLENGTH (cadr AL) (caddr AL) (cadddr AL))))
    (progn
     (setq C 0)
     (setq AL (nth C ALIGNLIST))
     (if (>= STA (car AL))
      (progn
       (while (> STA (+ (car AL) (RFL:ARCLENGTH (cadr AL) (caddr AL) (cadddr AL))))
        (setq C (+ C 1))
        (setq AL (nth C ALIGNLIST))
       )
       (if (listp (cadddr AL))
        (progn
         (if (< (distance (caddr AL) (caddr (cadddr AL))) (distance (cadr AL) (caddr (cadddr AL))))
          (progn
           (setq R (GETSPIRALRADIUS (+ (- STA
                                          (car AL)
                                       )
                                       (cadddr (cadddr AL))
                                    )
                               (car (cadddr AL))
                               (cadr (cadddr AL))
                               (caddr (cadddr AL))
                   )
           )
          )
          (progn
           (setq R (* -1.0
                      (GETSPIRALRADIUS (- (GETSPIRALLS2 (car (cadddr AL))
                                                        (cadr (cadddr AL))
                                                        (caddr (cadddr AL))
                                          )
                                          (- STA
                                             (car AL)
                                          )
                                       )
                                       (car (cadddr AL))
                                       (cadr (cadddr AL))
                                       (caddr (cadddr AL))
                   )
                   )
           )
          )
         )
        )
        (progn
         (if (< (abs (cadddr AL)) RFL:TOL)
          (progn
           (setq R 0.0)
          )
          (progn
           (setq DIR (RFL:SIGN (cadddr AL)))
           (setq R (* DIR (RFL:RADIUS (cadr AL) (caddr AL) (cadddr AL))))
          )
         )
        )
       )
      )
      (progn
       (princ "\n**** STATION OUT OF RANGE ****")
       (eval nil)
      )
     )
    )
    (progn
     (princ "\n**** STATION OUT OF RANGE ****")
     (eval nil)
    )
   )
  )
  (progn
   (princ "\n**** NO ALIGNMENT DEFINED ****")
   (eval nil)
  )
 )
)
