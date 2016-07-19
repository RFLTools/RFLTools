;
;
;     Program written by Robert Livingston, 2014-04-30
;
;
(defun RFL:CIRCLE3P (P1 P2 P3 / CALCC3P TOL X1 X2 X3 Y1 Y2 Y3)
 
 (defun CALCC3P (X1 Y1 X2 Y2 X3 Y3 / DEN M12 M23 NUM R RES XC YC)
  (setq M12 (/ (- Y2 Y1) (- X2 X1)))
  (setq M23 (/ (- Y3 Y2) (- X3 X2)))
  (setq NUM (- (+ (* M12 M23 (- Y1 Y3)) (* M23 (+ X1 X2))) (* M12 (+ X2 X3))))
  (setq DEN (* 2.0 (- M23 M12)))
  (setq XC (/ NUM DEN))
  (setq YC (- (/ (+ Y1 Y2) 2.0) (* (/ 1.0 M12) (- XC (/ (+ X1 X2) 2.0)))))
  (setq R (sqrt (+ (expt (- X1 XC) 2.0) (expt (- Y1 YC) 2.0))))
  (list (list XC YC) R)
 )
 
 (setq TOL 1e-10)
 (setq X1 (car P1))
 (setq Y1 (cadr P1))
 (setq X2 (car P2))
 (setq Y2 (cadr P2))
 (setq X3 (car P3))
 (setq Y3 (cadr P3))
 
 (setq RES (vl-catch-all-apply 'CALCC3P (list X1 Y1 X2 Y2 X3 Y3)))
 (if (vl-catch-all-error-p RES)
  (progn
   (setq RES (vl-catch-all-apply 'CALCC3P (list X2 Y2 X3 Y3 X1 Y1)))
   (if (vl-catch-all-error-p RES)
    (progn
     (setq RES (vl-catch-all-apply 'CALCC3P (list X3 Y3 X1 Y1 X2 Y2)))
     (if (vl-catch-all-error-p RES)
      (eval nil)
      (setq RES RES)
     )
    )
    (setq RES RES)
   )
  )
  (setq RES RES)
 )
)
