;
;
;     Program written by Robert Livingston, 2014-04-30
;
;
(defun RFL:RADIUS3P (P1 P2 P3 / FCTN RES)
 (defun FCTN (P1 P2 P3)
  (/ (distance P2 P3) (sin (- (angle P1 P3) (angle P1 P2))) 2.0)
 )
 (if (vl-catch-all-error-p (setq RES (vl-catch-all-apply 'FCTN (list P1 P2 P3))))
  nil
  (abs RES)
 )
)
