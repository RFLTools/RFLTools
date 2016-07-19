;
;
;     Program written by Robert Livingston, 2014-04-30
;
;
(defun RFL:ANGLE3P (P1 P2 P3 / ANG)
 (if (> (setq ANG (abs (- (angle P2 P1) (angle P2 P3)))) pi)
  (- (* 2.0 pi) ANG)
  ANG
 )
)
