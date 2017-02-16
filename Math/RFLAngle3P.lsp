;
;
;     Program written by Robert Livingston, 2014-04-30
;
;
(defun RFL:ANGLE3P (P1 P2 P3 / ANG)
 (setq ANG (- (angle P2 P1) (angle P2 P3)))
 (if (< ANG 0.0) (setq ANG (* -1.0 ANG)))
 (if (> ANG pi) (setq ANG (- (* 2.0 pi) ANG)))
 ANG
)
