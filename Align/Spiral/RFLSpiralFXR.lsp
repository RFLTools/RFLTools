;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:SPIRALFXR returns (R *  Spiral 'X') for a given deflection
;
;
(defun RFL:SPIRALFXR (THETA / AR2 DENOMINATOR N NUMERATOR SUM SUM2)
 (setq SUM -1.0)
 (setq SUM2 0.0)
 (setq AR2 (* 2.0 THETA))
 (setq N 1.0)
 (while (/= SUM SUM2)
  (setq SUM SUM2)
  (if (> THETA RFL:TOLFINE)
   (setq NUMERATOR (* (expt -1.0 (+ N 1.0)) (expt AR2 (* 2.0 (- N 1.0)))))
   (setq NUMERATOR 0.0)
  )
  (setq DENOMINATOR (* (expt 2.0 (* 2.0 (- N 1.0))) (- (* 4.0 N) 3.0) (RFL:FACT (* 2.0 (- N 1.0)))))
  (setq SUM2 (+ SUM2 (/ NUMERATOR DENOMINATOR)))
  (setq N (+ N 1))
 )
 (setq SUM (* SUM AR2))
 SUM
)
