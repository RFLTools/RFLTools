;
;
;     Program written by Robert Livingston, 2014-04-30
;
;
(defun RFL:COMB3 (N / A B C RES TMP)
 (setq RES nil)
 (if (>= N 3)
  (progn
   (setq A 1)
   (while (<= A (- N 2))
    (setq B (+ A 1))
    (while (<= B (- N 1))
     (setq C (+ B 1))
     (while (<= C N)
      (setq TMP (list A B C))
      (setq RES (append RES (list TMP)))
      (setq C (+ C 1))
     )
     (setq B (+ B 1))
    )
    (setq A (+ A 1))
   )
  )
 )
 (setq RES RES)
)
