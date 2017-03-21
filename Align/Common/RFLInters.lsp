;
;
;     Program written by Robert Livingston, 2016-09-23
;
;     RFL:INTERS returns the intersection of a line defined by P1/P2 and an RFL alignment
;
;
(defun RFL:ALINTERS (P1 P2 RFL:ALIGNLIST / ALSAVE C OS P SWAP TOL)
 (setq TOL 0.00001)
 (defun SWAP (/ TMP)
  (setq TMP RFL:ALIGNLIST RFL:ALIGNLIST ALSAVE ALSAVE TMP)
 )
 (setq C 0)
 (setq P (list (/ (+ (car P1) (car P2)) 2.0) (/ (+ (cadr P1) (cadr P2)) 2.0)))
 (setq ALSAVE (list (list 0.0 P1 P2 0.0)))
 (setq P (RFL:STAOFF P))
 (while (and P
             (> (abs (cadr P)) TOL)
             (< C 100)
        )
  (setq P (RFL:XY (list (car P) 0.0)))
  (SWAP)
  (setq P (RFL:STAOFF P))
  (setq C (+ C 1))
  (if (>= C 100)
   (princ (strcat "\n*** Warning - Maximum number of iterations reached at station " (rtos STA) "\n"))
  )
 )
 (if P (setq P (RFL:XY (list (car P) 0.0))))
 P
)
