;
;
;     Program written by Robert Livingston, 2018-04-26
;
;     RFL:GETSUBPLIST returns a sub-list of points between closest to P1 and closest to P2
;
;
(defun RFL:GETSUBPLIST (PLIST P1 P2 / C C1 C2 N1 N2 PLISTI PLIST TMP)
 (setq PLISTOUT nil)
 (setq C 0)
 (setq PLISTI nil)
 (while PLIST
  (setq PLISTI (append PLISTI (list (cons C (car PLIST)))))
  (setq PLIST (cdr PLIST))
  (setq C (1+ C))
 )
 (if (setq C1 (caar (vl-sort PLISTI (function (lambda (N1 N2) (< (distance P1 (cdr N1)) (distance P1 (cdr N2))))))))
  (if (setq C2 (caar (vl-sort PLISTI (function (lambda (N1 N2) (< (distance P2 (cdr N1)) (distance P2 (cdr N2))))))))
   (progn
    (if (> C1 C2) (setq TMP C1 C1 C2 C2 TMP))
    (setq C C1)
    (while (<= C C2)
     (setq PLIST (append PLIST (list (cdr (nth C PLISTI)))))
     (setq C (1+ C))
    )
   )
  )
 )
 PLIST
)
