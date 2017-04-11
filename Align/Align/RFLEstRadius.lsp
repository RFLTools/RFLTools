;
;
;     Program written by Robert Livingston, 2017-04-11
;
;     RFL:ESTRADIUS estimates the radius at a given station by finding points within a distance D
;
;
(setq RFL:ESTRADIUSDIST 30.0)
(setq RFL:ESTRADIUSNUMPOINTS 10)
(defun RFL:ESTRADIUS (STA / DLIST N1 N2 OS P PC PLIST)
 (setq DLIST nil)
 (if (setq P (RFL:XY (list STA 0.0)))
  (if (setq DLIST (mapcar '(lambda (N1) (append (list (distance P (cadr N1))) (cadr N1))) RFL:ALIGNLIST))
   (if (setq DLIST (vl-sort DLIST (function (lambda (N1 N2) (< (car N1) (car N2))))))
    (progn
     (setq PLIST nil)
     (foreach N1 DLIST
      (if (and (or (= RFL:ESTRADIUSDIST nil) (<= (car N1) RFL:ESTRADIUSDIST))
               (or (= RFL:ESTRADIUSNUMPOINTS nil) (< (length PLIST) RFL:ESTRADIUSNUMPOINTS))
          )
       (setq PLIST (append PLIST (list (list (cadr N1) (caddr N1)))))
      )
     )
     (if (> (length PLIST) 2)
      (if (setq PC (RFL:BESTCIRCLE PLIST))
       (if (setq OS (cadr (RFL:STAOFF (car PC))))
        (if (< OS 0.0)
         (cadr PC)
         (* -1.0 (cadr PC))
        )
        nil
       )
       nil
      )
      nil
     )
    )
    nil
   )
   nil
  )
  nil
 )
)
