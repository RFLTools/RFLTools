;
;
;     Program written by Robert Livingston, 2020-05-26
;
;     R180 is a simple routine that rotates a text entity or attribute 180 degrees
;
;
(defun C:R180 (/ ENT ENTLIST R)
 (if (setq ENT (car (nentsel)))
  (progn
   (setq ENTLIST (entget ENT))
   (cond ((= "ATTRIB" (cdr (assoc 0 ENTLIST)))
          (progn
           (setq ENTLIST (subst (cons 50 (+ (cdr (assoc 50 ENTLIST)) pi)) (assoc 50 ENTLIST) ENTLIST))
           (entmod ENTLIST)
          )
         )
         ((= "TEXT" (cdr (assoc 0 ENTLIST)))
          (progn
           (setq ENTLIST (subst (cons 50 (+ (cdr (assoc 50 ENTLIST)) pi)) (assoc 50 ENTLIST) ENTLIST))
           (entmod ENTLIST)
          )
         )
   )
  )
 )
)