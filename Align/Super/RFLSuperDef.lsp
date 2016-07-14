;
;
;   Program written by Robert Livingston, 99/10/08
;
;   RFL:SUPERDEF calculating superelevations from supplied entity set and setting RFL:SUPERLIST
;
;
;(defun RFL:SUPERDEF (ENTSET / C ENT ENTLIST P PT SORTSUPER SUPERLEFT SUPERRIGHT SUPERLIST2)
(defun RFL:SUPERDEF (ENTSET)
 (defun SORTSUPER (SL / A B)
  (vl-sort SL '(lambda (A B) (< (car A) (car B))))
 )
 (setq RFL:SUPERLIST nil)
 (setq SUPERLIST2 nil)
 (if (and (/= RFL:ALIGNLIST nil) (/= RFL:STAOFF nil))
  (progn
   (setq C 0)
   (while (< C (sslength ENTSET))
    (setq ENT (ssname ENTSET C))
    (setq ENTLIST (entget ENT))
    (if (and (= (cdr (assoc 0 ENTLIST)) "INSERT")
             (= (strcase (cdr (assoc 2 ENTLIST))) "SUPER")
             (= (cdr (assoc 66 ENTLIST)) 1))
     (progn
      (setq PT (cdr (assoc 10 ENTLIST)))
      (setq P (RFL:STAOFF PT))
      ;
      ; The following are to 'nudge' the point - sometimes RFL:STAOFF returns nil when the point is an an entity endpoint
      ;
      (if (= nil P) (setq P (RFL:STAOFF (list (+ (car PT) 0.00000001) (+ (cadr PT) 0.00000001)))))
      (if (= nil P) (setq P (RFL:STAOFF (list (- (car PT) 0.00000001) (- (cadr PT) 0.00000001)))))
      (if (= nil P) (setq P (RFL:STAOFF (list (+ (car PT) 0.00000001) (- (cadr PT) 0.00000001)))))
      (if (= nil P) (setq P (RFL:STAOFF (list (- (car PT) 0.00000001) (+ (cadr PT) 0.00000001)))))
      (if (/= P nil)
       (progn
        (setq SUPERLEFT nil)
        (setq SUPERRIGHT nil)
        (setq ENT (entnext ENT))
        (setq ENTLIST (entget ENT))
        (while (/= (cdr (assoc 0 ENTLIST)) "SEQEND")
         (if (= (cdr (assoc 2 ENTLIST)) "LEFT") (setq SUPERLEFT (atof (cdr (assoc 1 ENTLIST)))))
         (if (= (cdr (assoc 2 ENTLIST)) "RIGHT") (setq SUPERRIGHT (atof (cdr (assoc 1 ENTLIST)))))
         (setq ENT (entnext ENT))
         (setq ENTLIST (entget ENT))
        )
        (setq SUPERLIST2 (append SUPERLIST2 (list (list (car P) SUPERLEFT SUPERRIGHT))))
       )
      )
     )
    )
    (setq C (+ C 1))
   )
  )
 )
 (setq RFL:SUPERLIST (SORTSUPER SUPERLIST2))
 (princ (strcat "\n" (itoa (length RFL:SUPERLIST)) " nodes found."))
 T
)
