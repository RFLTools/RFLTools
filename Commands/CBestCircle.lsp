;
;
;     Program written by Robert Livingston, 2015/03/16
;
;     BESTCIRCLE is a utility for finding best fit circle along a selected polyline or points
;
;
(defun C:BESTCIRCLE (/ C ENT ENT2 ENTLIST ENTSET P PLIST)
 (setq PLIST nil)
 (setq ENTSET (ssget))
 (setq C 0)
 (while (< C (sslength ENTSET))
  (setq ENT (ssname ENTSET C))
  (setq ENTLIST (entget ENT))
  (if (= (cdr (assoc 0 ENTLIST)) "POINT")
   (setq PLIST (append PLIST (list (cdr (assoc 10 ENTLIST)))))
   (if (= "LWPOLYLINE" (cdr (assoc 0 ENTLIST)))
    (while (/= nil ENTLIST)
     (if (= 10 (caar ENTLIST))
       (setq PLIST (append PLIST (list (cdar ENTLIST))))
     )
     (setq ENTLIST (cdr ENTLIST))
    )
    (if (= "POLYLINE" (cdr (assoc 0 ENTLIST)))
     (progn
      (setq ENT (entnext ENT))
      (setq ENTLIST (entget ENT))
      (while (= "VERTEX" (cdr (assoc 0 ENTLIST)))
       (setq P (cdr (assoc 10 ENTLIST)))
       (setq PLIST (append PLIST (list (list (car P) (cadr P)))))
       (setq ENT (entnext ENT))
       (setq ENTLIST (entget ENT))
      )
     )
    )
   )
  )
  (setq C (+ C 1))
 )
 (if (/= nil (setq P (RFL:BESTCIRCLE PLIST)))
  (command "._CIRCLE" "NON" (car P) (cadr P))
 )
)
