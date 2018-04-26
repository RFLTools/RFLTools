;
;
;     Program written by Robert Livingston, 2015-03-13
;
;     RFL:GETPLIST returns a list of points along a polyline
;
;
(defun RFL:GETPLIST (ENT / ENTLIST P PLIST ZFLAG)
 (setq PLIST nil)
 (setq ENTLIST (entget ENT))
 (if (= "POLYLINE" (cdr (assoc 0 ENTLIST)))
  (progn
   (if (/ (cdr (assoc 70 ENTLIST)) 8) (setq ZFLAG T))
   (setq ENT (entnext ENT))
   (setq ENTLIST (entget ENT))
   (while (= "VERTEX" (cdr (assoc 0 ENTLIST)))
    (setq P (cdr (assoc 10 ENTLIST)))
    (if ZFLAG
     (setq P (list (car P) (cadr P) (caddr P)))
     (setq P (list (car P) (cadr P)))
    )
    (setq PLIST (append PLIST (list P)))
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
   )
  )
 )
 (if (= "LWPOLYLINE" (cdr (assoc 0 ENTLIST)))
  (progn
   (while (/= nil ENTLIST)
    (setq P (car ENTLIST))
    (setq ENTLIST (cdr ENTLIST))
    (if (= 10 (car P))
     (progn
      (setq P (list (cadr P) (caddr P)))
      (setq PLIST (append PLIST (list P)))
     )
    )
   )
  )
 )
 PLIST
)
