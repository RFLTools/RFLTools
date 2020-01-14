;
;
;     Program written by Robert Livingston, 2020-01-13
;
;     RFL:GETBLKPLIST returns a list of points contained in a block
;
;
(defun RFL:GETBLKPLIST (ENT / ANG BLKENT BLKP ENTLIST P PLIST SCALE)
 (setq PLIST nil)
 (setq ENTLIST (entget ENT))
 (if (= "INSERT" (cdr (assoc 0 ENTLIST)))
  (progn
   (setq P (cdr (assoc 10 ENTLIST)))
   (setq ANG (cdr (assoc 50 ENTLIST)))
   (setq SCALE (cdr (assoc 41 ENTLIST)))
   (if (and (= SCALE (cdr (assoc 42 ENTLIST)))
            (= SCALE (cdr (assoc 43 ENTLIST)))
       )
    (if (setq BLKENT (tblsearch "BLOCK" (cdr (assoc 2 ENTLIST))))
     (progn
      (setq BLKENT (cdr (assoc -2 BLKENT)))
      (while BLKENT
       (setq ENTLIST (entget BLKENT))
       (if (= "POINT" (cdr (assoc 0 ENTLIST)))
        (progn
         (setq BLKP (cdr (assoc 10 ENTLIST)))
         (setq PLIST (append PLIST (list (list (+ (car P) (* SCALE (- (* (car BLKP) (cos ANG)) (* (cadr BLKP) (sin ANG)))))
                                               (+ (cadr P) (* SCALE (+ (* (car BLKP) (sin ANG)) (* (cadr BLKP) (cos ANG)))))
                                         )
                                   )
                     )
         )
        )
       )
       (setq BLKENT (entnext BLKENT))
      )
     )
     (princ "\nBlock not found.")
    )
    (princ "\nScales not equal.")
   )
  )
  (princ "\nNot a block.")
 )
 PLIST
)