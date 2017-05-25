;
;
;     Program written by Robert Livingston - 2015-11-06
;
;     C:GPROF3DP extracts the vertex elevations and sets the RFL profile based on the current alignment
;
;
(defun C:GPROF3DP (/ ENT ENTLIST NODE P PLIST)
 (if (and (/= nil RFL:ALIGNLIST)
          (/= nil (setq PLIST (RFL:GETPLIST (car (entsel)))))
          (/= nil (caddar PLIST))
     )
  (progn
   (while (and (/= nil PLIST) (= nil (RFL:STAOFF (car PLIST))))
    (setq PLIST (cdr PLIST))
   )
   (setq PLIST (reverse PLIST))
   (while (and (/= nil PLIST) (= nil (RFL:STAOFF (car PLIST))))
    (setq PLIST (cdr PLIST))
   )
   (if (and (/= nil PLIST) (> (length PLIST) 1))
    (progn
     (if (< (car (RFL:STAOFF (last PLIST))) (car (RFL:STAOFF (car PLIST))))
      (setq PLIST (reverse PLIST))
     )
     (setq RFL:PVILIST nil)
     (foreach P PLIST
      (setq RFL:PVILIST (append RFL:PVILIST (list (list (car (RFL:STAOFF P)) (caddr P) "L" 0.0))))
     )
    )
   )
  )
 )
 nil
)