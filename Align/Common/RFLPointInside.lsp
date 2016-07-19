;
;
;     Program written by Robert Livingston, 2015-03-13
;
;     RFL:POINTINSIDE checks if a point is inside a polyline formed by PLIST
;
;
(defun RFL:POINTINSIDE (P PLIST / CROSSINGCOUNT P0 P1 PBASE PTMP)
 (setq P0 (last PLIST))
 ;  Subtracted/added pi from to the 'X' and 'Y' coordinate to have a point that is outside PLIST and 'hopefully' prevent on edge case
 (setq PBASE (list (- (apply 'min (mapcar '(lambda (PTMP) (car PTMP)) PLIST)) pi)
                   (+ (apply 'min (mapcar '(lambda (PTMP) (cadr PTMP)) PLIST)) pi)
             )
 )
 (setq CROSSINGCOUNT 0)
 (foreach P1 PLIST
  (progn
   (if (inters PBASE P P0 P1)
    (setq CROSSINGCOUNT (1+ CROSSINGCOUNT))
   )
   (setq P0 P1)
  )
 )
 (if (= 0 (rem CROSSINGCOUNT 2))
  nil
  T
 )
)
