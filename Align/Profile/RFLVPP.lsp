;
;
;   Program written by Robert Livingston, 99/12/03
;
;   (RFL:VPP pnt) returns the Station and Elevation of supplied point.
;
(defun RFL:VPP (P1 / STA Z)
 (if (= RFL:PROFDEFLIST nil) (RFL:PROFDEF))
 (setq STA (+ (* (cdr (assoc "DIRECTION" RFL:PROFDEFLIST))
                 (- (nth 0 P1)
                    (nth 0 (cdr (assoc "BPOINT" RFL:PROFDEFLIST)))
                 )
              )
              (cdr (assoc "STA" RFL:PROFDEFLIST))
           )
 )
 (setq Z (+ (/ (- (nth 1 P1)
                  (nth 1 (cdr (assoc "BPOINT" RFL:PROFDEFLIST)))
               )
               (cdr (assoc "VEXAG" RFL:PROFDEFLIST))
            )
            (cdr (assoc "ELEV" RFL:PROFDEFLIST))
         )
 )
 (list STA Z)
)
