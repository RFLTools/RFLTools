;
;
;   Program written by Robert Livingston, 98/05/14
;
;   RFL:PROFPOINT returns the point at a specified station and elevation for the curretnly defined profile grid RFL:PROFDEFLIST
;
;
(defun RFL:PROFPOINT (STA ELEV / D X Y)
 (if (/= nil RFL:PROFDEFLIST)
  (progn
   (if (= (assoc "DIRECTION" RFL:PROFDEFLIST) nil)
    (setq D 1)
    (setq D (cdr (assoc "DIRECTION" RFL:PROFDEFLIST)))
   )
   (setq X (+ (* (- STA
                    (cdr (assoc "STA" RFL:PROFDEFLIST))
                 )
                 D
              )
              (car (cdr (assoc "BPOINT" RFL:PROFDEFLIST)))
           )
   )
   (setq Y (+ (* (- ELEV
                    (cdr (assoc "ELEV" RFL:PROFDEFLIST))
                 )
                 (cdr (assoc "VEXAG" RFL:PROFDEFLIST))
              )
              (cadr (cdr (assoc "BPOINT" RFL:PROFDEFLIST)))
           )
   )
   (list X Y 0.0)
  )
  (progn
   (princ "\n*** PROFILE NOT SET - RUN GPROF OR RPROF ***\n")
   nil
  )
 )
)
