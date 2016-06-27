;
;
;   Program written by Robert Livingston, 98/05/14
;
;   RFL:PROFPOINT returns the point at a specified station and elevation for the curretnly defined profile grid (PROFDEF)
;
;
(defun RFL:PROFPOINT (STA ELEV / D X Y)
 (if (/= nil PROFDEF)
  (progn
   (if (= (assoc "DIRECTION" PROFDEF) nil)
    (setq D 1)
    (setq D (cdr (assoc "DIRECTION" PROFDEF)))
   )
   (setq X (+ (* (- STA
                    (+ (* (cdr (assoc "STAH" PROFDEF)) 1000.0)
                       (cdr (assoc "STAL" PROFDEF))
                    )
                 )
                 D
              )
              (car (cdr (assoc "BPOINT" PROFDEF)))
           )
   )
   (setq Y (+ (* (- ELEV (cdr (assoc "ELEV" PROFDEF)))
                 (cdr (assoc "VEXAG" PROFDEF))
              )
              (cadr (cdr (assoc "BPOINT" PROFDEF)))
           )
   )
   (list X Y 0.0)
  )
  (progn
   (princ "\n*** PROFILE NOT SET - RUN GPROF OR RPROF ***\n")
   (eval nil)
  )
 )
)
