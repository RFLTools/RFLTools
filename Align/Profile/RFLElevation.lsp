;
;
;   Program written by Robert Livingston, 98/05/14
;
;   RFL:ELEVATION returns the elevation at a specified station for the curretnly defined profile (RFL:PVILIST)
;
;
(if RFL:ELEVATION (princ "\nRFL:ELEVATION already loaded...")
(defun RFL:ELEVATION (STA / C CMDECHO ELEV ELEV1 ELEV2 ELEV3 G1 G2 L NODE P STA1 STA2 STA3)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (if (/= nil RFL:PVILIST)
  (progn
   (if (or (< STA (caar RFL:PVILIST)) (> STA (car (last RFL:PVILIST))))
    (progn
     (princ "\n*** STATION OUT OF RANGE ***\n")
     (setq ELEV nil)
    )
    (progn
     (setq C 1)
     (while (> STA (+ (car (setq NODE (nth C RFL:PVILIST)))
                      (/ (if (= nil (cadddr NODE)) 0.0 (cadddr NODE)) 2.0)
                   )
            )
      (setq C (1+ C))
     )
     (if (or (= "L" (caddr (nth C RFL:PVILIST))) (= nil (caddr (nth C RFL:PVILIST))))
      (progn
       (setq NODE (nth (1- C) RFL:PVILIST))
       (setq STA1 (car NODE))
       (setq ELEV1 (cadr NODE))
       (setq NODE (nth C RFL:PVILIST))
       (setq STA2 (car NODE))
       (setq ELEV2 (cadr NODE))
       (setq L (if (= nil (cadddr NODE)) 0.0 (cadddr NODE)))
       (setq G1 (/ (- ELEV2 ELEV1) (- STA2 STA1)))
       (setq ELEV (+ ELEV1 (* G1 (- STA STA1))))
       (setq D (- STA (- STA2 (/ L 2.0))))
       (if (> D 0.0)
        (progn
         (setq NODE (nth (1+ C) RFL:PVILIST))
         (setq STA3 (car NODE))
         (setq ELEV3 (cadr NODE))
         (setq G2 (/ (- ELEV3 ELEV2) (- STA3 STA2)))
         (setq ELEV (+ ELEV (/ (* D D (- G2 G1)) (* L 2.0))))
        )
       )        
      )
      (progn
       (princ "\n*** ONLY PARABILIC VERTICAL CURVES SUPPORTED ***\n")
       (setq ELEV nil)
      )
     )
    )
   )
  )
  (progn
   (princ "\n*** PROFILE NOT SET - RUN GPROF OR RPROF ***\n")
   (setq ELEV nil)
  )
 )

 (setvar "CMDECHO" CMDECHO)
 (eval ELEV)
)
)
