;
;
;   Program written by Robert Livingston, 98/05/14
;
;   RFL:SLOPE returns the slope at a specified station for the curretnly defined profile (PVILIST)
;
;
(defun RFL:SLOPE (STA / C CMDECHO ELEV1 ELEV2 ELEV3 G G1 G2 L NODE P)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (if (/= nil PVILIST)
  (progn
   (if (or (< STA (caar PVILIST)) (> STA (car (last PVILIST))))
    (progn
     (princ "\n*** STATION OUT OF RANGE ***\n")
     (if (< STA (caar PVILIST))
      (setq G (/ (- (cadadr PVILIST) (cadar PVILIST)) (- (caadr PVILIST) (caar PVILIST))))
      (setq G (/ (- (cadadr (reverse PVILIST)) (cadar (reverse PVILIST))) (- (caadr (reverse PVILIST)) (caar (reverse PVILIST)))))
     )
    )
    (progn
     (setq C 0)
     (while (> STA (+ (car (setq NODE (nth C PVILIST)))
                      (/ (cadddr NODE) 2.0)
                   )
            )
      (setq C (1+ C))
     )
     (if (= "L" (caddr (nth C PVILIST)))
      (progn
       (setq NODE (nth (1- C) PVILIST))
       (setq STA1 (car NODE))
       (setq ELEV1 (cadr NODE))
       (setq NODE (nth C PVILIST))
       (setq STA2 (car NODE))
       (setq ELEV2 (cadr NODE))
       (setq L (cadddr NODE))
       (setq G1 (/ (- ELEV2 ELEV1) (- STA2 STA1)))
       (setq G G1)
       (setq D (- STA (- STA2 (/ L 2.0))))
       (if (> D 0.0)
        (progn
         (setq NODE (nth (1+ C) PVILIST))
         (setq STA3 (car NODE))
         (setq ELEV3 (cadr NODE))
         (setq G2 (/ (- ELEV3 ELEV2) (- STA3 STA2)))
         (setq G (+ G1 (* (/ D L) (- G2 G1))))
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
   (setq G nil)
  )
 )

 (setvar "CMDECHO" CMDECHO)
 G
)
