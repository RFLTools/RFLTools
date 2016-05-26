;
;
;    Program Written by Robert Livingston, 99/07/14
;    C:DSPIRAL draws a reverse engineered DCA spiral at the end of a selected line
;
;
(defun C:DSPIRAL (/ ANG CMDECHO DIR ENT ENTLIST FX FY LR P P1 P2 PLT PLTST PST THETA)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (setq ENT (entsel "\nSelect line : "))
 (if (/= ENT nil) 
  (progn
   (setq P (car (cdr ENT)))
   (setq P (list (car P) (cadr P)))
   (setq ENT (car ENT))
   (if (= (cdr (assoc 0 (setq ENTLIST (entget ENT)))) "LINE")
    (progn
     (setq P1 (cdr (assoc 10 ENTLIST)))
     (setq P1 (list (car P1) (cadr P1)))
     (setq P2 (cdr (assoc 11 ENTLIST)))
     (setq P2 (list (car P2) (cadr P2)))
     (if (< (distance P P1) (distance P P2))
      (progn
       (setq TMP P1)
       (setq P1 P2)
       (setq P2 TMP)
      )
     )
     (setq ANG (angle P1 P2))
     (if (/= (setq R (getreal "Radius : ")) nil)
      (if (/= (setq L (getreal "Length : ")) nil)
       (progn
        (initget 1 "Left Right")
        (setq LR (getkword "\n Left or Right : "))
        (if (= LR "Left")
         (setq DIR 1.0)
         (setq DIR -1.0)
        )
        (setq THETA (/ L (* 2.0 R)))
        (setq FX (* R (RFL:SPIRALFXR THETA)))
        (setq FY (* R (RFL:SPIRALFYR THETA)))
        (setq PLT P2)
        (setq PST (list (+ (car PLT) (* FX (cos ANG)) (* DIR -1.0 FY (sin ANG)))
                        (+ (cadr PLT) (* FX (sin ANG)) (* DIR FY (cos ANG)))))
        (setq PLTST (list (+ (car PLT) (* (- FX (/ FY (RFL:TAN THETA))) (cos ANG)))
                          (+ (cadr PLT) (* (- FX (/ FY (RFL:TAN THETA))) (sin ANG))))) 
        (RFL:DRAWSPIRAL PLT PLTST PST 0.0 0.0)
       )
      )
     )
    )
   )
  )
 )
 (setvar "CMDECHO" CMDECHO)
)
