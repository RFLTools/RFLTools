;
;
;    Program Written by Robert Livingston 00/03/07
;    AARC is a utility for attaching an arc to the end of a line or arc
;
;
(defun C:AARC (/ *error* ANG ANG1 ANG2 ANGBASE ANGDIR CMDECHO DELTA DIR DRAWARC DX DY ENT ENTLIST L OSMODE P P1 P2 PC R R2 TMP)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0.0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 0)
 (setq OSMODE (getvar "OSMODE"))
 (setvar "OSMODE" 0)

 (defun *error* (msg)
  (command "._UCS" "P")
  (setvar "CMDECHO" CMDECHO)
  (setvar "OSMODE" OSMODE)
  (setvar "ANGBASE" ANGBASE)
  (setvar "ANGDIR" ANGDIR)
  (princ msg)
  (setq *error* nil)
 )

 (defun DRAWARC (P1 P2 R / ANG D PC X Y)
  (setq D (distance P1 P2)
        X (/ D 2.0)
        Y (* (RFL:SIGN R) (sqrt (- (* R R) (* X X))))
        ANG (angle P1 P2)
        PC (list (+ (car P1) (* X (cos ANG)) (* -1.0 Y (sin ANG)))
                 (+ (cadr P1) (* X (sin ANG)) (* Y (cos ANG)))
           )
  )
  
  (if (> R 0.0)
   (entmake (list (cons 0 "ARC")
                  (cons 10 PC)
                  (cons 40 (abs R))
                  (cons 50 (angle PC P1))
                  (cons 51 (angle PC P2))
            )
   )
   (entmake (list (cons 0 "ARC")
                  (cons 10 PC)
                  (cons 40 (abs R))
                  (cons 50 (angle PC P2))
                  (cons 51 (angle PC P1))
            )
   )
  )
 )
 
 (command "._UCS" "W")

 (setq ENT (entsel))
 (if (/= ENT nil)
  (progn
   (setq P (nth 1 ENT))
   (setq P (list (nth 0 P) (nth 1 P)))
   (setq ENT (car ENT))
   (setq ENTLIST (entget ENT))
   (setq R nil)
   (if (and (setq R (getdist "\nEnter radius :")) (> R 0.0))
    (progn
     (initget "Left Right")
     (if (= (setq DIR (getkword "\nLeft or Right <Left> : ")) "Right") (setq R (* -1.0 R)))
     (setq DELTA 1 L nil)
     (while (or (= DELTA nil) (= L nil))
      (if (= L nil)
       (progn
        (setq DELTA (getangle "\nEnter DELTA (<return> for L) :"))
        (if (= DELTA nil)
         (progn
          (setq DELTA nil)
          (setq L 1)
         )
         (progn
          (setq DELTA (abs DELTA))
          (setq L (abs (* R DELTA)))
         )
        )
       )
       (progn
        (setq L (getreal "\nEnter L (<return> for DELTA) :"))
        (if (= L nil)
         (progn
          (setq DELTA 1)
          (setq L nil)
         )
         (progn
          (setq L (abs L))
          (setq DELTA (abs (/ L R)))
         )
        )
       )
      )
     )
    )
    (progn
     (princ "\nR must be greater than 0.0!\n")
     (setq R nil)
    )
   )
   (if (/= R nil)
    (if (= (cdr (assoc 0 ENTLIST)) "LINE")
     (progn
      (setq P1 (cdr (assoc 10 ENTLIST)))
      (setq P1 (list (nth 0 P1) (nth 1 P1)))
      (setq P2 (cdr (assoc 11 ENTLIST)))
      (setq P2 (list (nth 0 P2) (nth 1 P2)))
      (if (< (distance P P2) (distance P P1))
       (progn
        (setq TMP P1)
        (setq P1 P2)
        (setq P2 TMP)
       )
      )
      (setq ANG (angle P2 P1))
      (setq DX (* (abs R) (sin DELTA)))
      (setq DY (* R (- 1.0 (cos DELTA))))
      (setq P (list (+ (nth 0 P1) (- (* DX (cos ANG)) (* DY (sin ANG))))
                    (+ (nth 1 P1) (+ (* DX (sin ANG)) (* DY (cos ANG))))
              )
      )
      (DRAWARC P1 P R)
     )
     (progn
      (if (= (cdr (assoc 0 ENTLIST)) "ARC")
       (progn
        (setq PC (cdr (assoc 10 ENTLIST)))
        (setq PC (list (nth 0 PC) (nth 1 PC)))
        (setq R2 (cdr (assoc 40 ENTLIST)))
        (setq ANG1 (cdr (assoc 50 ENTLIST)))
        (setq ANG2 (cdr (assoc 51 ENTLIST)))
        (setq P1 (list (+ (nth 0 PC) (* R2 (cos ANG1)))
                       (+ (nth 1 PC) (* R2 (sin ANG1)))))
        (setq ANG1 (- ANG1 (/ pi 2.0)))
        (setq P2 (list (+ (nth 0 PC) (* R2 (cos ANG2)))
                       (+ (nth 1 PC) (* R2 (sin ANG2)))))
        (setq ANG2 (+ ANG2 (/ pi 2.0)))
        (setq ANG ANG1)
        (if (< (distance P P2) (distance P P1))
         (progn
          (setq TMP P1)
          (setq P1 P2)
          (setq P2 TMP)
          (setq ANG ANG2)
         )
        )
        (setq DX (* (abs R) (sin DELTA)))
        (setq DY (* R (- 1.0 (cos DELTA))))
        (setq P (list (+ (nth 0 P1) (- (* DX (cos ANG)) (* DY (sin ANG))))
                      (+ (nth 1 P1) (+ (* DX (sin ANG)) (* DY (cos ANG))))
                )
        )
        (DRAWARC P1 P R)
       )
       (progn
        (if (/= (setq ENTLIST (RFL:GETSPIRALDATA ENT)) nil)
         (progn
          (setq TMP (nth 0 (RFL:SPIRALSTAOFF P ENT)))
          (if (< (- TMP (nth 3 ENTLIST)) (- (RFL:GETSPIRALLS ENT) TMP))
           (progn
            (setq P1 (RFL:SPIRALXY (list (nth 3 ENTLIST) 0.0) ENT))
            (setq ANG (angle (nth 1 ENTLIST) (nth 0 ENTLIST)))
            (if (> (nth 3 ENTLIST) 0.0)
             (progn
              (if (> (sin (- (angle (nth 1 ENTLIST) (nth 0 ENTLIST)) (angle (nth 2 ENTLIST) (nth 1 ENTLIST)))) 0.0)
               (setq TMP -1.0)
               (setq TMP 1.0)
              )
              (setq ANG (+ ANG
                           (* TMP
                              (expt (nth 3 ENTLIST) 2)
                              (RFL:GETSPIRALTHETA ENT)
                              (/ 1.0 (expt (RFL:GETSPIRALLS ENT) 2))
                           )
                        )
              )
             )
            )
           )
           (progn
            (setq P1 (nth 2 ENTLIST))
            (setq ANG (angle (nth 1 ENTLIST) (nth 2 ENTLIST)))
           )
          )
          (setq DX (* (abs R) (sin DELTA)))
          (setq DY (* R (- 1.0 (cos DELTA))))
          (setq P (list (+ (nth 0 P1) (- (* DX (cos ANG)) (* DY (sin ANG))))
                        (+ (nth 1 P1) (+ (* DX (sin ANG)) (* DY (cos ANG))))
                  )
          )
          (DRAWARC P1 P R)
         )
         (progn
          (princ "\n*** ENTITY NOT SPIRAL/ARC/LINE ***")
         )
        )
       )
      )
     )
    )
   )
  )
 )

 (command "._UCS" "P")
 (setvar "CMDECHO" CMDECHO)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
 (setvar "OSMODE" OSMODE)
 P2
)