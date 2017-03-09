;
;
;   Program written by Robert Livingston, 99/07/14
;
;   C:POFFSET offsets an arc a calculated spiral 'P' distance
;   Modified 02/04/18 - presets the offset distance and starts the offset command
;                     - works for compound spirals
;
;
(defun C:POFFSET (/ A AL CMDECHO ENT ENTLIST LS1 LS2 P PCEN1 PCEN2 R1 R2 THETA1 THETA2)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (setq R1 nil)
 (if (/= (setq ENT (car (entsel "\nSelect first arc or circle (<return> to enter radius) :"))) nil)
  (progn
   (setq ENTLIST (entget ENT))
   (if (or (= (cdr (assoc 0 ENTLIST)) "ARC")
           (= (cdr (assoc 0 ENTLIST)) "CIRCLE"))
    (progn
     (setq R1 (cdr (assoc 40 ENTLIST)))
    )
   )
  )
 )
 (if (= R1 nil)
  (progn
   (setq R1 (getdist "\nEnter radius : "))
  )
 )
 (setq R2 nil)
 (if (/= (setq ENT (car (entsel "\nSelect second arc or circle (<return> to enter radius) :"))) nil)
  (progn
   (setq ENTLIST (entget ENT))
   (if (or (= (cdr (assoc 0 ENTLIST)) "ARC")
           (= (cdr (assoc 0 ENTLIST)) "CIRCLE"))
    (progn
     (setq R2 (cdr (assoc 40 ENTLIST)))
    )
   )
  )
 )
 (if (= R2 nil)
  (progn
   (setq R2 (getdist "\nEnter radius (<return> for simple spiral) : "))
  )
 )
 (if (= R2 nil)
  (progn
   (setq LS nil)
   (setq AL "L")
   (while (= LS nil)
    (if (= AL "L")
     (progn
      (setq LS (getreal "\nEnter spiral length <return for A>:"))
      (if (= LS nil)
       (progn
        (setq AL "A")
       )
      )
     )
     (progn
      (setq LS (getreal "\nEnter spiral A <return for length>:"))
      (if (= LS nil)
       (progn
        (setq AL "L")
       )
       (progn
        (setq LS (/ (* LS LS) R1))
       )
      )
     )
    )
   )
   (setq THETA1 (/ LS R1 2.0))
   (setq P (* R1 (RFL:SPIRALPR THETA1)))
   (command "_OFFSET" P)
  )
  (progn
   (setq LS nil)
   (setq AL "L")
   (setq A nil)
   (while (= LS nil)
    (if (= AL "L")
     (progn
      (setq LS (getreal "\nEnter spiral length <return for A>:"))
      (if (= LS nil)
       (progn
        (setq AL "A")
       )
       (progn
        (setq A (sqrt (abs (/ (* LS R1 R2) (- R2 R1)))))
       )
      )
     )
     (progn
      (setq LS (getreal "\nEnter spiral A <return for length>:"))
      (if (= LS nil)
       (progn
        (setq AL "L")
       )
       (progn
        (setq A LS)
        (setq LS (abs (* (* LS LS) (/ (- R2 R1) (* R1 R2)))))
       )
      )
     )
    )
   )
   (setq THETA1 (/ (* A A) (* 2 R1 R1)))
   (setq PCEN1 (list (* R1 (- (RFL:SPIRALFXR THETA1) (sin THETA1)))
                     (* R1 (+ (RFL:SPIRALFYR THETA1) (cos THETA1)))))
   (setq THETA2 (/ (* A A) (* 2 R2 R2)))
   (setq PCEN2 (list (* R2 (- (RFL:SPIRALFXR THETA2) (sin THETA2)))
                     (* R2 (+ (RFL:SPIRALFYR THETA2) (cos THETA2)))))
   (setq P (- (abs (- R1 R2)) (distance PCEN1 PCEN2)))
   (command "_OFFSET" P)
  )
 )

 (setvar "CMDECHO" CMDECHO)
) 