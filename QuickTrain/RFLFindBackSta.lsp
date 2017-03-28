;
;
;     Program written by Robert Livingston, 2013-05-01
;
;     RFL:FINDBACKSTA calculates the back station of a vehicle with a wheelbase of WB
;
;
(if RFL:FINDBACKSTA (princ "\nRFL:FINDBACKSTA already loaded...")
(defun RFL:FINDBACKSTA (STA WB / *error* P P2 PM STA1 STA2 STAM STAS)
 (defun *error* (msg)
  nil
 )
 (if (= nil (setq P (RFL:POINTATSTATION STA)))
  nil
  (progn
   (setq STAS (caar RFL:ALIGNLIST))
   (setq STA2 (- STA (* 1.5 WB)))
   (if (< STA2 STAS) (setq STA2 STAS))
   (setq P2 (RFL:POINTATSTATION STA2))
   (if (or (= nil P2) (< (distance P P2) WB))
    nil
    (progn
     (setq STA1 (- STA (* 0.5 WB)))
     (setq P1 (RFL:POINTATSTATION STA1))
     (setq STAM (/ (+ STA1 STA2) 2.0))
     (setq PM (RFL:POINTATSTATION STAM))
     (while (> (abs (- WB (distance P PM))) RFL:TOL)
      (if (< (distance P PM) WB)
       (setq P1 PM STA1 STAM)
       (setq P2 PM STA2 STAM)
      )
      (setq STAM (/ (+ STA1 STA2) 2.0))
      (setq PM (RFL:POINTATSTATION STAM))
     )
     STAM
    )
   )
  )
 )
)
)
