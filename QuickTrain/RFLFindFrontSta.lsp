;
;
;     Program written by Robert Livingston, 2013-05-01
;
;     RFL:FINDFRONTSTA calculates the forward station of a vehicle with a wheelbase of WB
;
;
(if RFL:FINDFRONTSTA (princ "\nRFL:FINDFRONTSTA already loaded...")
(defun RFL:FINDFRONTSTA (STA WB / *error* P P2 PM STA1 STA2 STAM STAE)
 (defun *error* (msg)
  nil
 )
 (if (= nil (setq P (RFL:POINTATSTATION STA)))
  nil
  (progn
   (setq STAE (+ (caar RFL:ALIGNLIST) (RFL:GETALIGNLENGTH)))
   (setq STA2 (+ STA (* 1.5 WB)))
   (if (> STA2 STAE) (setq STA2 STAE))
   (setq P2 (RFL:POINTATSTATION STA2))
   (if (or (= nil P2) (< (distance P P2) WB))
    nil
    (progn
     (setq STA1 (+ STA (* 0.5 WB)))
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
