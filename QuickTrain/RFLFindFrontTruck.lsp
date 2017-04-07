;
;
;     Program written by Robert Livingston, 2013-05-01
;
;     RFL:FINDFRONTTRUCK calculates the location of the forward truck
;
;
(if RFL:FINDFRONTTRUCK (princ "\nRFL:FINDFRONTTRUCK already loaded...")
(defun RFL:FINDFRONTTRUCK (P WB WBT DX DZ GAUGE / *error* P1 P2 PM RX RY RZ STA STA1 STA2 STAM STAE)
 (defun *error* (msg)
  nil
 )
 (if (= nil (setq STA (car (RFL:STAOFF P))))
  nil
  (progn
   (setq STAE (+ (caar RFL:ALIGNLIST) (RFL:GETALIGNLENGTH)))
   (setq STA2 (+ STA (* 1.5 (max WB WBT))))
   (if (> STA2 STAE) (setq STA2 STAE))
   (setq P2 (RFL:GETTRUCKPOUT STA2 WBT GAUGE DX DY DZ))
   (if (or (= nil P2) (< (distance P P2) WB))
    nil
    (progn
     (setq STA1 (+ STA (* 0.5 WB)))
     (setq P1 (RFL:GETTRUCKPOUT STA1 WBT GAUGE DX DY DZ))
     (setq STAM (/ (+ STA1 STA2) 2.0))
     (setq PM (RFL:GETTRUCKPOUT STAM WBT GAUGE DX DY DZ))
     (while (> (abs (- WB (distance P PM))) RFL:TOL)
      (if (< (distance P PM) WB)
       (setq P1 PM STA1 STAM)
       (setq P2 PM STA2 STAM)
      )
      (setq STAM (/ (+ STA1 STA2) 2.0))
      (setq PM (RFL:GETTRUCKPOUT STAM WBT GAUGE DX DY DZ))
     )
     STAM
    )
   )
  )
 )
)
)
