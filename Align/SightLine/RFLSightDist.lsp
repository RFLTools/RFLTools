;
;
;     Program written by Robert Livingston 2013-02-12
;
;     RFL:SIGHTDIST returns a station that is DIST away from STA
;
;
(if RFL:SIGHTDIST (princ "\nRFL:SIGHTDIST already loaded...")
(defun RFL:SIGHTDIST (STA DIST / P P1 P2 PM POUT STA1 STA2 STAM)
 (if (= RFL:ALIGNLIST nil)
  nil
  (if (or (< STA (caar RFL:ALIGNLIST)) (> STA (+ (caar RFL:ALIGNLIST) (RFL:GETALIGNLENGTH))))
   nil
   (if (> DIST 0.0)
    (if (setq P (RFL:XY (list STA 0.0)))
     (progn
      (setq STA1 STA)
      (setq P1 P)
      (setq STA2 (+ (caar RFL:ALIGNLIST) (RFL:GETALIGNLENGTH)))
      (setq P2 (caddr (last RFL:ALIGNLIST)))
      (if (> (distance P P2) DIST)
       (progn
        (while (> (abs (- STA2 STA1)) RFL:TOL)
         (setq STAM (/ (+ STA1 STA2) 2.0))
         (setq PM (RFL:XY (list STAM 0.0)))
         (if (> (distance P PM) DIST)
          (progn
           (setq P2 PM)
           (setq STA2 STAM)
          )
          (progn
           (setq P1 PM)
           (setq STA1 STAM)
          )
         )
        )
        STAM
       )
       nil
      )
     )
     nil
    )
    (if (setq P (RFL:XY (list STA 0.0)))
     (progn
      (setq DIST (* DIST -1.0))
      (setq STA1 STA)
      (setq P1 P)
      (setq STA2 (caar RFL:ALIGNLIST))
      (setq P2 (cadar RFL:ALIGNLIST))
      (if (> (distance P P2) DIST)
       (progn
        (while (> (abs (- STA2 STA1)) RFL:TOL)
         (setq STAM (/ (+ STA1 STA2) 2.0))
         (setq PM (RFL:XY (list STAM 0.0)))
         (if (> (distance P PM) DIST)
          (progn
           (setq P2 PM)
           (setq STA2 STAM)
          )
          (progn
           (setq P1 PM)
           (setq STA1 STAM)
          )
         )
        )
        STAM
       )
       nil
      )
     )
     nil
    )
   )
  )
 )
)
)