;
;
;   Program written by Robert Livingston, 98/06/12
;
;   RFL:DRAWALIGNOS2 draws the current alignmnet at the specified offset
;
;
;
(defun RFL:DRAWALIGNOS2 (OS / ANG ANG1 ANG2 ALLIST ALENT ENT ENTLIST OS2 P1X P1Y P2X P2Y PC PREVENT R)
 (setq PREVENT nil)
 (setq ALLIST RFL:ALIGNLIST)
 (entmake)
 (while (/= ALLIST nil)
  (setq ALENT (car ALLIST))
  (setq ALLIST (cdr ALLIST))
  (if (listp (last ALENT))
   (progn
    (if (< (distance (nth 2 ALENT) (nth 2 (last ALENT))) (distance (nth 1 ALENT) (nth 2 (last ALENT))))
     (setq OS2 OS)
     (setq OS2 (* -1.0 OS))
    )
    (RFL:DRAWSPIRAL (nth 0 (last ALENT)) (nth 1 (last ALENT)) (nth 2 (last ALENT)) (nth 3 (last ALENT)) OS2)
    (setq ENT (entlast))
    (RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
   )
   (progn
    (if (> (abs (last ALENT)) RFL:TOLFINE)
     (progn
      (setq PC (RFL:CENTER (nth 1 ALENT) (nth 2 ALENT) (nth 3 ALENT)))
      (setq R (RFL:RADIUS (nth 1 ALENT) (nth 2 ALENT) (nth 3 ALENT)))
      (if (> (last ALENT) 0.0)
       (progn
        (setq OS2 OS)
        (setq ANG1 (angle PC (nth 1 ALENT)))
        (setq ANG2 (angle PC (nth 2 ALENT)))
       )
       (progn
        (setq OS2 (* -1.0 OS))
        (setq ANG2 (angle PC (nth 1 ALENT)))
        (setq ANG1 (angle PC (nth 2 ALENT)))
       )
      )
      (setq ENTLIST (list (cons 0 "ARC")
                          (list 10 (nth 0 PC) (nth 1 PC) 0.0)
                          (cons 40 (+ R OS2))
                          (cons 50 ANG1)
                          (cons 51 ANG2)
                    )
      )
      (entmake ENTLIST)
      (setq ENT (entlast))
      (RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
     )
     (progn
      (setq ANG (angle (nth 1 ALENT) (nth 2 ALENT)))
      (setq P1X (+ (nth 0 (nth 1 ALENT)) (* OS (sin ANG))))
      (setq P1Y (- (nth 1 (nth 1 ALENT)) (* OS (cos ANG))))
      (setq P2X (+ (nth 0 (nth 2 ALENT)) (* OS (sin ANG))))
      (setq P2Y (- (nth 1 (nth 2 ALENT)) (* OS (cos ANG))))
      (setq ENTLIST (list (cons 0 "LINE")
                          (list 10 P1X P1Y 0.0)
                          (list 11 P2X P2Y 0.0)
                    )
      )
      (entmake ENTLIST)
      (setq ENT (entlast))
      (RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
     )
    )
   )
  )
 )
)
