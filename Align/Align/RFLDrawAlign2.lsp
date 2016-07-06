;
;
;   Program written by Robert Livingston, 98/06/12
;
;   RFL:DRAWALIGN2 draws the current alignmnet
;
;
;
(defun RFL:DRAWALIGN2 (/ ANG1 ANG2 ALLIST ALENT ENT ENTLIST PC PREVENT R)
 (setq ALLIST ALIGNLIST)
 (entmake)
 (while (/= ALLIST nil)
  (setq ALENT (car ALLIST))
  (setq ALLIST (cdr ALLIST))
  (if (listp (last ALENT))
   (progn
    (RFL:DRAWSPIRAL (nth 0 (last ALENT)) (nth 1 (last ALENT)) (nth 2 (last ALENT)) (nth 3 (last ALENT)) 0.0)
    (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
   )
   (progn
    (if (> (abs (last ALENT)) RFL:TOLFINE)
     (progn
      (setq PC (RFL:CENTER (nth 1 ALENT) (nth 2 ALENT) (nth 3 ALENT)))
      (setq R (RFL:RADIUS (nth 1 ALENT) (nth 2 ALENT) (nth 3 ALENT)))
      (if (< (last ALENT) 0.0)
       (progn
        (setq ANG2 (angle PC (nth 1 ALENT)))
        (setq ANG1 (angle PC (nth 2 ALENT)))
       )
       (progn
        (setq ANG1 (angle PC (nth 1 ALENT)))
        (setq ANG2 (angle PC (nth 2 ALENT)))
       )
      )
      (setq ENTLIST (list (cons 0 "ARC")
                          (list 10 (nth 0 PC) (nth 1 PC) 0.0)
                          (cons 40 R)
                          (cons 50 ANG1)
                          (cons 51 ANG2)
                    )
      )
      (entmake ENTLIST)
      (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
     )
     (progn
      (setq ENTLIST (list (cons 0 "LINE")
                          (list 10 (nth 0 (nth 1 ALENT)) (nth 1 (nth 1 ALENT)) 0.0)
                          (list 11 (nth 0 (nth 2 ALENT)) (nth 1 (nth 2 ALENT)) 0.0)
                    )
      )
      (entmake ENTLIST)
      (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
     )
    )
   )
  )
 )
)
