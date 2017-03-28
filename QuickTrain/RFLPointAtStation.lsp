;
;
;     Program written by Robert Livingston, 2013-05-01
;
;     RFL:POINTATSTATION returns the 3D point at a given station.
;
;
(if RFL:POINTATSTATION (princ "\nRFL:POINTATSTATION already loaded...")
(defun RFL:POINTATSTATION (STA / P S Z)
 (setq P (RFL:XY (list STA 0.0)))
 (if (= nil P)
  nil
  (progn
   (if (or (= nil RFL:PVILIST) (= nil (setq Z (RFL:ELEVATION STA))))
    (setq Z 0.0)
   )
   (if (or (= nil RFL:SUPERLIST) (= nil (setq S (RFL:SUPER STA))))
    (setq S (list 0.0 0.0))
   )
   (list (car P) (cadr P) (+ Z (/ (+ (car S) (cadr S)) 2000.0)))
  )
 )
)
)
