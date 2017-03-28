;
;
;     Program written by Robert Livingston, 2013-05-01
;
;     RFL:GETTRUCKPOUT calculates the 3D point of the truck 
;
;
(if RFL:GETTRUCKPOUT (princ "\nRFL:GETTRUCKPOUT already loaded...")
(defun RFL:GETTRUCKPOUT (STA WB GAUGE DX DY DZ / A P R RX RY RZ STA2)
 (if (= nil (setq P (RFL:POINTATSTATION STA)))
  nil
  (if (= nil (setq STA2 (RFL:FINDFRONTSTA STA WB)))
   nil
   (if (= nil (setq A (RFL:GETTRUCKANGS STA STA2 GAUGE)))
    nil
    (progn
     (setq RX (car A))
     (setq RY (cadr A))
     (setq RZ (caddr A))
     (setq A (+ RX (angle (list 0.0 0.0) (list DY DZ))))
     (setq R (sqrt (+ (expt DY 2) (expt DZ 2))))
     (setq DY (* R (cos A)))
     (setq DZ (* R (sin A)))
     (setq A (+ RY (angle (list 0.0 0.0) (list DZ DX))))
     (setq R (sqrt (+ (expt DZ 2) (expt DX 2))))
     (setq DZ (* R (cos A)))
     (setq DX (* R (sin A)))
     (setq A (+ RZ (angle (list 0.0 0.0) (list DX DY))))
     (setq R (sqrt (+ (expt DX 2) (expt DY 2))))
     (setq DX (* R (cos A)))
     (setq DY (* R (sin A)))
     (list (+ (car P) DX) (+ (cadr P) DY) (+ (caddr P) DZ))
    )
   )
  )
 )
)
)
