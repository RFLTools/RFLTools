;
;
;     Program written by Robert Livingston, 2013-05-01
;
;     RFL:DRAWPLANE draws a 3D plane at STA and offsets
;
;
(defun DRAWPLANE (STA LOS ROS BOS TOS / ATTREQ ANGBASE ANGDIR OSMODE ORTHOMODE P PLT PLB PRT PRB Z)
 (setq ATTREQ (getvar "ATTREQ"))
 (setvar "ATTREQ" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0.0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 1)
 (setq OSMODE (getvar "OSMODE"))
 (setvar "OSMODE" 0)
 (setq ORTHOMODE (getvar "ORTHOMODE"))
 (setvar "ORTHOMODE" 0)

 (if (or (= nil RFL:PVILIST) (= nil (setq Z (RFL:ELEVATION STA))))
  (setq Z 0.0)
 )
 (if (/= nil (setq P (RFL:XY (list STA LOS))))
  (progn
   (setq PLT (list (car P) (cadr P) (+ Z TOS)))
   (setq PLB (list (car P) (cadr P) (+ Z BOS)))
   (setq P (RFL:XY (list STA ROS)))
   (setq PRT (list (car P) (cadr P) (+ Z TOS)))
   (setq PRB (list (car P) (cadr P) (+ Z BOS)))
   (command "._3DFACE" PLB PRB PRT PLT "")
  )
 )

 (setvar "ATTREQ" ATTREQ)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
 (setvar "OSMODE" OSMODE)
 (setvar "ORTHOMODE" ORTHOMODE)
)
