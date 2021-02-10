;
;
;     Program written by Robert Livingston, 2016-03-30
;
;     DRAWBARRIER is a utility for drawing a polyline from one sta/os to another os at a specified length
;
;
(setq RFL:DRAWBARRIERSEGS 20)
(setq RFL:DRAWBARRIERLENGTH 24.2)
(setq RFL:DRAWBARRIEROS 6.45)
(defun C:DRAWBARRIER (/ D D1 D2 DOS DSEG OS OS1 OS2 P STA STA1 STA2 STAM TMP TOL)
 (setq TOL 0.00001)
 (if (= nil (setq P (getpoint "\nStart point (<return> to enter Sta/OS) : ")))
  (progn
   (setq STA (getreal "\nStart Sta : "))
   (setq OS1 (getreal "\nStart OS : "))
  )
  (progn
   (setq P (RFL:STAOFF P))
   (if (= nil (setq STA (getreal (strcat "\nStart Sta <" (rtos (car P)) "> : "))))
    (setq STA (car P))
   )
   (if (= nil (setq OS1 (getreal (strcat "\nStart OS <" (rtos (cadr P)) ">: "))))
    (setq OS1 (cadr P))
   )
  )
 )
 (setq TMP (getreal (strcat "\nLength (+ve = upchainage, -ve = downchainage) <" (rtos RFL:DRAWBARRIERLENGTH) "> : ")))
 (if (/= nil TMP) (setq RFL:DRAWBARRIERLENGTH TMP))
 (setq TMP (getreal (strcat "\nEnd OS (+ve = right, -ve = left) <" (rtos RFL:DRAWBARRIEROS) "> : ")))
 (if (/= nil TMP) (setq RFL:DRAWBARRIEROS TMP))
 (setq TMP (getint (strcat "\nNumber of pline segments <" (itoa RFL:DRAWBARRIERSEGS) "> : ")))
 (if (/= nil TMP) (setq RFL:DRAWBARRIERSEGS TMP))
 (setq OS2 RFL:DRAWBARRIEROS)
 (setq DOS (/ (- OS2 OS1) RFL:DRAWBARRIERSEGS))
 (setq DSEG (/ RFL:DRAWBARRIERLENGTH RFL:DRAWBARRIERSEGS))
 (command "._PLINE" "_NON" (setq P (RFL:XY (list STA OS1))))
 (repeat RFL:DRAWBARRIERSEGS
  (setq STA1 STA)
  (setq STA2 (+ STA DSEG DSEG))
  (setq STAM (/ (+ STA1 STA2) 2.0))
  (setq OS1 (+ OS1 DOS))
  (while (> (abs (- STA1 STA2)) TOL)
   (if (> (distance P (RFL:XY (list STAM OS1))) (abs DSEG))
    (setq STA2 STAM)
    (setq STA1 STAM)
   )
   (setq STAM (/ (+ STA1 STA2) 2.0))
  )
  (setq STA STAM)
  (command "_NON" (setq P (RFL:XY (list STA OS1))))
 )
 (command "")
 nil
)
