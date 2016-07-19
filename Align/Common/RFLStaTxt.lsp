;
;
;     Program written by Robert Livingston, 2014-11-20
;
;     RFL:STATXT converts a real to a station string
;
;
(setq RFL:STAPOS nil)
(defun RFL:STATXT (STA / C DIMZIN S STAH STAL)
 (if (= nil RFL:STAPOS) (if (= nil (setq RFL:STAPOS (getint "\nStation label '+' location <3> : "))) (setq RFL:STAPOS 3)))
 (setq DIMZIN (getvar "DIMZIN"))
 (setvar "DIMZIN" 8)
 (if (< RFL:STAPOS 1)
  (rtos STA)
  (progn
   (if (< STA 0.0)
    (setq S "-")
    (setq S "")
   )
   (setq STAH (fix (/ (abs STA) (expt 10 RFL:STAPOS))))
   (setq STAL (- (abs STA) (* STAH (expt 10 RFL:STAPOS))))
   (if (= (substr (rtos STAL) 1 (+ RFL:STAPOS 1)) (itoa (expt 10 RFL:STAPOS)))
    (progn
     (setq STAL 0.0)
     (setq STAH (+ STAH (RFL:SIGN STAH)))
    )
   )
   (setq STAH (itoa STAH))
   (setq C (- RFL:STAPOS (strlen (itoa (fix STAL)))))
   (setq STAL (rtos STAL 2 (getvar "LUPREC")))
   (while (> C 0)
    (setq STAL (strcat "0" STAL))
    (setq C (- C 1))
   )
   (setvar "DIMZIN" DIMZIN)
   (setq RFLSTAHTXT (strcat S STAH) RFLSTALTXT STAL)
   (strcat S STAH "+" STAL)
  )
 )
)
