;
;
;   Program written by Robert Livingston, 99/12/03
;
;   C:VPL labels a selected point's station and elevation
;
(defun C:VPL (/ ANGBASE ANGDIR CMDECHO ENT ENTLIST OFFSET P1 P2 P3 SIGN STA STR STR2 STR3 STR4 *error* TMP Z)
 (defun *error* (msg)
  (terpri)
  ;(setq *error* nil)
 )
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 0)

 (defun SIGN (X)
  (if (< X 0)
   (eval -1)
   (eval 1)
  )
 )

 (if (= RFL:ELEVATION nil)
  (progn
   (princ "\n*****   Alignment utilities not loaded   *****")
  )
  (progn
   (if (= RFL:PROFDEFLIST nil) (RFL:PROFDEF))
   (setq P1 (getpoint "\nEnter point :"))
   (setq P2 (getpoint P1 "Second point for leader :"))
   (setq TMP (RFL:VPP P1))
   (setq STA (car TMP))
   (setq Z (cadr TMP))
   (setq STR (strcat "Sta.: " (RFL:STATXT STA)))
   (setq STR2 (strcat "Elev.: " (rtos Z)))
   (setq TMP (RFL:ELEVATION STA))
   (if (/= TMP nil)
    (setq STR3 (strcat "Ctrl.Elev.: " (rtos TMP)))
    (setq STR3 nil)
   )
   (setq TMP (RFL:SLOPE STA))
   (if (/= TMP nil)
    (setq STR4 (strcat "Ctrl.Grade: " (rtos (* 100.0 TMP)) "%"))
    (setq STR4 nil)
   )

   (command "LEADER" "_NON" P1 "_NON" P2 "" STR)
   (if (/= nil STR2) (command  STR2))
   (if (/= nil STR3) (command  STR3))
   (if (/= nil STR4) (command  STR4))
   (command "")
  )
 )

 (setvar "CMDECHO" CMDECHO)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
)
