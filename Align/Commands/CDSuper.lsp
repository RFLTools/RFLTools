;
;
;   Program written by Robert Livingston, 99/10/08
;
;   DSUPER inserts SUPER blocks along the current alignment
;
;
(defun C:DSUPER (/ AL DIMZIN CMDECHO REP SFLAG STEP STEPSTA)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (RFL:DSUPER)

 (setvar "CMDECHO" CMDECHO)
)