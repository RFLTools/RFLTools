;
;
;
; Program written by Robert Livingston 90/09/07
;
;  SLANT sets the view angle to that specified by two points.
;        Return to normal view by entering SLANT 0,0 1,0 (hoz. line)
;
;
;
(defun C:SLANT (/ ANGBASE ANGDIR CMDECHO REGENMODE)
 (setq R (getvar "REGENMODE"))
 (setvar "REGENMODE" 1)
 (setvar "CMDECHO" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0.0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 0)
 (setq P1 (getpoint "\nEnter First Point :"))
 (setq P2 (getpoint P1 "\nSecond Point :"))

 (command "DVIEW" "" "TW" (/ (* -180 (angle P1 P2)) pi) "")
 (setvar "SNAPANG" (angle P1 P2))

 (setvar "REGENMODE" R)
 (setvar "CMDECHO" 1)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
)