;
;
;     Program written by Robert Livingston, 2008-11-04
;
;     RABKILL removes alignment definition lists from RFLALIGN blocks
;
;
(defun C:RABKILL (/ ENT)
 (command "._UNDO" "M")
 (setq ENT (car (entsel "\nSelect Alignment Block : ")))
 (RFL:RABKILL ENT "HOR")
 (setq ENT (entlast))
 (RFL:RABKILL ENT "VRT")
 (setq ENT (entlast))
 (RFL:RABKILL ENT "OG")
 (setq ENT (entlast))
 (RFL:RABKILL ENT "E")
)
