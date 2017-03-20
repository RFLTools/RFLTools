;
;
;     Program written by Robert Livingston, 2001/01/11
;
;     C:3DP2ALIGN converts a 3d polyline to horizontal and vertical alignments
;
;
(defun C:3DP2ALIGN (/ CMDECHO ENT FILENAME STA)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (setq ENT (car (entsel "\nSelect 3d polyline : ")))
 (setq STA (getreal "\nEnter starting chainage : "))
 (RFL:3DP2ALIGN ENT STA)

 (setvar "CMDECHO" CMDECHO)
)
