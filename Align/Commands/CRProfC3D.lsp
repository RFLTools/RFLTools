;
;
;     Program written by Robert Livingston, 11-03-09
;
;     RPROF3D reads the profile from a selected C3D profile
;     NOTE - Must be using C3D, will not work in straight AutoCAD
;     NOTE - Works for type 1 and type 3 vertical curves
;
;
(defun C:RPROFC3D (/ *error* ENT)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (defun *error* (msg)
  (setvar "CMDECHO" CMDECHO)
  ;(setq *error* nil)
  (princ msg)
 )

 (if (setq ENT (car (entsel "\nSelect C3D profile : ")))
  (RFL:RPROFC3D ENT)
 )

 (setvar "CMDECHO" CMDECHO)
 T
)