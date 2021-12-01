;
;
;     Program written by Robert Livingston, 10-04-30
;
;     RALIGNC3D reads the alignment from a selected C3D alignment
;     NOTE - Must be using C3D, will not work in straight AutoCAD
;     NOTE - Works for type 1, type 2, type 3 and type 4 alignment entities
;
;
(defun C:RALIGNC3D (/ *error* CMDECHO)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (if (= nil vlax-create-object) (vl-load-com))

 (defun *error* (msg)
  (setvar "CMDECHO" CMDECHO)
  ;(setq *error* nil)
  (princ msg)
 )

 (setq OBALIGNMENT (RFL:GETC3DALIGNMENT))
 (RFL:RALIGNC3D OBALIGNMENT)
 (setvar "CMDECHO" CMDECHO)

 nil
)