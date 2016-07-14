;
;
;   Program written by Robert Livingston, 99/10/08
;
;   C:GSUPER extracts superelevation from the current drawing for the current alignment
;
;
(defun C:GSUPER (/ ENTSET)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (if (/= RFL:ALIGNLIST nil)
  (progn
   (if (/= RFL:SUPERDEF nil)
    (progn
     (princ "\nSelect SUPER blocks :")
     (setq ENTSET (ssget))
     (if (/= ENTSET nil)
      (progn
       (RFL:SUPERDEF ENTSET)
      )
     )
    )
    (progn
     (princ "\n!!!!! Superelevation tools not loaded !!!!!\n")
    )
   )
  )
  (progn
   (princ "\n!!!!! Horizontal Alignment Not Set !!!!!\n")
  )
 )
 (setvar "CMDECHO" CMDECHO)
 nil
)
