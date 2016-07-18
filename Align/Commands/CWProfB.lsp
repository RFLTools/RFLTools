;
;
;   Program written by Robert Livingston, 2008/11/04
;
;   WPROFB writes a vertical alinment to a RFLALIGN Block
;
;
(defun C:WPROFB (/ CMDECHO BLKENT BLKENTLIST ENT ENTLIST)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (setq BLKENT (car (entsel "\nSelect RFL Alignment Block : ")))
 (setq BLKENTLIST (entget BLKENT))
 (if (and (= "INSERT" (cdr (assoc 0 BLKENTLIST))) (= "RFLALIGN" (strcase (cdr (assoc 2 BLKENTLIST)))))
  (if (= nil RFL:PVILIST)
   (RFL:RABKILL BLKENT "VRT")
   (RFL:WPROFB BLKENT)
  )
  (princ "\n*** NOT AN RFL ALIGNMENT BLOCK ***")
 )

 (setvar "CMDECHO" CMDECHO)
)
