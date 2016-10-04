;
;
;   Program written by Robert Livingston, 2008-11-04
;
;   RSUPERB reads the Superelevation from a RFLAlign Block
;
;
(defun C:RSUPERB (/ CMDECHO BLKENT BLKENTLIST)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (setq BLKENT (car (entsel "\nSelect RFL Alignment Block : ")))
 (setq BLKENTLIST (entget BLKENT))
 (if (and (= "INSERT" (cdr (assoc 0 BLKENTLIST))) (= "RFLALIGN" (strcase (cdr (assoc 2 BLKENTLIST)))))
  (RFL:RSUPERB BLKENT)
  (princ "\n*** NOT AN RFL ALIGNMENT BLOCK ***")
 )

 (setvar "CMDECHO" CMDECHO)
)
