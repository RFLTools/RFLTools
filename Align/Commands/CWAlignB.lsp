;
;
;   Program written by Robert Livingston, 2008/11/04
;
;   WALIGNB writes a horizontal alignment to a RFLALIGN Block
;
;
(defun C:WALIGNB (/ CMDECHO BLKENT BLKENTLIST ENT ENTLIST)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (setq BLKENT (car (entsel "\nSelect RFL Alignment Block : ")))
 (setq BLKENTLIST (entget BLKENT))
 (if (and (= "INSERT" (cdr (assoc 0 BLKENTLIST))) (= "RFLALIGN" (strcase (cdr (assoc 2 BLKENTLIST)))))
  (if (= nil RFL:ALIGNLIST)
   (RFL:RABKILL BLKENT "HOR")
   (RFL:WALIGNB BLKENT)
  )
  (princ "\n*** NOT AN RFL ALIGNMENT BLOCK ***")
 )

 (setvar "CMDECHO" CMDECHO)
)
