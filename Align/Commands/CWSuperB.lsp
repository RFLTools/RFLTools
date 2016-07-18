;
;
;   Program written by Robert Livingston, 2008/11/04
;
;   WSUPERB writes the superelevation to a RFLALIGN Block
;
;
(defun C:WSUPERB (/ CMDECHO BLKENT BLKENTLIST ENT ENTLIST)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (setq BLKENT (car (entsel "\nSelect RFL Alignment Block : ")))
 (setq BLKENTLIST (entget BLKENT))
 (if (and (= "INSERT" (cdr (assoc 0 BLKENTLIST))) (= "RFLALIGN" (strcase (cdr (assoc 2 BLKENTLIST)))))
  (if (= nil RFL:SUPERLIST)
   (RFL:RABKILL BLKENT "E")
   (RFL:WSUPERB BLKENT)
  )
  (princ "\n*** NOT AN RFL ALIGNMENT BLOCK ***")
 )

 (setvar "CMDECHO" CMDECHO)
)
