;
;
;     Program written by Robert Livingston, 2008-11-04
;
;     RAB loads hor/vrt/E/OG from a selected nested RFLALign block
;
;
(defun C:RABN (/ BLKENT BLKENTLIST CMDECHO)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (setq BLKENT (car (nentsel "\nSelect nested RFL Alignment Block : ")))
 (setq BLKENT (cdr (assoc 330 (entget BLKENT))))
 (setq BLKENTLIST (entget BLKENT))
 (if (and (= "INSERT" (cdr (assoc 0 BLKENTLIST))) (/= nil (vl-string-search "RFLALIGN" (strcase (cdr (assoc 2 BLKENTLIST))))))
  (progn
   (RFL:RALIGNB BLKENT)
   (RFL:RPROFB BLKENT)
   (RFL:RSUPERB BLKENT)
   (RFL:RPROFOGB BLKENT)
  )
  (princ "\n*** NOT AN RFL ALIGNMENT BLOCK ***")
 )

 (setvar "CMDECHO" CMDECHO)
)
