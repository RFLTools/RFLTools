;
;
;     Program written by Robert Livingston, 02-10-21
;
;     RFL:MAKEENT cycles through the block list returned by RFL:GETBLOCKLIST creating the block
;
;
(defun RFL:MAKEENT (BLKNAME / BLOCKLIST NODE)
 (setq BLOCKLIST (RFL:GETBLOCKLIST BLKNAME))
 (if (/= nil BLOCKLIST)
  (progn
   (entmake)
   (foreach NODE BLOCKLIST
    (entmake NODE)
   )
  )
  nil
 )
)
(defun RFL:GETBLOCKLIST (BLKNAME)
 (cond
