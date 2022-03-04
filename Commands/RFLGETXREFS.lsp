;
;
;     Program written by Robert Livingston, 2022-03-04
;
;     RFL:GETXREFS returns a list of all the XREFS in the current drawing
;          Thanks to earv at https://www.afralisp.net/archive/Tips/code66.htm
;
;
(defun RFL:GETXREFS (/ BLKNAME ACTIVEDOCUMENT ITEM REFLIST)
 (vl-load-com)
 (setq ACTIVEDOCUMENT (vla-get-activedocument (vlax-get-Acad-Object)))
 (setq REFLIST '())
 ;process each block
 (vlax-for ITEM (vla-get-blocks ACTIVEDOCUMENT)
  (if (= (vlax-get-property ITEM 'isXref) :vlax-true)
   (progn
    (setq BLKNAME (vlax-get-property ITEM 'Name))
    (setq REFLIST (append (list BLKNAME) REFLIST))
   )
  )
 )
 REFLIST
)
