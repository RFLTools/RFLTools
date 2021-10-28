;
;
;     Program written by Robert Livingston, 2021-10-07
;
;     RFL:INSERT is a uility to insert a block
;
;    BLOCKNAME  : Block Name
;    P          : 2D or 3D Point
;    XS, YS, ZS : Scale
;    R          : Rotation (radians)
;
(defun RFL:INSERT (BLOCKNAME P XS YS ZS R / ACTIVEDOC ACTIVESPACE)
 (setq ACTIVEDOC (vla-get-activedocument (vlax-get-acad-object)))
 (setq ACTIVESPC
       (vlax-get-property ACTIVEDOC
        (if (or (eq acmodelspace (vla-get-activespace ACTIVEDOC)) (eq :vlax-true (vla-get-mspace ACTIVEDOC)))
         'modelspace
         'paperspace
        )
       )
 )
 (if BLOCKNAME
  (progn
   (if (= 2 (length P)) (setq P (append P (list 0.0))))
   (if RFL:MAKEENT (if (= nil (tblsearch "BLOCK" BLOCKNAME)) (RFL:MAKEENT BLOCKNAME))) ; this line can be removed if not using RFLTools
   (if (tblsearch "BLOCK" BLOCKNAME)
    (if (vla-insertblock ACTIVESPC
                         (vlax-3D-point P)
                         BLOCKNAME
                         XS
                         YS
                         ZS
                         R
        )
     (entlast)
     nil
    )
    nil
   )
  )
  nil
 )
)