;
;
;     Program written by Robert Livingston, 2016/07/11
;
;     C:PVI0 draws a pviblock with 0.0 for K and L at the selected point
(defun C:PVI0 (/ ACTIVEDOC ACTIVESPACE P)
 (vl-load-com)
 (setq ACTIVEDOC (vla-get-activedocument (vlax-get-acad-object)))
 (setq ACTIVESPC
       (vlax-get-property ACTIVEDOC
        (if (or (eq acmodelspace (vla-get-activespace ACTIVEDOC)) (eq :vlax-true (vla-get-mspace ACTIVEDOC)))
         'modelspace
         'paperspace
        )
       )
 )
 (if (setq P (getpoint "\nSelect point : "))
  (progn
   (if (= nil (tblsearch "BLOCK" "PVI2")) (RFL:MAKEENT "PVI2"))
   (vla-insertblock ACTIVESPC
                    (vlax-3D-point P)
                    "PVI2"
                    25.4
                    25.4
                    25.4
                    0.0
   )
   (entlast)
  )
  nil
 )
)
