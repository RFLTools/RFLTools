;
;
;     Program written by Robert Livingston, 2016-03-03
;
;     SCALEXYZ is a utility to scale objects by relative XYZ
;
;
(defun C:SCALEXYZ (/ *error* ACTIVEDOC ACTIVESPACE BLOCKNAME CMDECHO ENTSET PB SX SY SZ TMP)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (if (/= nil (setq ENTSET (ssget)))
  (progn
   (setq BLOCKNAME "SCALEXYZBLOCK")
   (setq ACTIVEDOC (vla-get-activedocument (vlax-get-acad-object)))
   (setq ACTIVESPC
         (vlax-get-property ACTIVEDOC
          (if (or (eq acmodelspace (vla-get-activespace ACTIVEDOC)) (eq :vlax-true (vla-get-mspace ACTIVEDOC)))
           'modelspace
           'paperspace
          )
         )
   )
   (if (= nil (setq TMP (getpoint "\nBase point (0,0,0) : ")))
    (setq PB (list 0.0 0.0 0.0))
    (setq PB TMP)
   )
   (if (= nil (setq TMP (getdist (strcat "\n'X' scale factor (" (rtos 1.0) ") : "))))
    (setq SX 1.0)
    (setq SX TMP)
   )
   (if (= nil (setq TMP (getdist (strcat "\n'Y' scale factor (" (rtos SX) ") : "))))
    (setq SY SX)
    (setq SY TMP)
   )
   (if (= nil (setq TMP (getdist (strcat "\n'Z' scale factor (" (rtos SY) ") : "))))
    (setq SZ SY)
    (setq SZ TMP)
   )
   (command "._BLOCK" BLOCKNAME PB ENTSET "")
   (setq TMP (vla-insertblock ACTIVESPC
                              (vlax-3D-point PB)
                              BLOCKNAME
                              SX
                              SY
                              SZ
                              0.0
             )
   )
   (command "._EXPLODE" (vlax-vla-object->ename TMP))
   (command "._PURGE" "Blocks" BLOCKNAME "No")
  )
 )
 (setvar "CMDECHO" CMDECHO)
 T
)