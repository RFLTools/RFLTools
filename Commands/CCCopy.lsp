;
;
;    Program Written by Robert Livingston, 91/04/01
;    CCOPY copies a selected object to the current layer
;
;
(defun C:CCOPY ()

 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (setq CLAYER (getvar "CLAYER"))

 (setq S (nth 0 (entsel "Select Object :")))

 (command "COPY" S "" "0,0" "0,0")
 (command "CHANGE" "L" "" "P" "LA" CLAYER "")

 (setvar "CMDECHO" CMDECHO)
)
