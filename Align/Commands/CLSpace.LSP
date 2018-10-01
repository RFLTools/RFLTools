;
;
;     Program written by Robert Livingston, 2018/10/01
;
;     LSPACE is a utility for labelling spaces
;
;
(setq RFL:LSPACELAYER "")
(setq RFL:LSPACETHEIGHT 0.75)
(setq RFL:LSPACESUFFIX "m")
(setq RFL:LSPACEPREC 2)
(defun C:LSPACESETUP (/ CMDECHO TMP)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (setq RFL:LSPACELAYER (getstring "\nEnter layer of new entities (<return> for current layer) : "))
 (setq RFL:LSPACETHEIGHT (getdist "\nLabel text height <0.75> : "))
 (if (or (= RFL:LSPACETHEIGHT nil) (= RFL:LSPACETHEIGHT 0.0)) (setq RFL:LSPACETHEIGHT 0.75))
 (setq RFL:LSPACESUFFIX (getstring "\nEnter suffix for label text : "))
 (setq RFL:LSPACEPREC (getint "\nLabel text precision <2> : "))
 (if (or (= RFL:LSPACEPREC nil) (<= RFL:LSPACEPREC 0)) (setq RFL:LSPACEPREC 2))

 (setvar "CMDECHO" CMDECHO)
)
(defun C:LSPACE (/ *error* ACTIVEDOC ACTIVESPC ANG P P1 P2 VT)
 (setq CLAYER (getvar "CLAYER"))
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq OSMODE (getvar "OSMODE"))
 (setvar "OSMODE" 640)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0.0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 0)
 (setq DIMZIN (getvar "DIMZIN"))
 (setvar "DIMZIN" 0)

 (setq ACTIVEDOC (vla-get-activedocument (vlax-get-acad-object)))
 (setq ACTIVESPC
       (vlax-get-property ACTIVEDOC
        (if (or (eq acmodelspace (vla-get-activespace ACTIVEDOC)) (eq :vlax-true (vla-get-mspace ACTIVEDOC)))
         'modelspace
         'paperspace
        )
       )
 )

 (command "._UNDO" "M")
 
 (defun *error* (msg)
  (if (>= (atof (getvar "ACADVER")) 18.2)
   (command-s "._UCS" "P")
   (command "._UCS" "P")
  )
  (setvar "CLAYER" CLAYER)
  (setvar "CMDECHO" CMDECHO)
  (setvar "OSMODE" OSMODE)
  (setvar "ANGBASE" ANGBASE)
  (setvar "ANGDIR" ANGDIR)
  (setvar "DIMZIN" DIMZIN)
  ;(setq *error* nil)
  (princ msg)
 )

 (command "._UNDO" "M")

 (command "._UCS" "W")

 (if (/= "" RFL:LSPACELAYER)
  (progn
   (if (= nil (tblsearch "LAYER" RFL:LSPACELAYER))
    (command "._LAYER" "M" RFL:LSPACELAYER)
    (setvar "CLAYER" RFL:LSPACELAYER)
   )
  )
 )

 (setq VT (getvar "VIEWTWIST"))
 (if (> VT pi) (setq VT (- VT (* 2.0 pi))))
 (setq P1 (getpoint "\nFirst point : "))
 (setq P1 (list (car P1) (cadr P1)))
 (setq P2 (getpoint P1 "\nSecond point : "))
 (setq P2 (list (car P2) (cadr P2)))
 (setq P (list (/ (+ (car P1) (car P2)) 2.0) (/ (+ (cadr P1) (cadr P2)) 2.0)))
 (if (and (> (angle P P2) (- 0.0 VT)) (< (angle P P2) (- pi VT)))
  (setq ANG (- (angle P P2) (/ pi 2)))
  (setq ANG (+ (angle P P2) (/ pi 2)))
 )
 (setq TEXTSTRING (strcat (rtos (distance P1 P2) 2 RFL:LSPACEPREC) RFL:LSPACESUFFIX))
 (if (setq TEXTOBJ (vla-addtext ACTIVESPC
                                TEXTSTRING
                                (vlax-3D-point P)
                                RFL:LSPACETHEIGHT
                   )
     )
  (progn
   (vla-put-Alignment textObj acAlignmentMiddle)
   (vla-put-Rotation textObj ANG)
   (vla-put-TextAlignmentPoint textObj (vlax-3D-point P))
  )
 )

 (command "._UCS" "P")
 (setvar "CLAYER" CLAYER)
 (setvar "CMDECHO" CMDECHO)
 (setvar "OSMODE" OSMODE)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
 (setvar "DIMZIN" DIMZIN)
)
(princ "\n*****  Use LSPACESETUP to change defaults *****\n")