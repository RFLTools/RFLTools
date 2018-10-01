;
;
;     Program written by Robert Livingston, 01/02/14
;
;     LARC is a small utility for labelling arc and spiral entities
;
;
(setq RFL:LARCSCALE 0.1 RFL:LARCLAYER "")
(setq RFL:LARCCIRCLES "Yes")
(setq RFL:LARCSPIRALS "B")
(setq RFL:LARCTHEIGHT 0.75)
(defun C:LARCSETUP (/ CMDECHO TMP)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (setq TMP (getreal (strcat "\nEnter scale for blocks <" (rtos RFL:LARCSCALE) "> : " )))
 (if (/= nil TMP) (setq RFL:LARCSCALE TMP))
 (setq RFL:LARCLAYER (getstring "\nEnter layer of new entities (<return> for current layer) : "))
 (initget "Yes No")
 (setq RFL:LARCCIRCLES (getkword "\nDraw circles at entity ends (<Yes>/No) : "))
 (initget "Yes No")
 (if (= RFL:LARCCIRCLES nil) (setq RFL:LARCCIRCLES "Yes"))
 (setq LARCEQUALS (getkword "\nInclude equals '=' (Yes/<No>) : "))
 (if (= LARCEQUALS nil) (setq LARCEQUALS "No"))
 (initget "A L B")
 (setq RFL:LARCSPIRALS (getkword "\nLabel spiral A, Ls or Both (A/L/<B>) : "))
 (if (= RFL:LARCSPIRALS nil) (setq RFL:LARCSPIRALS "B"))
 (setq RFL:LARCTHEIGHT (getdist "\nLabel text height <0.75> :"))
 (if (or (= RFL:LARCTHEIGHT nil) (= RFL:LARCTHEIGHT 0.0)) (setq RFL:LARCTHEIGHT 0.75))

 (setvar "CMDECHO" CMDECHO)
)
(defun C:LARC (/ *error* A ACTIVEDOC ACTIVESPC ANG ANG1 ANG2 ANGBASE ANGDIR CHECKDUPLICATE CLAYER CMDECHO DIMZIN ENT ENTLIST LO LS OS OSMODE P P1 P2 PC R RL SPIRALDATA TEXTOBJ TEXTSTRING VT)
 (setq CLAYER (getvar "CLAYER"))
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq OSMODE (getvar "OSMODE"))
 (setvar "OSMODE" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0.0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 0)
 (setq DIMZIN (getvar "DIMZIN"))
 (setvar "DIMZIN" 8)

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

 (defun CHECKDUPLICATE (/ ENT ENTLIST ENTSET)
  (setq ENT (entlast))
  (setq ENTLIST (entget ENT))
  (if (and (= "INSERT" (cdr (assoc 0 ENTLIST))) (= "CIRC" (cdr (assoc 2 ENTLIST))))
   (progn
    (setq ENTSET (ssget "X" (list (cons 0 "INSERT")
                                  (cons 2 "CIRC")
                                  (assoc 8 ENTLIST)
                                  (assoc 10 ENTLIST)
                                  (assoc 41 ENTLIST)
                                  (assoc 42 ENTLIST)
                                  (assoc 43 ENTLIST)
                                  (assoc 50 ENTLIST)
                            )
                 )
    )
    (if (> (sslength ENTSET) 1) (command "._ERASE" ENT ""))
   )
  )
 )
 (command "._UNDO" "M")

 (command "._UCS" "W")

 (if (/= "" RFL:LARCLAYER)
  (progn
   (if (= nil (tblsearch "LAYER" RFL:LARCLAYER))
    (command "._LAYER" "M" RFL:LARCLAYER)
    (setvar "CLAYER" RFL:LARCLAYER)
   )
  )
 )

 (setq VT (getvar "VIEWTWIST"))
 (if (> VT pi) (setq VT (- VT (* 2.0 pi))))
 (if (and (= RFL:LARCCIRCLES "Yes") (not (tblsearch "BLOCK" "CIRC"))) (RFL:MAKEENT "CIRC"))
 (if (/= nil (setq ENT (car (entsel "Select arc or LDD spiral : "))))
  (progn
   (setq ENTLIST (entget ENT))
   (if (/= "ARC" (cdr (assoc 0 ENTLIST)))
    (if (= (setq SPIRALDATA (RFL:GETSPIRALDATA ENT)) nil)
     (if (/= "LINE" (cdr (assoc 0 ENTLIST)))
      (princ "\n***** NOT AN ARC OR LDD SPIRAL *****")
      (progn
       (setq P1 (cdr (assoc 10 ENTLIST)))
       (setq P2 (cdr (assoc 11 ENTLIST)))
       (if (= RFL:LARCCIRCLES "Yes")
        (progn
         (vla-insertblock ACTIVESPC
                          (vlax-3D-point P1)
                          "CIRC"
                          RFL:LARCSCALE
                          RFL:LARCSCALE
                          RFL:LARCSCALE
                          0.0
         )
         (CHECKDUPLICATE)
         (vla-insertblock ACTIVESPC
                          (vlax-3D-point P2)
                          "CIRC"
                          RFL:LARCSCALE
                          RFL:LARCSCALE
                          RFL:LARCSCALE
                          0.0
         )
         (CHECKDUPLICATE)
        )
       )

       (setq P (list (/ (+ (nth 0 P1) (nth 0 P2)) 2.0) (/ (+ (nth 1 P1) (nth 1 P2)) 2.0)))
       (if (and (> (angle P P2) (- (* pi 0.5) VT)) (< (angle P P2) (- (* pi 1.5) VT)))
        (setq ANG (- (angle P P2) pi))
        (setq ANG (angle P P2))
       )
       (setq TEXTSTRING "TAN")
       (if (setq TEXTOBJ (vla-addtext ACTIVESPC
                                      TEXTSTRING
                                      (vlax-3D-point P)
                                      RFL:LARCTHEIGHT
                         )
           )
        (progn
         (vla-put-Alignment textObj acAlignmentMiddle)
         (vla-put-Rotation textObj ANG)
         (vla-put-TextAlignmentPoint textObj (vlax-3D-point P))
        )
       )
      )
     )
     (progn
      (setq LS (RFL:GETSPIRALLS ENT))
      (setq A (RFL:GETSPIRALA ENT))

      (if (listp (last SPIRALDATA))
       (setq LO 0.0)
       (setq LO (last SPIRALDATA))
      )
      (setq LS (- LS LO))

      (setq OS (RFL:SPIRALOFFSET ENT))
      (if (< (abs OS) RFL:TOL) (setq OS 0.0))

      (if (/= 0.0 OS)
       (progn
        (setq R (RFL:GETSPIRALR ENT))
        (if (= LO 0.0)
         (setq RL 0.0)
         (setq RL (/ (* A A) LO))
        )
        (setq LS (+ LS (* OS (* A A 0.5 (- (/ 1.0 (* R R))
                                         (if (= LO 0.0) 0.0 (/ 1.0 (* RL RL)))
                                      )
                             )
                       )
                 )
        )
       )
      )

      (if (= "LWPOLYLINE" (cdr (assoc 0 ENTLIST)))
       (progn
        (setq P1 (cdr (assoc 10 ENTLIST)))
        (setq P1 (list (nth 0 P1) (nth 1 P1) 0.0))
        (setq P2 (cdr (assoc 10 (reverse ENTLIST))))
        (setq P2 (list (nth 0 P2) (nth 1 P2) 0.0))
       )
       (progn
        (setq ENT (entnext ENT))
        (setq ENTLIST (entget ENT))
        (setq P1 (cdr (assoc 10 ENTLIST)))
        (while (/= "SEQEND" (cdr (assoc 0 ENTLIST)))
         (setq P2 (cdr (assoc 10 ENTLIST)))
         (setq ENT (entnext ENT))
         (setq ENTLIST (entget ENT))
        )
       )
      )
      (if (= RFL:LARCCIRCLES "Yes")
       (progn
        (vla-insertblock ACTIVESPC
                         (vlax-3D-point P1)
                         "CIRC"
                         RFL:LARCSCALE
                         RFL:LARCSCALE
                         RFL:LARCSCALE
                         0.0
        )
        (CHECKDUPLICATE)
        (vla-insertblock ACTIVESPC
                         (vlax-3D-point P2)
                         "CIRC"
                         RFL:LARCSCALE
                         RFL:LARCSCALE
                         RFL:LARCSCALE
                         0.0
        )
        (CHECKDUPLICATE)
       )
      )

      (setq P (list (/ (+ (nth 0 P1) (nth 0 P2)) 2.0) (/ (+ (nth 1 P1) (nth 1 P2)) 2.0)))
      (if (and (> (angle P P2) (- (* pi 0.5) VT)) (< (angle P P2) (- (* pi 1.5) VT)))
       (setq ANG (- (angle P P2) pi))
       (setq ANG (angle P P2))
      )
      (if (or (= RFL:LARCSPIRALS "L") (= RFL:LARCSPIRALS "B"))
       (progn
        (setq TEXTSTRING (strcat (if (> (abs OS) 0.0) "os-" "") (if (> LO 0.0) "c" "") (if (= LARCEQUALS "Yes") "Ls=" "Ls") (rtos LS 2 1)))
        (if (= RFL:LARCSPIRALS "B")
         (progn
          (setq TEXTSTRING (strcat TEXTSTRING " / " (if (= LARCEQUALS "Yes") "A=" "A") (rtos A 2 1)))
         )
        )
       )
       (progn
        (if (= RFL:LARCSPIRALS "A")
         (progn
          (setq TEXTSTRING (strcat (if (= LARCEQUALS "Yes") "A=" "A") (rtos A 2 1)))
         )
        )
       )
      )
      (if (setq TEXTOBJ (vla-addtext ACTIVESPC
                                     TEXTSTRING
                                     (vlax-3D-point P)
                                     RFL:LARCTHEIGHT
                        )
          )
       (progn
        (vla-put-Alignment textObj acAlignmentMiddle)
        (vla-put-Rotation textObj ANG)
        (vla-put-TextAlignmentPoint textObj (vlax-3D-point P))
       )
      )
     )
    )
    (progn
     (setq PC (cdr (assoc 10 ENTLIST)))
     (setq R (cdr (assoc 40 ENTLIST)))
     (setq ANG1 (cdr (assoc 50 ENTLIST)))
     (setq ANG2 (cdr (assoc 51 ENTLIST)))
     (setq P1 (list (+ (nth 0 PC) (* R (cos ANG1)))
                    (+ (nth 1 PC) (* R (sin ANG1)))
              )
     )
     (setq P2 (list (+ (nth 0 PC) (* R (cos ANG2)))
                    (+ (nth 1 PC) (* R (sin ANG2)))
              )
     )
     (if (= RFL:LARCCIRCLES "Yes")
      (progn
       (vla-insertblock ACTIVESPC
                        (vlax-3D-point P1)
                        "CIRC"
                        RFL:LARCSCALE
                        RFL:LARCSCALE
                        RFL:LARCSCALE
                        0.0
       )
       (CHECKDUPLICATE)
       (vla-insertblock ACTIVESPC
                        (vlax-3D-point P2)
                        "CIRC"
                        RFL:LARCSCALE
                        RFL:LARCSCALE
                        RFL:LARCSCALE
                        0.0
       )
       (CHECKDUPLICATE)
      )
     )

     (setq P (list (/ (+ (nth 0 P1) (nth 0 P2)) 2.0) (/ (+ (nth 1 P1) (nth 1 P2)) 2.0)))
     (if (and (> (angle P P2) (- (* pi 0.5) VT)) (< (angle P P2) (- (* pi 1.5) VT)))
      (setq ANG (- (angle P P2) pi))
      (setq ANG (angle P P2))
     )
     (setq TEXTSTRING (strcat (if (= LARCEQUALS "Yes") "R=" "R") (rtos R 2 1)))
     (if (setq TEXTOBJ (vla-addtext ACTIVESPC
                                    TEXTSTRING
                                    (vlax-3D-point P)
                                    RFL:LARCTHEIGHT
                       )
         )
      (progn
       (vla-put-Alignment textObj acAlignmentMiddle)
       (vla-put-Rotation textObj ANG)
       (vla-put-TextAlignmentPoint textObj (vlax-3D-point P))
      )
     )
    )
   )
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
(princ "\n*****  Use LARCSETUP to change defaults *****\n")