;
;
;     Program written by Robert Livingston, 2016/07/11
;
;     C:LPROF is a utility for labelling profiles
;
;     xxxLAYER  :  '*' concatinates current layer
;
(setq RFL:LPROFLIST (list (cons "PROFDRAW" 0)           ;  Draw profile
                          (cons "PROFLAYER" "*")        ;  Profile Layer
                          (cons "TEXTLAYER" "*-LBL")    ;  Text Layer
                          (cons "TEXTHEIGHT" 3.5)       ;  Text Height
                          (cons "UNITS" "m")            ;  Linear Units
                          (cons "LSLOPE" 1)             ;  Label Slope
                          (cons "LL" 1)                 ;  Label 'L'
                          (cons "LK" 1)                 ;  Label 'K'
                          (cons "CNODES" 1)             ;  Circle Nodes
                          (cons "CNODERAD" 1.0)         ;  Node Circle Radius
                          (cons "CNODELENGTH" 10.0)     ;  Node Line Length
                          (cons "CNODEVOFFSET" 1.0)     ;  Node Text Vertical Offset
                          (cons "CNODEHOFFSET" 0.875)   ;  Node Text Horisontal Offset
                          (cons "DPVI" 1)               ;  Draw PVI
                          (cons "LPVI" 1)               ;  Label PVI
                          (cons "LBVC" 1)               ;  Label BVC and EVC
                          (cons "LHIGH" 0)              ;  Label 'high' Chainage (if not stations are labelled as '+234.567'
                          (cons "LELEVATIONSC" 1)       ;  Label Elevations Curves
                          (cons "LELEVATIONST" 1)       ;  Label Elevations Tangents
                          (cons "ELEVTEXTHEIGHT" 2.5)   ;  Elevation Text Height
                          (cons "ELEVTEXTINC" 20.0)     ;  Elevation Text Increment
                          (cons "ELEVTEXTOS" -20.0)     ;  Elevation Text Offset (from grid basepoint)
                          (cons "RAB" 0)                ;  Reverse Above/Below flag (1 = labels above with K/L below)
                          (cons "DIRECTION" "DIRRIGHT") ;  Label Direction (DIRRIGHT / DIRLEFT / DIRUP / DIRDOWN)
                          (cons "KPREC" 1)              ;  'K' Precision
                          (cons "LPREC" 0)              ;  'L' Precision
                          (cons "SLOPEPREC" 3)          ;  Slope Precision
                          (cons "STAPREC" 3)            ;  Station Precision
                          (cons "ELEVPREC" 3)           ;  Elevation Precision
                    )
)
(defun C:LPROF (/ ACTIVEDOC ACTIVESPC ADDTEXT ANG ANGBASE ANGDIR C CLAYER CMDECHO DIMZIN DIRECTIONT DIRECTIONS ENT
                  G1 G2 K L1 L2 L3 LUPREC MLMR MLMRT MLMRB NLAYER OSMODE P P1 P2 PLINETYPE PREVENT
                  REGENMODE REP SIDE STA STA1 STA2 STA3 STAH STAL STAHTXT SPLINETYPE
                  SPLINESEGS TEXTSTYLE TOL Z1 Z2 Z3 ZMAX ZMIN)
 (setq REGENMODE (getvar "REGENMODE"))
 (setvar "REGENMODE" 1)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq DIMZIN (getvar "DIMZIN"))
 (setq LUPREC (getvar "LUPREC"))
 (setvar "DIMZIN" 0)
 (setq CLAYER (getvar "CLAYER"))
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0.0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 0)
 (setq OSMODE (getvar "OSMODE"))
 (setvar "OSMODE" 0)
 (setq SPLINETYPE (getvar "SPLINETYPE"))
 (setvar "SPLINETYPE" 5)
 (setq SPLINESEGS (getvar "SPLINESEGS"))
 (setvar "SPLINESEGS" 65)
 (setq PLINETYPE (getvar "PLINETYPE"))
 (setvar "PLINETYPE" 0)
 (setq TEXTSTYLE (getvar "TEXTSTYLE"))

 (setq TOL 0.000001)
 
 (setq PREVENT nil)
 
 (command "._UNDO" "M")

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

 (defun ADDTEXT (STR P H AL A / ANGBASE ANGDIR OBJ P1)
  (setq ANGBASE (getvar "ANGBASE"))
  (setvar "ANGBASE" 0.0)
  (setq ANGDIR (getvar "ANGDIR"))
  (setvar "ANGDIR" 0)
  ; acAlignmentLeft          :   0
  ; acAlignmentCenter        :   1
  ; acAlignmentRight         :   2
  ; acAlignmentMiddle        :   4
  ; acAlignmentTopLeft       :   6
  ; acAlignmentTopCenter     :   7
  ; acAlignmentTopRight      :   8
  ; acAlignmentMiddleLeft    :   9
  ; acAlignmentMiddleCenter  :  10
  ; acAlignmentMiddleRight   :  11
  ; acAlignmentBottomLeft    :  12
  ; acAlignmentBottomCenter  :  13
  ; acAlignmentBottomRight   :  14
  (vla-addtext ACTIVESPC
               STR
               (vlax-3d-point P)
               H
  )
  (setq ENT (entlast))
  (setq OBJ (vlax-ename->vla-object ENT))
  (setq ENTLIST (entget ENT))
  (setq P1 (vlax-get-property OBJ 'InsertionPoint))
  (cond ((= AL "L")
         (progn
          (vlax-put-property OBJ 'Alignment acAlignmentLeft)
         )
        )
        ((= AL "C")
         (progn
          (vlax-put-property OBJ 'Alignment acAlignmentCenter)
          (vlax-put-property OBJ 'TextAlignmentPoint P1)
         )
        )
        ((= AL "R")
         (progn
          (vlax-put-property OBJ 'Alignment acAlignmentRight)
          (vlax-put-property OBJ 'TextAlignmentPoint P1)
         )
        )
        ((= AL "M")
         (progn
          (vlax-put-property OBJ 'Alignment acAlignmentMiddle)
          (vlax-put-property OBJ 'TextAlignmentPoint P1)
         )
        )
        ((= AL "TL")
         (progn
          (vlax-put-property OBJ 'Alignment acAlignmentTopLeft)
          (vlax-put-property OBJ 'TextAlignmentPoint P1)
         )
        )
        ((= AL "TC")
         (progn
          (vlax-put-property OBJ 'Alignment acAlignmentTopCenter)
          (vlax-put-property OBJ 'TextAlignmentPoint P1)
         )
        )
        ((= AL "TR")
         (progn
          (vlax-put-property OBJ 'Alignment acAlignmentTopRight)
          (vlax-put-property OBJ 'TextAlignmentPoint P1)
         )
        )
        ((= AL "ML")
         (progn
          (vlax-put-property OBJ 'Alignment acAlignmentMiddleLeft)
          (vlax-put-property OBJ 'TextAlignmentPoint P1)
         )
        )
        ((= AL "MC")
         (progn
          (vlax-put-property OBJ 'Alignment acAlignmentMiddleCenter)
          (vlax-put-property OBJ 'TextAlignmentPoint P1)
         )
        )
        ((= AL "MR")
         (progn
          (vlax-put-property OBJ 'Alignment acAlignmentMiddleRight)
          (vlax-put-property OBJ 'TextAlignmentPoint P1)
         )
        )
        ((= AL "BL")
         (progn
          (vlax-put-property OBJ 'Alignment acAlignmentBottomLeft)
          (vlax-put-property OBJ 'TextAlignmentPoint P1)
         )
        )
        ((= AL "BC")
         (progn
          (vlax-put-property OBJ 'Alignment acAlignmentBottomCenter)
          (vlax-put-property OBJ 'TextAlignmentPoint P1)
         )
        )
        ((= AL "BR")
         (progn
          (vlax-put-property OBJ 'Alignment acAlignmentBottomRight)
          (vlax-put-property OBJ 'TextAlignmentPoint P1)
         )
        )
  )
  (vlax-put-property OBJ 'Rotation A)
  (setvar "ANGBASE" ANGBASE)
  (setvar "ANGDIR" ANGDIR)
 )
 
 (if PVILIST
  (progn
   (RFL:PROFDEF)
   (if RFL:PROFDEFLIST
    (progn
     (if (= (cdr (assoc "PROFDRAW" RFL:LPROFLIST)) 1)
      (progn
       (setq NLAYER (cdr (assoc "PROFLAYER" RFL:LPROFLIST)))
       (if (= "*" (substr NLAYER 1 1)) (setq NLAYER (strcat CLAYER (substr NLAYER 2))))
       (if (not (tblsearch "LAYER" NLAYER))
        (entmake (list (cons 0 "LAYER")
                       (cons 100 "AcDbSymbolTableRecord")
                       (cons 100 "AcDbLayerTableRecord")
                       (cons 2 NLAYER)
                       (cons 70 0)
                 )
        )
       )
       (setvar "CLAYER" NLAYER)
       (command "._PLINE")
       (foreach TMP PVILIST
        (progn
         (if (< (cadddr TMP) TOL)
          (command (RFL:PROFPOINT (car TMP) (cadr TMP)))
          (progn
           (setq C 0)
           (while (<= C 64)
            (command (RFL:PROFPOINT (+ (- (car TMP) (/ (cadddr TMP) 2.0)) (* (/ (cadddr TMP) 64) C))
                                    (RFL:ELEVATION (+ (- (car TMP) (/ (cadddr TMP) 2.0)) (* (/ (cadddr TMP) 64) C)))
                     )
            )
            (setq C (+ C 1))
           )
          )
         )
        )
       )
       (command "")
       (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
      )
     )
     (if (or (= (cdr (assoc "LSLOPE" RFL:LPROFLIST)) 1)
             (= (cdr (assoc "LL" RFL:LPROFLIST)) 1)
             (= (cdr (assoc "LK" RFL:LPROFLIST)) 1)
             (= (cdr (assoc "CNODES" RFL:LPROFLIST)) 1)
             (= (cdr (assoc "DPVI" RFL:LPROFLIST)) 1)
             (= (cdr (assoc "LPVI" RFL:LPROFLIST)) 1)
             (= (cdr (assoc "LBVC" RFL:LPROFLIST)) 1)
             (= (cdr (assoc "LELEVATIONS" RFL:LPROFLIST)) 1)
         )
      (progn
       (if (or (= (cdr (assoc "DIRECTION" RFL:LPROFLIST)) 1)
               (= (cdr (assoc "DIRECTION" RFL:LPROFLIST)) nil)
           )
        (cond ((= (cdr (assoc "DIRECTION" RFL:LPROFLIST)) "DIRLEFT")
               (setq DIRECTIONT 1 DIRECTIONS 1)
              )
              ((= (cdr (assoc "DIRECTION" RFL:LPROFLIST)) "DIRRIGHT")
               (setq DIRECTIONT -1 DIRECTIONS 1)
              )
              ((= (cdr (assoc "DIRECTION" RFL:LPROFLIST)) "DIRUP")
               (setq DIRECTIONT 1 DIRECTIONS 1)
              )
              ((= (cdr (assoc "DIRECTION" RFL:LPROFLIST)) "DIRDOWN")
               (setq DIRECTIONT -1 DIRECTIONS 1)
              )
        )
        (cond ((= (cdr (assoc "DIRECTION" RFL:LPROFLIST)) "DIRLEFT")
               (setq DIRECTIONT 1 DIRECTIONS -1)
              )
              ((= (cdr (assoc "DIRECTION" RFL:LPROFLIST)) "DIRRIGHT")
               (setq DIRECTIONT -1 DIRECTIONS -1)
              )
              ((= (cdr (assoc "DIRECTION" RFL:LPROFLIST)) "DIRUP")
               (setq DIRECTIONT -1 DIRECTIONS -1)
              )
              ((= (cdr (assoc "DIRECTION" RFL:LPROFLIST)) "DIRDOWN")
               (setq DIRECTIONT 1 DIRECTIONS -1)
              )
        )
       )
       (if (= DIRECTIONT 1)
        (progn
         (if (= (cdr (assoc "RAB" RFL:LPROFLIST)) 0)
          (progn
           (setq MLMR "MR")
           (setq MLMRT "R")
           (setq MLMRB "TR")
           (setq SIDE 1)
          )
          (progn
           (setq MLMR "ML")
           (setq MLMRT "L")
           (setq MLMRB "TL")
           (setq SIDE -1)
          )
         )
        )
        (progn
         (if (= (cdr (assoc "RAB" RFL:LPROFLIST)) 0)
          (progn
           (setq MLMR "ML")
           (setq MLMRT "L")
           (setq MLMRB "TL")
           (setq SIDE 1)
          )
          (progn
           (setq MLMR "MR")
           (setq MLMRT "R")
           (setq MLMRB "TR")
           (setq SIDE -1)
          )
         )
        )
       )
       (setq ZMAX (nth 1 (nth 0 PVILIST)))
       (setq ZMIN (nth 1 (nth 0 PVILIST)))
       (setq C 1)
       (while (< C (length PVILIST))
        (if (> (nth 1 (nth C PVILIST)) ZMAX)
         (setq ZMAX (nth 1 (nth C PVILIST)))
        )
        (if (< (nth 1 (nth C PVILIST)) ZMIN)
         (setq ZMIN (nth 1 (nth C PVILIST)))
        )
        (setq C (+ C 1))
       )
       (setq C 1)
       (setq NLAYER (cdr (assoc "TEXTLAYER" RFL:LPROFLIST)))
       (if (= "*" (substr NLAYER 1 1)) (setq NLAYER (strcat CLAYER (substr NLAYER 2))))
       (if (not (tblsearch "LAYER" NLAYER))
        (entmake (list (cons 0 "LAYER")
                       (cons 100 "AcDbSymbolTableRecord")
                       (cons 100 "AcDbLayerTableRecord")
                       (cons 2 NLAYER)
                       (cons 70 0)
                 )
        )
       )
       (setvar "CLAYER" NLAYER)
       (setq STA1 (nth 0 (nth 0 PVILIST)))
       (setq Z1 (nth 1 (nth 0 PVILIST)))
       (setq L1 (nth 3 (nth 0 PVILIST)))
       (setq P (RFL:PROFPOINT STA1 Z1))
       (if (= (cdr (assoc "CNODES" RFL:LPROFLIST)) 1)
        (progn
         (command "._CIRCLE" P (cdr (assoc "CNODERAD" RFL:LPROFLIST)))
         (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
         (command "._LINE"
                  (list (nth 0 P) (+ (nth 1 P) (* SIDE (cdr (assoc "CNODERAD" RFL:LPROFLIST)))))
                  (list (nth 0 P) (+ (nth 1 P) (* SIDE (+ (cdr (assoc "CNODERAD" RFL:LPROFLIST)) (cdr (assoc "CNODELENGTH" RFL:LPROFLIST))))))
                  ""
         )
         (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
        )
       )
       (if (= (cdr (assoc "LBVC" RFL:LPROFLIST)) 1)
        (progn
         (setq TMP (RFL:STATXT STA1))
         (if (= (cdr (assoc "LHIGH" RFL:LPROFLIST)) 0)
          (setq TMP (substr TMP (1+ (vl-string-search "+" TMP))))
         )
         (ADDTEXT (strcat "STA " TMP)
                  (list (+ (nth 0 P) (* DIRECTIONT (* 1.0 (cdr (assoc "CNODEHOFFSET" RFL:LPROFLIST)))))
                        (+ (nth 1 P) (* SIDE (+ (cdr (assoc "CNODERAD" RFL:LPROFLIST)) (cdr (assoc "CNODELENGTH" RFL:LPROFLIST)) (cdr (assoc "CNODEVOFFSET" RFL:LPROFLIST)))))
                  )
                  (cdr (assoc "TEXTHEIGHT" RFL:LPROFLIST))
                  MLMRT
                  (if (= DIRECTIONT 1)
                   (/ pi -2.0)
                   (/ pi 2.0)
                  )
         )
         (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
         (setq TMP (RFL:ROUND Z1 (cdr (assoc "ELEVPREC" RFL:LPROFLIST))))
         (ADDTEXT (strcat "PIVC  "
                          (rtos TMP 2 (cdr (assoc "ELEVPREC" RFL:LPROFLIST)))
                          " "
                          (cdr (assoc "UNITS" RFL:LPROFLIST))
                  )
                  (list (+ (nth 0 P) (* DIRECTIONT (* -1.0 (cdr (assoc "CNODEHOFFSET" RFL:LPROFLIST)))))
                        (+ (nth 1 P) (* SIDE (+ (cdr (assoc "CNODERAD" RFL:LPROFLIST)) (cdr (assoc "CNODELENGTH" RFL:LPROFLIST)) (cdr (assoc "CNODEVOFFSET" RFL:LPROFLIST)))))
                  )
                  (cdr (assoc "TEXTHEIGHT" RFL:LPROFLIST))
                  MLMRB
                  (if (= DIRECTIONT 1)
                   (/ pi -2.0)
                   (/ pi 2.0)
                  )
         )
         (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
        )
       )
       (while (< (+ C 1) (length PVILIST))
        (setq STA1 (nth 0 (nth (- C 1) PVILIST)))
        (setq Z1 (nth 1 (nth (- C 1) PVILIST)))
        (setq L1 (nth 3 (nth (- C 1) PVILIST)))
        (setq STA2 (nth 0 (nth C PVILIST)))
        (setq Z2 (nth 1 (nth C PVILIST)))
        (setq L2 (nth 3 (nth C PVILIST)))
        (setq STA3 (nth 0 (nth (+ C 1) PVILIST)))
        (setq Z3 (nth 1 (nth (+ C 1) PVILIST)))
        (setq L3 (nth 3 (nth (+ C 1) PVILIST)))
        (setq G1 (* (/ (- Z2 Z1) (- STA2 STA1)) 100.0))
        (setq G2 (* (/ (- Z3 Z2) (- STA3 STA2)) 100.0))
        (setq STA (/ (+ (+ STA1
                           (/ L1 2.0)
                        )
                        (- STA2
                           (/ L2 2.0)
                        )
                     )
                     2.0
                  )
        )
        (if (= (cdr (assoc "LSLOPE" RFL:LPROFLIST)) 1)
         (progn
          (ADDTEXT (strcat (if (> G1 0.0) "+" "") (rtos G1 2 (cdr (assoc "SLOPEPREC" RFL:LPROFLIST))) "%")
                   (list (+ (car (RFL:PROFPOINT STA (RFL:ELEVATION STA)))
                            (* 1.75
                               SIDE
                               (if (= (cdr (assoc "DIRECTION" RFL:PROFDEFLIST)) -1) -1 1)
                               (sin (angle (RFL:PROFPOINT STA2 Z2) (RFL:PROFPOINT STA1 Z1)))
                            )
                         )
                         (+ (cadr (RFL:PROFPOINT STA (RFL:ELEVATION STA)))
                            (* -1.75
                               SIDE
                               (if (= (cdr (assoc "DIRECTION" RFL:PROFDEFLIST)) -1) -1 1)
                               (cos (angle (RFL:PROFPOINT STA2 Z2) (RFL:PROFPOINT STA1 Z1)))
                            )
                         )
                   )
                   (cdr (assoc "TEXTHEIGHT" RFL:LPROFLIST))
                   (if (= SIDE 1) "C" "TC")
                   (if (= DIRECTIONS 1)
                    (angle (RFL:PROFPOINT STA2 Z2) (RFL:PROFPOINT STA1 Z1))
                    (angle (RFL:PROFPOINT STA1 Z1) (RFL:PROFPOINT STA2 Z2))
                   )
          )
          (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
         )
        )
        (setq P (RFL:PROFPOINT STA2 Z2))
        (if (= (cdr (assoc "LPVI" RFL:LPROFLIST)) 1)
         (progn
          (setq TMP (RFL:STATXT STA2))
          (if (= (cdr (assoc "LHIGH" RFL:LPROFLIST)) 0)
           (setq TMP (substr TMP (1+ (vl-string-search "+" TMP))))
          )
          (ADDTEXT (strcat "STA  " TMP)
                   (list (+ (nth 0 P) (* DIRECTIONT (* 1.0 (cdr (assoc "CNODEHOFFSET" RFL:LPROFLIST)))))
                         (+ (nth 1 P) (* SIDE (+ (cdr (assoc "CNODERAD" RFL:LPROFLIST)) (cdr (assoc "CNODELENGTH" RFL:LPROFLIST)) (cdr (assoc "CNODEVOFFSET" RFL:LPROFLIST)))))
                   )
                   (cdr (assoc "TEXTHEIGHT" RFL:LPROFLIST))
                   MLMRT
                  (if (= DIRECTIONT 1)
                   (/ pi -2.0)
                   (/ pi 2.0)
                  )
          )
          (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
          (setq TMP (RFL:ROUND Z2 (cdr (assoc "ELEVPREC" RFL:LPROFLIST))))
          (ADDTEXT (strcat "PIVC  "
                           (rtos TMP 2 (cdr (assoc "ELEVPREC" RFL:LPROFLIST)))
                           " "
                           (cdr (assoc "UNITS" RFL:LPROFLIST))
                   )
                   (list (+ (nth 0 P) (* DIRECTIONT (* -1.0 (cdr (assoc "CNODEHOFFSET" RFL:LPROFLIST)))))
                         (+ (nth 1 P) (* SIDE (+ (cdr (assoc "CNODERAD" RFL:LPROFLIST)) (cdr (assoc "CNODELENGTH" RFL:LPROFLIST)) (cdr (assoc "CNODEVOFFSET" RFL:LPROFLIST)))))
                   )
                   (cdr (assoc "TEXTHEIGHT" RFL:LPROFLIST))
                   MLMRB
                  (if (= DIRECTIONT 1)
                   (/ pi -2.0)
                   (/ pi 2.0)
                  )
          )
          (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
         )
        )
        (if (= (cdr (assoc "DPVI" RFL:LPROFLIST)) 1)
         (progn
          (command "._CIRCLE" (RFL:PROFPOINT STA2 Z2) (cdr (assoc "CNODERAD" RFL:LPROFLIST)))
          (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
          (command "._LINE"
                   (list (nth 0 (RFL:PROFPOINT STA2 Z2)) (+ (nth 1 (RFL:PROFPOINT STA2 Z2)) (* SIDE (cdr (assoc "CNODERAD" RFL:LPROFLIST)))))
                   (list (nth 0 (RFL:PROFPOINT STA2 Z2)) (+ (nth 1 (RFL:PROFPOINT STA2 Z2)) (* SIDE (+ (cdr (assoc "CNODERAD" RFL:LPROFLIST)) (cdr (assoc "CNODELENGTH" RFL:LPROFLIST))))))
                   ""
          )
          (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
         )
        )
        (if (> L2 0.0)
         (progn
          (setq K (abs (/ L2
                          (- G2 G1)
                       )
                  )
          )
          (if (= (cdr (assoc "LL" RFL:LPROFLIST)) 1)
           (progn
            (setq TMP (RFL:ROUND L2 (cdr (assoc "LPREC" RFL:LPROFLIST))))
            (ADDTEXT (strcat (rtos TMP 2 (cdr (assoc "LPREC" RFL:LPROFLIST))) " VC")
                     (list (nth 0 P) (- (nth 1 P) (* SIDE 50.0)))
                     (cdr (assoc "TEXTHEIGHT" RFL:LPROFLIST))
                     "TC"
                    0.0
            )
            (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
           )
          )
          (if (= (cdr (assoc "LK" RFL:LPROFLIST)) 1)
           (progn
            (setq TMP (RFL:ROUND K (cdr (assoc "KPREC" RFL:LPROFLIST))))
            (ADDTEXT (strcat "K = " (rtos TMP 2 (cdr (assoc "KPREC" RFL:LPROFLIST))))
                     (list (nth 0 P) (- (nth 1 P) (* SIDE 50.0) 5.25))
                     (cdr (assoc "TEXTHEIGHT" RFL:LPROFLIST))
                     "TC"
                    0.0
            )
            (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
           )
          )
          (if (= (cdr (assoc "DPVI" RFL:LPROFLIST)) 1)
           (progn
            (setq ANG (angle P (RFL:PROFPOINT STA1 Z1)))
            (command "._LINE"
                     (list (+ (nth 0 P)
                              (* (cdr (assoc "CNODERAD" RFL:LPROFLIST))
                                 (cos ANG)
                              )
                           )
                           (+ (nth 1 P)
                              (* (cdr (assoc "CNODERAD" RFL:LPROFLIST))
                                 (sin ANG)
                              )
                           )
                     )
                     (list (+ (nth 0 P)
                              (* (+ (cdr (assoc "CNODERAD" RFL:LPROFLIST)) (cdr (assoc "CNODELENGTH" RFL:LPROFLIST)))
                                 (cos ANG)
                              )
                           )
                           (+ (nth 1 P)
                              (* (+ (cdr (assoc "CNODERAD" RFL:LPROFLIST)) (cdr (assoc "CNODELENGTH" RFL:LPROFLIST)))
                                 (sin ANG)
                              )
                           )
                     )
                     ""
            )
            (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
            (setq ANG (angle P (RFL:PROFPOINT STA3 Z3)))
            (command "._LINE"
                     (list (+ (nth 0 P)
                              (* (cdr (assoc "CNODERAD" RFL:LPROFLIST))
                                 (cos ANG)
                              )
                           )
                           (+ (nth 1 P)
                              (* (cdr (assoc "CNODERAD" RFL:LPROFLIST))
                                 (sin ANG)
                              )
                           )
                     )
                     (list (+ (nth 0 P)
                              (* (+ (cdr (assoc "CNODERAD" RFL:LPROFLIST)) (cdr (assoc "CNODELENGTH" RFL:LPROFLIST)))
                                 (cos ANG)
                              )
                           )
                           (+ (nth 1 P)
                              (* (+ (cdr (assoc "CNODERAD" RFL:LPROFLIST)) (cdr (assoc "CNODELENGTH" RFL:LPROFLIST)))
                                 (sin ANG)
                              )
                           )
                     )
                     ""
            )
            (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
           )
          )
          (setq P (RFL:PROFPOINT (- STA2 (/ L2 2.0)) (RFL:ELEVATION (- STA2 (/ L2 2.0)))))
          (if (= (cdr (assoc "LBVC" RFL:LPROFLIST)) 1)
           (progn
            (setq TMP (RFL:ROUND (RFL:ELEVATION (- STA2 (/ L2 2.0))) (cdr (assoc "ELEVPREC" RFL:LPROFLIST))))
            (ADDTEXT (strcat "BVC  "
                             (rtos TMP 2 (cdr (assoc "ELEVPREC" RFL:LPROFLIST)))
                             " "
                             (cdr (assoc "UNITS" RFL:LPROFLIST))
                     )
                     (list (nth 0 P)
                           (+ (nth 1 P) (* SIDE (+ (cdr (assoc "CNODERAD" RFL:LPROFLIST)) (cdr (assoc "CNODELENGTH" RFL:LPROFLIST)) (cdr (assoc "CNODEVOFFSET" RFL:LPROFLIST)))))
                     )
                     (cdr (assoc "TEXTHEIGHT" RFL:LPROFLIST))
                     MLMR
                     (if (= DIRECTIONT 1)
                      (/ pi -2.0)
                      (/ pi 2.0)
                     )
            )
            (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
           )
          )
          (if (= (cdr (assoc "CNODES" RFL:LPROFLIST)) 1)
           (progn
            (command "._CIRCLE" P (cdr (assoc "CNODERAD" RFL:LPROFLIST)))
            (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
            (command "._LINE"
                     (list (nth 0 P) (+ (nth 1 P) (* SIDE (cdr (assoc "CNODERAD" RFL:LPROFLIST)))))
                     (list (nth 0 P) (+ (nth 1 P) (* SIDE (+ (cdr (assoc "CNODERAD" RFL:LPROFLIST)) (cdr (assoc "CNODELENGTH" RFL:LPROFLIST))))))
                     ""
            )
            (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
           )
          )
          (setq P (RFL:PROFPOINT (+ STA2 (/ L2 2.0)) (RFL:ELEVATION (+ STA2 (/ L2 2.0)))))
          (if (= (cdr (assoc "LBVC" RFL:LPROFLIST)) 1)
           (progn
            (setq TMP (RFL:ROUND (RFL:ELEVATION (+ STA2 (/ L2 2.0))) (cdr (assoc "ELEVPREC" RFL:LPROFLIST))))
            (ADDTEXT (strcat "EVC  "
                             (rtos TMP 2 (cdr (assoc "ELEVPREC" RFL:LPROFLIST)))
                             " "
                             (cdr (assoc "UNITS" RFL:LPROFLIST))
                     )
                     (list (nth 0 P)
                           (+ (nth 1 P) (* SIDE (+ (cdr (assoc "CNODERAD" RFL:LPROFLIST)) (cdr (assoc "CNODELENGTH" RFL:LPROFLIST)) (cdr (assoc "CNODEVOFFSET" RFL:LPROFLIST)))))
                     )
                     (cdr (assoc "TEXTHEIGHT" RFL:LPROFLIST))
                     MLMR
                     (if (= DIRECTIONT 1)
                      (/ pi -2.0)
                      (/ pi 2.0)
                     )
            )
            (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
           )
          )
          (if (= (cdr (assoc "CNODES" RFL:LPROFLIST)) 1)
           (progn
            (command "._CIRCLE" P (cdr (assoc "CNODERAD" RFL:LPROFLIST)))
            (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
            (command "._LINE"
                     (list (nth 0 P) (+ (nth 1 P) (* SIDE (cdr (assoc "CNODERAD" RFL:LPROFLIST)))))
                     (list (nth 0 P) (+ (nth 1 P) (* SIDE (+ (cdr (assoc "CNODERAD" RFL:LPROFLIST)) (cdr (assoc "CNODELENGTH" RFL:LPROFLIST))))))
                     ""
            )
            (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
           )
          )
          (if (= (cdr (assoc "LELEVATIONSC" RFL:LPROFLIST)) 1)
           (progn
            (setq STA (float (* (+ (fix (/ (- STA2 (/ L2 2.0)) (cdr (assoc "ELEVTEXTINC" RFL:LPROFLIST)))) 1) (cdr (assoc "ELEVTEXTINC" RFL:LPROFLIST)))))
            (while (< STA (+ STA2 (/ L2 2.0)))
             (setq P (RFL:PROFPOINT STA (cdr (assoc "ELEV" RFL:PROFDEFLIST))))
             (setq P (list (car P)
                           (+ (cadr P)
                              (cdr (assoc "ELEVTEXTOS" RFL:LPROFLIST))
                           )
                     )
             )
             (setq TMP (RFL:ROUND (RFL:ELEVATION STA) (cdr (assoc "ELEVPREC" RFL:LPROFLIST))))
             (ADDTEXT (rtos TMP 2 (cdr (assoc "ELEVPREC" RFL:LPROFLIST)))
                      P
                      (cdr (assoc "ELEVTEXTHEIGHT" RFL:LPROFLIST))
                      MLMR
                      (if (= DIRECTIONT 1)
                       (/ pi -2.0)
                       (/ pi 2.0)
                      )
             )
             (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
             (setq STA (+ STA (cdr (assoc "ELEVTEXTINC" RFL:LPROFLIST))))
            )
           )
          )
          (if (= (cdr (assoc "LELEVATIONST" RFL:LPROFLIST)) 1)
           (progn
            (setq STA (float (* (+ (fix (/ (+ STA1 (/ L1 2.0)) (cdr (assoc "ELEVTEXTINC" RFL:LPROFLIST)))) 1) (cdr (assoc "ELEVTEXTINC" RFL:LPROFLIST)))))
            (while (< STA (- STA2 (/ L2 2.0)))
             (setq P (RFL:PROFPOINT STA (cdr (assoc "ELEV" RFL:PROFDEFLIST))))
             (setq P (list (car P)
                           (+ (cadr P)
                              (cdr (assoc "ELEVTEXTOS" RFL:LPROFLIST))
                           )
                     )
             )
             (setq TMP (RFL:ROUND (RFL:ELEVATION STA) (cdr (assoc "ELEVPREC" RFL:LPROFLIST))))
             (ADDTEXT (rtos TMP 2 (cdr (assoc "ELEVPREC" RFL:LPROFLIST)))
                      P
                      (cdr (assoc "ELEVTEXTHEIGHT" RFL:LPROFLIST))
                      MLMR
                      (if (= DIRECTIONT 1)
                       (/ pi -2.0)
                       (/ pi 2.0)
                      )
             )
             (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
             (setq STA (+ STA (cdr (assoc "ELEVTEXTINC" RFL:LPROFLIST))))
            )
           )
          )
         )
        )
        (setq C (+ C 1))
       )
       (setq STA (/ (+ (+ STA2
                          (/ L2 2.0)
                       )
                       (- STA3
                          (/ L3 2.0)
                       )
                    )
                    2.0
                 )
       )
       (if (= (cdr (assoc "LSLOPE" RFL:LPROFLIST)) )
        (progn
         (ADDTEXT (strcat (if (> G2 0.0) "+" "") (rtos G2 2 (cdr (assoc "SLOPEPREC" RFL:LPROFLIST))) "%")
                  (list (+ (car (RFL:PROFPOINT STA (RFL:ELEVATION STA)))
                           (* 1.75
                              SIDE
                              (if (= (cdr (assoc "DIRECTION" RFL:PROFDEFLIST)) -1) -1 1)
                              (sin (angle (RFL:PROFPOINT STA3 Z3) (RFL:PROFPOINT STA2 Z2)))
                           )
                        )
                        (+ (cadr (RFL:PROFPOINT STA (RFL:ELEVATION STA)))
                           (* -1.75
                              SIDE
                              (if (= (cdr (assoc "DIRECTION" RFL:PROFDEFLIST)) -1) -1 1)
                              (cos (angle (RFL:PROFPOINT STA2 Z2) (RFL:PROFPOINT STA1 Z1)))
                           )
                        )
                  )
                  (cdr (assoc "TEXTHEIGHT" RFL:LPROFLIST))
                  (if (= SIDE 1) "C" "TC")
                  (if (= DIRECTIONS 1)
                   (angle (RFL:PROFPOINT STA3 Z3) (RFL:PROFPOINT STA2 Z2))
                   (angle (RFL:PROFPOINT STA2 Z2) (RFL:PROFPOINT STA3 Z3))
                  )
         )
         (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
        )
       )
       (setq P (RFL:PROFPOINT STA3 Z3))
       (if (= (cdr (assoc "CNODES" RFL:LPROFLIST)) 1)
        (progn
         (command "._CIRCLE" P (cdr (assoc "CNODERAD" RFL:LPROFLIST)))
         (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
         (command "._LINE"
                  (list (nth 0 P) (+ (nth 1 P) (* SIDE (cdr (assoc "CNODERAD" RFL:LPROFLIST)))))
                  (list (nth 0 P) (+ (nth 1 P) (* SIDE (+ (cdr (assoc "CNODERAD" RFL:LPROFLIST)) (cdr (assoc "CNODELENGTH" RFL:LPROFLIST))))))
                  ""
         )
         (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
        )
       )
       (if (= (cdr (assoc "LBVC" RFL:LPROFLIST)) 1)
        (progn
         (setq TMP (RFL:STATXT STA3))
         (if (= (cdr (assoc "LHIGH" RFL:LPROFLIST)) 0)
          (setq TMP (substr TMP (1+ (vl-string-search "+" TMP))))
         )
         (ADDTEXT (strcat "STA  " TMP)
                  (list (+ (nth 0 P) (* DIRECTIONT (* 1.0 (cdr (assoc "CNODEHOFFSET" RFL:LPROFLIST)))))
                        (+ (nth 1 P) (* SIDE (+ (cdr (assoc "CNODERAD" RFL:LPROFLIST)) (cdr (assoc "CNODELENGTH" RFL:LPROFLIST)) (cdr (assoc "CNODEVOFFSET" RFL:LPROFLIST)))))
                  )
                  (cdr (assoc "TEXTHEIGHT" RFL:LPROFLIST))
                  MLMRT
                  (if (= DIRECTIONT 1)
                   (/ pi -2.0)
                   (/ pi 2.0)
                  )
         )
         (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
         (setq TMP (RFL:ROUND Z3 (cdr (assoc "SLOPEPREC" RFL:LPROFLIST))))
         (ADDTEXT (strcat "PIVC  "
                          (rtos TMP 2 (cdr (assoc "ELEVPREC" RFL:LPROFLIST)))
                          " "
                          (cdr (assoc "UNITS" RFL:LPROFLIST))
                  )
                  (list (+ (nth 0 P) (* DIRECTIONT (* -1.0 (cdr (assoc "CNODEHOFFSET" RFL:LPROFLIST)))))
                        (+ (nth 1 P) (* SIDE (+ (cdr (assoc "CNODERAD" RFL:LPROFLIST)) (cdr (assoc "CNODELENGTH" RFL:LPROFLIST)) (cdr (assoc "CNODEVOFFSET" RFL:LPROFLIST)))))
                  )
                  (cdr (assoc "TEXTHEIGHT" RFL:LPROFLIST))
                  MLMRB
                  (if (= DIRECTIONT 1)
                   (/ pi -2.0)
                   (/ pi 2.0)
                  )
         )
         (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
        )
       )
       (if (= (cdr (assoc "LELEVATIONSC" RFL:LPROFLIST)) 1)
        (progn
         (setq STA (float (* (+ (fix (/ (+ STA2 (/ L2 2.0)) (cdr (assoc "ELEVTEXTINC" RFL:LPROFLIST)))) 1) (cdr (assoc "ELEVTEXTINC" RFL:LPROFLIST)))))
         (while (< STA (- STA3 (/ L3 2.0)))
          (setq P (RFL:PROFPOINT STA (cdr (assoc "ELEV" RFL:PROFDEFLIST))))
          (setq P (list (car P)
                        (+ (cadr P)
                           (cdr (assoc "ELEVTEXTOS" RFL:LPROFLIST))
                        )
                  )
          )
          (setq TMP (RFL:ROUND (RFL:ELEVATION STA) (cdr (assoc "ELEVPREC" RFL:LPROFLIST))))
          (ADDTEXT (rtos TMP 2 (cdr (assoc "ELEVPREC" RFL:LPROFLIST)))
                   P
                   (cdr (assoc "ELEVTEXTHEIGHT" RFL:LPROFLIST))
                   MLMR
                   (if (= DIRECTIONT 1)
                    (/ pi -2.0)
                    (/ pi 2.0)
                   )
          )
          (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
          (setq STA (+ STA (cdr (assoc "ELEVTEXTINC" RFL:LPROFLIST))))
         )
        )
       )
      )
     )
    )
   )
  )
 )

 (setvar "REGENMODE" REGENMODE)
 (setvar "CMDECHO" CMDECHO)
 (setvar "DIMZIN" DIMZIN)
 (setvar "LUPREC" LUPREC)
 (setvar "CLAYER" CLAYER)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
 (setvar "OSMODE" OSMODE)
 (setvar "SPLINETYPE" SPLINETYPE)
 (setvar "SPLINESEGS" SPLINESEGS)
 (setvar "PLINETYPE" PLINETYPE)
 (setvar "TEXTSTYLE" TEXTSTYLE)
)