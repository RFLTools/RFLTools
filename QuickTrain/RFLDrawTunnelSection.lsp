;
;
;     Program written by Robert Livingston, 2013-05-01
;
;     RFL:DRAWTUNNELSECTION is a utility to draw a train section using survey shots
;
;
(defun RFL:DRAWTUNNELSECTION (PBASE STA STALIST PLIST SECTYPE / *error* ANGBASE ANGDIR ATTREQ BASEOS BASEZ C CLAYER CMDECHO DIN DOUT INSERTCONTROL MAXERR NODE ORTHOMODE OSMODE P PC PT R RDES SETLAYER ST TMP TOL ZT)
;  SECTYPE:
;        0 = Points Only
;        1 = Circular
;        2 = 4 Point (top/bottom/left/right) Square
;
 (setq TOL 0.00000001)
 (command "._UNDO" "M")
 (setq CLAYER (getvar "CLAYER"))
 (setq ATTREQ (getvar "ATTREQ"))
 (setvar "ATTREQ" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0.0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 1)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq OSMODE (getvar "OSMODE"))
 (setvar "OSMODE" 0)
 (setq ORTHOMODE (getvar "ORTHOMODE"))
 (setvar "ORTHOMODE" 0)

 (defun *error* (msg)
  (setvar "ATTREQ" ATTREQ)
  (setvar "ANGBASE" ANGBASE)
  (setvar "ANGDIR" ANGDIR)
  (setvar "CLAYER" CLAYER)
  (setvar "CMDECHO" CMDECHO)
  (setvar "OSMODE" OSMODE)
  (setvar "ORTHOMODE" ORTHOMODE)
  (print msg)
 )

 (defun SETLAYER (LNAME / LNAMESTR)
  (setq LNAMESTR (strcat CLAYER (cdr (assoc LNAME RFL:QUICKTRAINDEF))))
  (if (= nil (tblsearch "LAYER" LNAMESTR))
   (command "._LAYER" "M" LNAMESTR "")
  )
  (setvar "CLAYER" LNAMESTR)
 )
 
 (defun INSERTCONTROL (P1 P2 / ENT ENT2 ENTLIST P)
  (setq P (list (+ (car PBASE) (- (car P2) BASEOS))
                (+ (cadr PBASE) (- (cadr P2) BASEZ))))
  (command "._INSERT" "SECT-REF" P "1.0" "1.0" "0.0")
  (setq ENT (entlast))
  (setq ENT2 (entnext ENT))
  (setq ENTLIST (entget ENT2))
  (while (/= "SEQEND" (cdr (assoc 0 ENTLIST)))
   (cond ((= "E" (cdr (assoc 2 ENTLIST)))
          (progn
           (setq ENTLIST (subst (cons 1 (rtos (car P1))) (assoc 1 ENTLIST) ENTLIST))
           (entmod ENTLIST)
          )
         )
         ((= "N" (cdr (assoc 2 ENTLIST)))
          (progn
           (setq ENTLIST (subst (cons 1 (rtos (cadr P1))) (assoc 1 ENTLIST) ENTLIST))
           (entmod ENTLIST)
          )
         )
         ((= "Z" (cdr (assoc 2 ENTLIST)))
          (progn
           (setq ENTLIST (subst (cons 1 (rtos (caddr P1))) (assoc 1 ENTLIST) ENTLIST))
           (entmod ENTLIST)
          )
         )
   )
   (setq ENT2 (entnext ENT2))
   (setq ENTLIST (entget ENT2))
  )
  (entupd ENT)
 )
 
 (if (= nil RFL:QUICKTRAINDEF) (RFL:SETQUICKTRAINDEF))

 (if (and (= SECTYPE 1) (< (length STALIST) 3))
  (setq SECTYPE 0)
  (if (and (= SECTYPE 2) (/= (length STALIST) 4))
   (setq SECTYPE 0)
  )
 )
 (cond ((= 0 SECTYPE)
        (progn
         (setq BASEOS 0.0)
         (setq BASEZ 0.0)
         (foreach NODE STALIST
          (progn
           (setq BASEOS (+ BASEOS (car NODE)))
           (setq BASEZ (+ BASEZ (cadr NODE)))
          )
         )
         (setq BASEOS (/ BASEOS (length STALIST)))
         (setq BASEZ (/ BASEZ (length STALIST)))
         (setq P (RFL:XY (list (car STA) BASEOS)))
;         (SETLAYER "DLAYERS")
;         (INSERTCONTROL (list (car P) (cadr P) BASEZ) (list BASEOS BASEZ))
         (setq PT (RFL:XY (list (car STA) 0.0)))
         (setq ZT (RFL:ELEVATION (car STA)))
         (setq ST (RFL:SUPER (car STA)))
         (setq RDES (RFL:GETRADIUS (car STA)))
         (setq DIN nil DOUT nil)
         (if (and (/= nil RDES)
                  (tblsearch "BLOCK" (cdr (assoc "SECTION" RFL:QUICKTRAINDEF)))
                  (tblsearch "BLOCK" (cdr (assoc "LSECTION" RFL:QUICKTRAINDEF)))
                  (tblsearch "BLOCK" (cdr (assoc "RSECTION" RFL:QUICKTRAINDEF)))
             )
          (setq DIN (RFL:INSWING (cdr (assoc "WB" RFL:QUICKTRAINDEF)) (abs RDES))
                DOUT (RFL:OUTSWING (cdr (assoc "WB" RFL:QUICKTRAINDEF)) (cdr (assoc "OH" RFL:QUICKTRAINDEF)) (abs RDES))
          )
         )
         (if (= nil ST) (setq ST (list 0.0 0.0)))
         (if (and (/= nil PT) (/= nil ZT))
          (INSERTCONTROL (list (car PT) (cadr PT) ZT) (list 0.0 ZT))
         )
         (setvar "CLAYER" CLAYER)
         (RFL:SPOT2SECTION PBASE (car P) (cadr P) BASEZ)
         (SETLAYER "LLAYERS")
         (setq C 0)
         (SETLAYER "DLAYERS")
         (while (< C (length PLIST))
          (INSERTCONTROL (nth C PLIST) (nth C STALIST))
          (setq C (+ C 1))
         )
         (SETLAYER "TLAYERS")
         (command "._MTEXT" (list (+ (car PBASE) (cdr (assoc "LABELOSX" RFL:QUICKTRAINDEF)))
                                  (+ (cadr PBASE) (cdr (assoc "LABELOSY" RFL:QUICKTRAINDEF))))
                                  "H" (cdr (assoc "LTITLEH" RFL:QUICKTRAINDEF))"J" "TL" "W" 0.0
                  (strcat "{\\H" (rtos (cdr (assoc "LTITLEH" RFL:QUICKTRAINDEF)) 2 8) ";Station : " (RFL:STATXT (car STA)) "\\P\\P}"
                          "{\\H" (rtos (cdr (assoc "LTEXTH" RFL:QUICKTRAINDEF)) 2 8) ";Number of Shots Found : " (itoa (length PLIST)) "\\P}"
                          "{\\H" (rtos (cdr (assoc "LTEXTH" RFL:QUICKTRAINDEF)) 2 8) ";Sta Min : " (RFL:STATXT (cadr STA)) "\\P}"
                          "{\\H" (rtos (cdr (assoc "LTEXTH" RFL:QUICKTRAINDEF)) 2 8) ";Sta Max : " (RFL:STATXT (caddr STA)) "\\P}"
                          "{\\H" (rtos (cdr (assoc "LTEXTH" RFL:QUICKTRAINDEF)) 2 8) ";Sta Delta : " (rtos (- (caddr STA) (cadr STA)) 2 3) "\\P\\P}"
                          "{\\H" (rtos (cdr (assoc "LTEXTH" RFL:QUICKTRAINDEF)) 2 8) ";Design Northing : " (rtos (cadr PT) 2 3) "\\P}"
                          "{\\H" (rtos (cdr (assoc "LTEXTH" RFL:QUICKTRAINDEF)) 2 8) ";Design Easting : " (rtos (car PT) 2 3) "\\P}"
                          "{\\H" (rtos (cdr (assoc "LTEXTH" RFL:QUICKTRAINDEF)) 2 8) ";Design Elevation : " (rtos ZT 2 3) "\\P}"
                          "{\\H" (rtos (cdr (assoc "LTEXTH" RFL:QUICKTRAINDEF)) 2 8) ";Design Super : " (rtos (car ST) 2 3) " Left, " (rtos (cadr ST) 2 3) " Right" "\\P}"
                          "{\\H" (rtos (cdr (assoc "LTEXTH" RFL:QUICKTRAINDEF)) 2 8) ";Design Radius : " (if (= nil RDES) "*UNDEFINED*" (if (< (abs RDES) TOL) "Tangent" (strcat (rtos (abs RDES) 2 3) (if (< RDES 0.0) " Right" " Left")))) "\\P\\P}"
                          "{\\H" (rtos (cdr (assoc "LTEXTH" RFL:QUICKTRAINDEF)) 2 8) ";Vehicle Inswing : " (if (= nil DIN) "*UNDEFINED*" (rtos DIN 2 3)) "\\P}"
                          "{\\H" (rtos (cdr (assoc "LTEXTH" RFL:QUICKTRAINDEF)) 2 8) ";Vehicle Outswing : " (if (= nil DOUT) "*UNDEFINED*" (rtos DOUT 2 3)) "}"
                  )
                  ""
         )
        )
       )
       ((= 1 SECTYPE)
        (progn
         (setq PC (RFL:BESTCIRCLE STALIST))
         (if (/= nil PC)
          (progn
           (setq BASEOS (caar PC))
           (setq BASEZ (cadar PC))
           (setq R (cadr PC))
           (setq MAXERR (caddr PC))
           (setq P (RFL:XY (list (car STA) BASEOS)))
           (SETLAYER "DLAYERS")
           (INSERTCONTROL (list (car P) (cadr P) BASEZ) (list BASEOS BASEZ))
           (setq PT (RFL:XY (list (car STA) 0.0)))
           (setq ZT (RFL:ELEVATION (car STA)))
           (setq ST (RFL:SUPER (car STA)))
           (setq RDES (RFL:GETRADIUS (car STA)))
           (setq DIN nil DOUT nil)
           (if (and (/= nil RDES)
                    (tblsearch "BLOCK" (cdr (assoc "SECTION" RFL:QUICKTRAINDEF)))
                    (tblsearch "BLOCK" (cdr (assoc "LSECTION" RFL:QUICKTRAINDEF)))
                    (tblsearch "BLOCK" (cdr (assoc "RSECTION" RFL:QUICKTRAINDEF)))
               )
            (setq DIN (RFL:INSWING (cdr (assoc "WB" RFL:QUICKTRAINDEF)) (abs RDES))
                  DOUT (RFL:OUTSWING (cdr (assoc "WB" RFL:QUICKTRAINDEF)) (cdr (assoc "OH" RFL:QUICKTRAINDEF)) (abs RDES))
            )
           )
           (if (= nil ST) (setq ST (list 0.0 0.0)))
           (if (and (/= nil PT) (/= nil ZT))
            (INSERTCONTROL (list (car PT) (cadr PT) ZT) (list 0.0 ZT))
           )
           (setvar "CLAYER" CLAYER)
           (RFL:SPOT2SECTION PBASE (car P) (cadr P) BASEZ)
           (SETLAYER "LLAYERS")
           (command "._CIRCLE" PBASE R)
           (setq C 0)
           (SETLAYER "DLAYERS")
           (while (< C (length PLIST))
            (INSERTCONTROL (nth C PLIST) (nth C STALIST))
            (setq C (+ C 1))
           )
           (SETLAYER "TLAYERS")
           (command "._MTEXT" (list (+ (car PBASE) (cdr (assoc "LABELOSX" RFL:QUICKTRAINDEF)))
                                    (+ (cadr PBASE) (cdr (assoc "LABELOSY" RFL:QUICKTRAINDEF))))
                                    "H" (cdr (assoc "LTITLEH" RFL:QUICKTRAINDEF)) "J" "TL" "W" 0.0
                    (strcat "{\\H" (rtos (cdr (assoc "LTITLEH" RFL:QUICKTRAINDEF)) 2 8) ";Station : " (RFL:STATXT (car STA)) "\\P\\P}"
                            "{\\H" (rtos (cdr (assoc "LTEXTH" RFL:QUICKTRAINDEF)) 2 8) ";Number of Shots Found : " (itoa (length PLIST)) "\\P}"
                            "{\\H" (rtos (cdr (assoc "LTEXTH" RFL:QUICKTRAINDEF)) 2 8) ";Sta Min : " (RFL:STATXT (cadr STA)) "\\P}"
                            "{\\H" (rtos (cdr (assoc "LTEXTH" RFL:QUICKTRAINDEF)) 2 8) ";Sta Max : " (RFL:STATXT (caddr STA)) "\\P}"
                            "{\\H" (rtos (cdr (assoc "LTEXTH" RFL:QUICKTRAINDEF)) 2 8) ";Sta Delta : " (rtos (- (caddr STA) (cadr STA)) 2 3) "\\P\\P}"
                            "{\\H" (rtos (cdr (assoc "LTEXTH" RFL:QUICKTRAINDEF)) 2 8) ";Tunnel Radius : " (rtos R 2 3) "\\P}"
                            "{\\H" (rtos (cdr (assoc "LTEXTH" RFL:QUICKTRAINDEF)) 2 8) ";Tunnel Radius Maximum Error : " (rtos MAXERR 2 3) "\\P\\P}"
                            "{\\H" (rtos (cdr (assoc "LTEXTH" RFL:QUICKTRAINDEF)) 2 8) ";Design Northing : " (rtos (cadr PT) 2 3) "\\P}"
                            "{\\H" (rtos (cdr (assoc "LTEXTH" RFL:QUICKTRAINDEF)) 2 8) ";Design Easting : " (rtos (car PT) 2 3) "\\P}"
                            "{\\H" (rtos (cdr (assoc "LTEXTH" RFL:QUICKTRAINDEF)) 2 8) ";Design Elevation : " (rtos ZT 2 3) "\\P}"
                            "{\\H" (rtos (cdr (assoc "LTEXTH" RFL:QUICKTRAINDEF)) 2 8) ";Design Super : " (rtos (car ST) 2 3) " Left, " (rtos (cadr ST) 2 3) " Right" "\\P}"
                            "{\\H" (rtos (cdr (assoc "LTEXTH" RFL:QUICKTRAINDEF)) 2 8) ";Design Radius : " (if (= nil RDES) "*UNDEFINED*" (if (< (abs RDES) TOL) "Tangent" (strcat (rtos (abs RDES) 2 3) (if (< RDES 0.0) " Right" " Left")))) "\\P\\P}"
                            "{\\H" (rtos (cdr (assoc "LTEXTH" RFL:QUICKTRAINDEF)) 2 8) ";Vehicle Inswing : " (if (= nil DIN) "*UNDEFINED*" (rtos DIN 2 3)) "\\P}"
                            "{\\H" (rtos (cdr (assoc "LTEXTH" RFL:QUICKTRAINDEF)) 2 8) ";Vehicle Outswing : " (if (= nil DOUT) "*UNDEFINED*" (rtos DOUT 2 3)) "}"
                    )
                    ""
           )
          )
         )
        )
       )
       ((= 2 SECTYPE)
        (progn
        )
       )
 )
 
 (setvar "ATTREQ" ATTREQ)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
 (setvar "CLAYER" CLAYER)
 (setvar "CMDECHO" CMDECHO)
 (setvar "OSMODE" OSMODE)
 (setvar "ORTHOMODE" ORTHOMODE)
)