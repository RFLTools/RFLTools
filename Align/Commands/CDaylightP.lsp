;
;
;     Program written by Robert Livingston, 2017/09/07
;
;     DAYLIGHTP is a utility for 'Daylighting' from a specified point
;
;
(defun C:DAYLIGHTP (/ A AECCOBJ ANGBASE ANGDIR *error* C CANCEL CATCHLINE
                      CMDECHO CURRENTSURFACE ENT ENTLIST FLAG GETFROMLIST N N0 NODE NODE1 NODE2
                      OBSURFACE OGLINE OGOFFSETLIST OS OSCUT OSFILL OSMODE
                      P P1 P2 PD1 PD2 PLIST SA1 SA2 SLOPECUT SLOPEFILL STA SWATH V1 V2 Z ZCUT ZFILL)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0.0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 0)
 (setq DIMZIN (getvar "DIMZIN"))
 (setvar "DIMZIN" 0)
 (setq OSMODE (getvar "OSMODE"))
 (setvar "OSMODE" 0)

 (command "._UNDO" "M")

 (defun *error* (msg)
  (setvar "CMDECHO" CMDECHO)
  (setvar "OSMODE" OSMODE)
  (setvar "ANGBASE" ANGBASE)
  (setvar "ANGDIR" ANGDIR)
  (setvar "DIMZIN" DIMZIN)
  (princ msg)
  ;(setq *error* nil)
 )

 (defun GETSLOPEINT (OFFSET ELEV SLOPE OGLIST / C ELEV1 ELEV2 ELEV3 OFFSET1 OFFSET2 OFFSET3 TOL)
  (setq TOL 0.000001)
  (setq C 0)
  (setq OFFSET3 nil)
  (setq ELEV3 nil)
  (while (< C (- (length OGLIST) 1))
   (setq OFFSET1 (car (nth C OGLIST)))
   (setq ELEV1 (cadr (nth C OGLIST)))
   (setq OFFSET2 (car (nth (+ C 1) OGLIST)))
   (setq ELEV2 (cadr (nth (+ C 1) OGLIST)))
   (if (< (abs (- OFFSET2 OFFSET1)) TOL)
    (setq C (+ C 1))
    (progn
     (setq OFFSET3 (/ (- (+ (* OFFSET1
                               (/ (- ELEV2 ELEV1)
                                  (- OFFSET2 OFFSET1)
                               )
                            )
                            ELEV
                         )
                         (+ (/ OFFSET SLOPE) ELEV1)
                      )
                      (- (/ (- ELEV2 ELEV1)
                            (- OFFSET2 OFFSET1)
                         )
                         (/ 1.0 SLOPE)
                      )
                   )
     )
     (setq ELEV3 (+ ELEV
                    (/ (- OFFSET3 OFFSET)
                       SLOPE
                    )
                 )
     )
     (if (>= (* (- OFFSET3 OFFSET1) (- OFFSET2 OFFSET3)) 0.0)
      (setq C (length OGLIST))
      (setq C (+ C 1))
     )
    )
   )
  )
  (list OFFSET3 ELEV3)
 )

 (setq OBSURFACE (RFL:GETC3DSURFACE))
 
 (if (= nil OBSURFACE)
  (princ "\n***** ERROR WITH SURFACE *****")
  (progn
   (if (/= nil (setq P (RFL:GETSPOTPOINT (car (entsel "\nSelect spot elevation :")))))
    (progn
     (setq Z (caddr P))
     (setq P (list (car P) (cadr P)))
     (setq N 0)
     (while (< N 3)
      (if (= nil (setq N (getint "\nEnter number of radial points <60> : ")))
       (setq N 60)
       (if (< N 2) (princ "\nMust have at least 3 points!"))
      )
     )
     (setq SWATH 0.0)
     (while (<= SWATH 0.0)
      (if (= nil (setq SWATH (getdist "\nEnter swath distance <100.0> : ")))
       (setq SWATH 100.0)
       (if (<= SWATH 0.0) (princ "\nMust be greater then 0.0!"))
      )
     )
     (if (= nil (setq SLOPEFILL (getreal "\nEnter fill slope (X:1) <3.0> : ")))
      (setq SLOPEFILL 3.0)
     )
     (if (= nil (setq SLOPECUT (getreal "\nEnter cut slope (X:1) <2.0> : ")))
      (setq SLOPECUT 2.0)
     )
     (setq N0 0)
     (setq PLIST nil)
     (while (< N0 N)
      (setq A (/ (* N0 2 pi) N))
      (setq OGLINE nil)
      (setq P1 P)
      (setq Z1 Z)
      (setq P2 (list (+ (car P1) (* SWATH (cos A)))
                     (+ (cadr P1) (* SWATH (sin A)))
               )
      )
      (if (/= nil (setq OGLINE (RFL:GETSURFACELINE P1 P2 OBSURFACE)))
       (progn
        (setq OGOFFSETLIST nil)
        (foreach NODE OGLINE
         (setq OGOFFSETLIST (append OGOFFSETLIST (list (list (distance P1 (list (car NODE) (cadr NODE))) (caddr NODE)))))
        )
        (setq NODE1 (car OGOFFSETLIST)
              OGOFFSETLIST (cdr OGOFFSETLIST)
              NODE2 (car OGOFFSETLIST)
              OGOFFSETLIST (cdr OGOFFSETLIST)
        )
        (setq FLAG T)
        (setq P2 nil)
        (while (and FLAG NODE1 NODE2)
         (setq P2 (inters NODE1 NODE2 (list 0.0 Z1) (list SWATH (- Z1 (/ SWATH SLOPEFILL)))))
         (if (= nil P2)
          (setq P2 (inters NODE1 NODE2 (list 0.0 Z1) (list SWATH (+ Z1 (/ SWATH SLOPECUT)))))
         )
         (if (/= nil P2)
          (progn
           (setq FLAG nil)
           (setq PLIST (append PLIST (list (list (+ (car P1) (* (car P2) (cos A)))
                                                 (+ (cadr P1) (* (car P2) (sin A)))
                                                 (cadr P2)
                                           )
                                     )
                       )
           )
          )
         )
         (setq NODE1 NODE2
               NODE2 (car OGOFFSETLIST)
               OGOFFSETLIST (cdr OGOFFSETLIST)
         )
        )
       )
      )
      (setq N0 (1+ N0))
     )
     (if (/= nil PLIST)
      (progn
       (command "._3DPOLY")
       (foreach NODE PLIST (command NODE))
       (command "C")
      )
     )
    )
   )
  )
 )

 (setvar "CMDECHO" CMDECHO)
 (setvar "OSMODE" OSMODE)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
 (setvar "DIMZIN" DIMZIN)
 T
)