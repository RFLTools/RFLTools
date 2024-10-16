;
;
;     Program written by Robert Livingston, 2020-02-12
;
;     QLSECTIONPP is a utility for drawing point to point section based on (lidar) points
;
;     Note:  This command is a minor edit of QLSECTIONDB, any revisions should be mirrored between the two commands.
;
;     Math:
;     https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line
;
(defun C:QLSECTIONPP (/ ANG ANGBASE ATTREQ C C0 CMDECHO DISTANCE2D D DSTA ENT ENTLIST ENTSET NODE ORTHOMODE OSMODE P PBASE PS P1 P2 P3 P4 P5 P6 PLIST PLISTDB STA SWATH TMP ZBASE ZHEIGHT ZMIN ZMAX)
 (setq ATTREQ (getvar "ATTREQ"))
 (setvar "ATTREQ" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0.0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 1)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq OSMODE (getvar "OSMODE"))
 (setq ORTHOMODE (getvar "ORTHOMODE"))

 (defun *error* (msg)
  (setvar "ATTREQ" ATTREQ)
  (setvar "ANGBASE" ANGBASE)
  (setvar "ANGDIR" ANGDIR)
  (setvar "CMDECHO" CMDECHO)
  (setvar "OSMODE" OSMODE)
  (setvar "ORTHOMODE" ORTHOMODE)
  ;(setq *error* nil)
  (print msg)
 )

 (defun DISTANCE2D (P1 P2)
  (sqrt (+ (expt (- (car P2) (car P1)) 2) (expt (- (cadr P2) (cadr P1)) 2)))
 )

 (if (setq DBFILE (getfiled "Select a source .db file" "" "db" 2))
  (progn
   (if (= nil (setq DSTA (getdist "\nEnter delta station length (0.01) : "))) (setq DSTA 0.01))
   (while (setq P1 (getpoint "\nFirst point : "))
    (if (setq P2 (getpoint P1 "\nSecond point : "))
     (progn
	  (setq P1 (list (car P1) (cadr P1)))
	  (setq P2 (list (car P2) (cadr P2)))
	  (setq SWATH (distance P1 P2))
      (setq ANG (angle P1 P2))
      (setq PLISTDB nil)
      (setq P (list (/ (+ (car P1) (car P2)) 2.0)
                    (/ (+ (cadr P1) (cadr P2)) 2.0)
              )
      )
      (setq P3 (list (+ (car P) (* (/ SWATH 2.0) (sin ANG)))
                     (- (cadr P) (* (/ SWATH 2.0) (cos ANG)))
               )
      )
      (setq P4 (list (- (car P) (* (/ SWATH 2.0) (sin ANG)))
                     (+ (cadr P) (* (/ SWATH 2.0) (cos ANG)))
               )
      )
      (setq P5 (list (+ (car P1) (* (/ SWATH 2.0) (sin ANG)))
                     (- (cadr P1) (* (/ SWATH 2.0) (cos ANG)))
               )
      )
      (setq P6 (list (- (car P1) (* (/ SWATH 2.0) (sin ANG)))
                     (+ (cadr P1) (* (/ SWATH 2.0) (cos ANG)))
               )
      )
      (if (setq PLISTDB (RFL:GETPLISTDB P1 P2 P3 P4 DSTA SWATH DBFILE))
       (progn
        (setq PLIST nil)
        (setq ZMIN nil)
        (setq ZMAX nil)
        (foreach NODE PLISTDB
         (progn
          (if ZMIN
           (if (< (caddr NODE) ZMIN) (setq ZMIN (caddr NODE)))
           (setq ZMIN (caddr NODE))
          )
          (if ZMAX
           (if (> (caddr NODE) ZMAX) (setq ZMAX (caddr NODE)))
           (setq ZMAX (caddr NODE))
          )
          (setq D (/ (abs (- (+ (* (- (cadr P6) (cadr P5)) (car NODE)) (* (car P6) (cadr P5)))
                             (+ (* (- (car P6) (car P5)) (cadr NODE)) (* (cadr P6) (car P5)))
                          )
                     )
                     SWATH
                  )
          )
          (setq PLIST (append PLIST (list (list (- D (/ SWATH 2.0)) (caddr NODE)))))
         )
        )
        (if PLIST
         (progn
          (princ (strcat "\n" (itoa (length PLIST)) " points found..."))
          (setq PBASE (getpoint "\nBase point for section : "))
          (setq ZBASE (float (fix ZMIN)))
          (setq ZHEIGHT (- ZMAX ZMIN))
          (RFL:DRAWGRID "Lidar Section"                                             ; Title Text
                        0.5                                                         ; Title Height
                        0.5                                                         ; Title OFFSET
                        (list (- (car PBASE) (/ SWATH 2.0)) (cadr PBASE))           ; Basepoint
                        0.0                                                         ; Base Station
                        ZBASE                                                       ; Base Elevation
                        SWATH                                                       ; Grid Width
                        (float (+ (fix ZHEIGHT) 1))                                 ; Grid Height
                        1.0                                                         ; Vertical Exageration
                        0.25                                                        ; Text Height
                        0.25                                                        ; Text OFFSET
                        (/ (expt 10.0 (fix (/ (log SWATH) (log 10.0)))) 10.0)       ; Horizontal Grid
                        nil                                                         ; Horizontal Fine Grid
                        (/ (expt 10.0 (fix (/ (log SWATH) (log 10.0)))) 10.0)       ; Horizontal Text
                        (/ (expt 10.0 (fix (/ (log SWATH) (log 10.0)))) 10.0 1.0)   ; Vertical Grid
                        nil                                                         ; Vertical Fine Grid
                        (/ (expt 10.0 (fix (/ (log SWATH) (log 10.0)))) 10.0 1.0)   ; Vertical Text
                        "PR-GRID"                                                   ; Grid Layer
                        (getvar "CLAYER")                                           ; Fine Grid Layer
                        (getvar "CLAYER")                                           ; Text Layer
                        nil                                                         ; Label as Station
                        1.0                                                         ; Master Scale
                        1                                                           ; Direction (1 = Left to Right, -1 = Right to Left)
          )
          (setq ENTLISTALL (list (list (cons 0 "BLOCK")
                                       (cons 2 "*U")
                                       (cons 8 "0")
                                       (cons 70 1)
                                       (list 10 0.0 0.0 0.0)
                                 )
                           )
          )
          (setq RFL:QLSECTIONPLIST nil)
          (foreach P PLIST
           (progn
            (setq ENTLIST (list (cons 0 "POINT")
                                (list 10 (car P) (- (cadr P) ZBASE) 0.0)
                    )
            )
            (setq ENTLISTALL (append ENTLISTALL (list ENTLIST)))
            (setq RFL:QLSECTIONPLIST (append RFL:QLSECTIONPLIST (list (list (+ (car PBASE) (car P))
                                                                            (+ (cadr PBASE) (- (cadr P) ZBASE))
                                                                      )
                                                                )
                                     )
            )
           )
          )
          (setq ENTLISTALL (append ENTLISTALL (list (list (cons 0 "ENDBLK")))))
          (foreach NODE ENTLISTALL
           (progn
            (setq TMP (entmake NODE))
           )
          )
          (entmake (list (cons 0 "INSERT")
                         (cons 2 TMP)
                         (list 10 (car PBASE) (cadr PBASE) 0.0)
                         (cons 41 1.0)
                         (cons 42 1.0)
                         (cons 43 1.0)
                         (cons 50 0.0)
                   )
          )
         )
        )
       )
      )
     )
    )
   )
  )
 )

 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
 (setvar "CMDECHO" CMDECHO)
 (setvar "OSMODE" OSMODE)
 (setvar "ORTHOMODE" ORTHOMODE)
 nil
)
(defun C:CREATELIDARDB (/ C DBSOURCE DBFILE)
 (alert (strcat "Warning: This process can be very slow.  Testing has shown\n"
                "                 that writing to a local SSD on an Intel i5 will take\n"
                "                 approximately 3 seconds per 100,000 points,\n"
                "                 or approximately 1 hour for 100M points"
        )
 )
 (if (setq DBSOURCE (getfiled "Select a source .pts point file" "" "pts" 2))
  (if (setq DBFILE (getfiled "Select a target for new .db file" "" "db" 1))
   (progn
    (setq C -1)
    (while (and (/= (setq C (getint "\nEnter number of lines to disregard (typically 1 for new files) <1> : ")) nil)
                (< C 0)
           )
    )
    (if (= C nil) (setq C 1))
    (textscr)
    (RFL:CREATEDATABASE DBFILE)
    (RFL:ADDDATABASEPOINTS DBFILE DBSOURCE C)
   )
   (princ "\nNo target given!")
  )
  (princ "\nSo source given!")
 )
 T
)
(defun C:ADDLIDARDB (/ DBSOURCE DBFILE)
 (alert (strcat "Warning: This process can be very slow.  Testing has shown\n"
                "                 that writing to a local SSD on an Intel i5 will take\n"
                "                 approximately 3 seconds per 100,000 points,\n"
                "                 or approximately 1 hour for 100M points"
        )
 )
 (if (setq DBSOURCE (getfiled "Select a source .pts point file" "" "pts" 2))
  (if (setq DBFILE (getfiled "Select a target for new .db file" "" "db" 1))
   (progn
    (setq C -1)
    (while (and (/= (setq C (getint "\nEnter number of lines to disregard (typically 1 for new files) <1> : ")) nil)
                (< C 0)
           )
    )
    (if (= C nil) (setq C 0))
    (textscr)
    (RFL:ADDDATABASEPOINTS DBFILE DBSOURCE C)
   )
   (princ "\nNo target given!")
  )
  (princ "\nSo source given!")
 )
 T
)
