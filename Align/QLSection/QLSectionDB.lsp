;
;
;     Program written by Robert Livingston, 2019-11-06
;
;     QLSECTION is a utility for drawing cross sections based on (lidar) points
;
;     Math:
;     https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line
;
(defun C:QLSECTIONDB (/ ANG ANGBASE ATTREQ C C0 CMDECHO DISTANCE2D D DSTA ENT ENTLIST ENTSET GETPLISTDB NODE ORTHOMODE OSMODE P PBASE PS P1 P2 P3 P4 P5 P6 PLIST PLISTDB STA SWATH TMP ZBASE ZHEIGHT ZMIN ZMAX)
;(defun C:QLSECTIONDB (/)
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
 
 (defun GETPLISTDB (P1 P2 P3 P4 DSTA SWATH DBFILE / CHKERR D1 D2 PLISTDB S SQLSTRING)
  (defun CHKERR (CMD)
   (if (not CMD)
    (progn
     (princ (DSQL_LASTERR))
      nil
     )
    T
   )
  )
  (if (CHKERR (DSQL_OPEN DBFILE))
   (progn
    (setq D1 (DISTANCE2D P1 P2))
    (setq D2 (DISTANCE2D P3 P4))
    (setq SQLSTRING (strcat "SELECT * FROM Points WHERE "
                            "abs("
                            (rtos (- (cadr P2) (cadr P1)) 2 8)
                            " * X - "
                            (rtos (- (car P2) (car P1)) 2 8)
                            " * Y + "
                            (rtos (- (* (car P2) (cadr P1)) (* (cadr P2) (car P1))) 2 8)
                            " ) / "
                            (rtos D1 2 8)
                            " <= "
                            (rtos (/ DSTA 2.0) 2 8)
                            " AND "
                            "abs("
                            (rtos (- (cadr P4) (cadr P3)) 2 8)
                            " * X - "
                            (rtos (- (car P4) (car P3)) 2 8)
                            " * Y + "
                            (rtos (- (* (car P4) (cadr P3)) (* (cadr P4) (car P3))) 2 8)
                            " ) / "
                            (rtos D2 2 8)
                            " <= "
                            (rtos (/ SWATH 2.0) 2 8)
                            ";"
                    )
    )
    (setq PLISTDB (DSQL_QUERY DBFILE SQLSTRING))
    (CHKERR (DSQL_CLOSE DBFILE))
    PLISTDB
   )
   (progn
    (princ "\nSomething went wrong - problems opening database!")
    nil
   )
  )
 )
 
 (if (setq DBFILE (getfiled "Select a source .db file" "" "db" 2))
  (progn
   (if (= nil (setq SWATH (getdist "\nEnter swath width (30.0) : "))) (setq SWATH 30.0))
   (if (= nil (setq DSTA (getdist "\nEnter delta station length (0.01) : "))) (setq DSTA 0.01))
;   (while (setq STA (getreal "\nEnter Station : "))
   (while (/= "" (setq STA (getstring "\nStation ('P' to pick point) : ")))
    (if (= "P" (strcase (substr STA 1 1)))
     (setq STA (car (RFL:STAOFF (getpoint "\nPick point for section : "))))
     (setq STA (atof STA))
    )
    (if (and (setq P1 (RFL:XY (list STA (/ SWATH -2.0))))
             (setq P2 (RFL:XY (list STA (/ SWATH 2.0))))
        )
     (progn
      (setq ANG (angle P1 P2))
      (setq PLISTDB nil)
      (setq P (list (/ (+ (car P1) (car P2)) 2.0) (/ (+ (cadr P1) (cadr P2)) 2.0)))
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
      (if (setq PLISTDB (GETPLISTDB P1 P2 P3 P4 DSTA SWATH DBFILE))
       (progn
        (setq PLIST nil)
        (setq ZMIN nil)
        (setq ZMAX nil)
        (setq PLISTDB (cdr PLISTDB))
        (foreach NODE PLISTDB
         (progn
          (if ZMIN
           (if (< (cadddr NODE) ZMIN) (setq ZMIN (cadddr NODE)))
           (setq ZMIN (cadddr NODE))
          )
          (if ZMAX
           (if (> (cadddr NODE) ZMAX) (setq ZMAX (cadddr NODE)))
           (setq ZMAX (cadddr NODE))
          )
          (setq D (/ (abs (- (+ (* (- (cadr P6) (cadr P5)) (cadr NODE)) (* (car P6) (cadr P5)))
                             (+ (* (- (car P6) (car P5)) (caddr NODE)) (* (cadr P6) (car P5)))
                          )
                     )
                     SWATH
                  )
          )
          (setq PLIST (append PLIST (list (list (- D (/ SWATH 2.0)) (cadddr NODE)))))
         )
        )
        (if PLIST
         (progn
          (princ (strcat "\n" (itoa (length PLIST)) " points found..."))
          (setq PBASE (getpoint "\nBase point for section : "))
          (setq ZBASE ZMIN)
          (setq ZHEIGHT (- ZMAX ZMIN))
          (RFL:DRAWGRID (strcat "Sta: " (RFL:STATXT STA))                           ; Title Text
                        0.5                                                         ; Title Height
                        0.5                                                         ; Title OFFSET
                        (list (- (car PBASE) (/ SWATH 2.0)) (cadr PBASE))           ; Basepoint
                        (/ SWATH -2.0)                                              ; Base Station
                        (float (fix ZBASE))                                         ; Base Elevation
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
          (foreach P PLIST
           (progn
            (setq ENTLIST (list (cons 0 "POINT")
                                (list 10 (car P) (- (cadr P) ZMIN) 0.0)
                    )
            )
            (setq ENTLISTALL (append ENTLISTALL (list ENTLIST)))
            
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
(defun C:CREATELIDARDB (/ DBSOURCE DBFILE)
 (if (setq DBSOURCE (getfiled "Select a source .pts file" "" "pts" 2))
  (if (setq DBFILE (getfiled "Select a target for new .db file" "" "db" 1))
   (RFL:CREATELIDARDB DBSOURCE DBFILE T)
   (princ "\nNo target given!")
  )
  (princ "\nSo source given!")
 )
 T
)
(defun C:ADDLIDARDB (/ DBSOURCE DBFILE)
 (if (setq DBSOURCE (getfiled "Select a source .pts file" "" "pts" 2))
  (if (setq DBFILE (getfiled "Select a target .db file" "" "db" 2))
   (RFL:CREATELIDARDB DBSOURCE DBFILE nil)
   (princ "\nNo target given!")
  )
  (princ "\nSo source given!")
 )
 T
)
(defun RFL:CREATELIDARDB (DBSOURCE DBFILE NEWFLAG / C CHKERR CREATEDATABASE INFILE INLINE)
 (defun CHKERR (CMD)
  (if (not CMD)
   (progn
    (princ (DSQL_LASTERR))
     nil
    )
   T
  )
 )
 (defun CREATEDATABASE (MYDB)
  (CHKERR (DSQL_OPEN MYDB))
  (CHKERR (DSQL_CLOSE MYDB))
 )

 (if (setq INFILE (open DBSOURCE "r"))
  (progn
   (if (or NEWFLAG
           (CREATEDATABASE DBFILE)
       )
    (progn
     (CHKERR (DSQL_OPEN DBFILE))
     (if NEWFLAG
      (progn
       (setq C 0)
       (CHKERR (DSQL_DML DBFILE "CREATE TABLE Points (No int, X double, Y double, Z double);"))
      )
      (progn
       (setq C (DSQL_QUERY DBFILE "select MAX(No) from Points;"))
       (setq C (1+ (car (last C))))
      )
     )
     (CHKERR(DSQL_DML DBFILE "begin transaction;"))
     (while (setq INLINE (read-line INFILE))
      (if (and (/= (setq X (RFL:COLUMN INLINE 1 " ")) "")
               (/= (setq Y (RFL:COLUMN INLINE 2 " ")) "")
               (/= (setq Z (RFL:COLUMN INLINE 3 " ")) "")
	      )
       (progn
        (CHKERR (DSQL_DML DBFILE (strcat "INSERT INTO Points VALUES ("
                                         (itoa C)
                                         ", "
                                         X
                                         ", "
                                         Y
                                         ", "
                                         Z
                                         ");"
                                 )
                )
        )
        (if (= (rem C 1000) 0) (princ (strcat "\r" (itoa C))))
        (setq C (1+ C))
       )
      )
     )
     (CHKERR (DSQL_DML DBFILE "commit transaction;"))
     (CHKERR (DSQL_CLOSE DBFILE))
    )
    (princ "\nSomething went wrong - could not open database!")
   )
   (close INFILE)
  )
  (princ"\nSomething went wrong - could not open source file!")
 )
 T
)
;(defun C:TESTDB ()
; (CHKERR(DSQL_OPEN DBFILE))
; (setq XXX (DSQL_QUERY DBFILE "SELECT * FROM Points;"))
; T
;)
;; Test Code
;(setq MYDB "C:\\Users\\robert.livingston.CORP\\Documents\\test.db")
;(DEFUN C:BENCH1 ( / db end i start)
;  (SETQ I 0)
;  (SETVAR "cmdecho" 0)
;  (SETQ START (GETVAR "TDUSRTIMER"))
;  
;  (SETQ DB MYDB)
;  (CHKERR(DSQL_OPEN DB))
;  (CHKERR(DSQL_DML DB "create table Test1(No int, Name char(64));"))
;  (CHKERR(DSQL_DML DB "begin transaction;"))
;  (REPEAT 10000
;     (CHKERR(DSQL_DML "C:\\Users\\robert.livingston.CORP\\Documents\\test.db" 
;        (STRCAT "insert into Test1 values (" 
;           (ITOA (SETQ I (1+ I))) ", 'Welcome To TheSwamp');")))
;  )
;  (CHKERR (DSQL_DML MYDB "commit transaction;"))
;  (CHKERR (DSQL_CLOSE MYDB))
;  
;  (SETQ END (* 86400.0 (- (GETVAR "TDUSRTIMER") START)))
;  (PRINC "\n")
;  (PRINC END)
;  (PRINC)
;)
;
;
;(SETQ A (DSQL_QUERY DBFILE "SELECT * FROM Points WHERE X - Y < 100.0;"))T