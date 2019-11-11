;
;
;     Program written by Robert Livingston, 2019-11-06
;
;     QLSECTION is a utility for drawing cross sections based on (lidar) points
;
;     Math:
;     https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line
;
(defun C:QLSECTION (/ ANGBASE ATTREQ C C0 CMDECHO DSTA ENT ENTLIST ENTSET NODE ORTHOMODE OSMODE P PBASE PS P1 P2 P3 P4 PLIST STA SWATH TMP ZBASE ZHEIGHT ZMIN ZMAX)
;(defun C:QLSECTION (/)
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
  (if (not INFILE) (close INFILE))
  (if (not INFILE2) (close INFILE2))
  ;(setq *error* nil)
  (print msg)
 )

 (if (= nil (setq SWATH (getdist "\nEnter swath width (30.0) : "))) (setq SWATH 30.0))
 (if (= nil (setq DSTA (getdist "\nEnter delta station length (0.2) : "))) (setq DSTA 0.2))
 (while (setq STA (getreal "\nEnter Station : "))
  (if (and (setq P1 (RFL:XY (list (- STA (/ DSTA 2.0)) (/ SWATH -2.0))))
           (setq P2 (RFL:XY (list (- STA (/ DSTA 2.0)) (/ SWATH 2.0))))
           (setq P3 (RFL:XY (list (+ STA (/ DSTA 2.0)) (/ SWATH -2.0))))
           (setq P4 (RFL:XY (list (+ STA (/ DSTA 2.0)) (/ SWATH 2.0))))
      )
   (progn
    (setq ENTSET (ssget "_CP" (list P1 P2 P4 P3)))
    (if (> (sslength ENTSET) 0)
     (progn
      (setq PLIST nil)
      (setq ZMIN nil)
      (setq ZMAX nil)
      (setq C 0)
      (setq C0 (sslength ENTSET))
      (while (< C C0)
       (setq ENT (ssname ENTSET C))
       (setq ENTLIST (entget ENT))
       (if (= "POINT" (cdr (assoc 0 ENTLIST)))
        (progn
         (setq P (cdr (assoc 10 ENTLIST)))
         (setq PS (RFL:STAOFF P))
         (setq PLIST (append PLIST (list (list (cadr PS) (caddr P)))))
         (if ZMIN
          (if (< (caddr P) ZMIN) (setq ZMIN (caddr P)))
          (setq ZMIN (caddr P))
         )
         (if ZMAX
          (if (> (caddr P) ZMAX) (setq ZMAX (caddr P)))
          (setq ZMAX (caddr P))
         )
        )
       )
       (if (= 0 (rem C 20)) (princ (strcat "\r" (itoa C) " of " (itoa C0))))
       (setq C (1+ C))
      )
      (if PLIST
       (progn
        (setq PBASE (getpoint "\nBase point for section : "))
        (setq ZBASE ZMIN)
        (setq ZHEIGHT (- ZMAX ZMIN))
        (RFL:DRAWGRID (strcat "Sta: " (RFL:STATXT STA))                           ; Title Text
                      0.5                                                         ; Title Height
                      0.5                                                         ; Title OFFSET
                      (list (- (car PBASE) (/ SWATH 2.0)) (cadr PBASE))           ; Basepoint
                      (/ SWATH -2.0)                                              ; Base Station
                      ZBASE                                                       ; Base Elevation
                      SWATH                                                       ; Grid Width
                      ZHEIGHT                                                     ; Grid Height
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
        (setq C 0)
        (setq C0 (length PLIST))
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
          
          ;(entmake (list (cons 0 "POINT")
          ;               (list 10
          ;                     (+ (car PBASE) (car P))
          ;                     (+ (cadr PBASE) (- (cadr P) ZMIN))
          ;               )
          ;         )
          ;)
          ;(if (= 0 (rem C 20)) (princ (strcat "\r" (itoa C) " of " (itoa C0))))
          ;(setq C (1+ C))
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