;
;
;     Program written by Robert Livingston, 2013-05-01
;
;     C:TUNNELSECTION is a utility to draw a train section using survey shots
;
;
(defun C:TUNNELSECTION (/ *error* ANGBASE ANGDIR ATTREQ C CMDECHO E ENT ENTH ENTLIST ENTSET GROUPTOL NODE
                          ORTHOMODE OS OSMODE P PC PLIST R REP S1 S2 SECTINC SECTLIST SECTYPE STA STAAVE STAC
                          STALIST Z)
 (command "._UNDO" "M")
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
  (setvar "CMDECHO" CMDECHO)
  (setvar "OSMODE" OSMODE)
  (setvar "ORTHOMODE" ORTHOMODE)
  (print msg)
 )

 (if (= nil RFL:ALIGNLIST)
  (princ "\nNo alignment defined!")
  (progn
   (if (= nil RFL:QUICKTRAINDEF) (RFL:SETQUICKTRAINDEF))

   (initget 1 "0 1 2")
   (setq SECTYPE (atoi (getkword "\nSection type (0 = Points / 1 = Circular / 2 = 4 Point Square) : ")))
   (setq SECTLIST nil)
   (setq PLIST nil)
   (setq STALIST nil)
   (setq STAMIN nil)
   (setq STAMAX nil)
   (princ  "\nSelect survey point blocks : ")
   (setq ENTSET (ssget))
   (setq C 0)
   (princ (strcat "\nTotal entities selected : " (itoa (sslength ENTSET))))
   (while (< C (sslength ENTSET))
    (setq ENT (ssname ENTSET C))
    (setq ENTLIST (entget ENT))
    (if (and (= "INSERT" (cdr (assoc 0 ENTLIST))) (= 1 (cdr (assoc 66 ENTLIST))))
     (progn
      (setq P (cdr (assoc 10 ENTLIST)))
      (setq ENTH (cdr (assoc 5 ENTLIST)))
      (setq Z nil)
      (setq STA nil)
      (setq STAAVE 0.0)
      (setq STAC 0)
      (setq ENT (entnext ENT))
      (setq ENTLIST (entget ENT))
      (while (and (= nil Z) (/= "SEQEND" (cdr (assoc 0 ENTLIST))))
       (if (or (= "Z" (strcase (cdr (assoc 2 ENTLIST)))) (= "ELEV" (strcase (cdr (assoc 2 ENTLIST)))) (= "ELEVATION" (strcase (cdr (assoc 2 ENTLIST)))))
        (setq Z (atof (cdr (assoc 1 ENTLIST))))
       )
       (setq ENT (entnext ENT))
       (setq ENTLIST (entget ENT))
      )
      (if (/= nil Z)
       (setq STA (RFL:STAOFF P))
      )
      (if (/= nil STA)
       (setq SECTLIST (append SECTLIST (list (list (car STA) (cadr STA) (car P) (cadr P) Z))))
      )
     )
     (if (= "POINT" (cdr (assoc 0 ENTLIST)))
      (progn
       (setq P (cdr (assoc 10 ENTLIST)))
       (setq ENTH (cdr (assoc 5 ENTLIST)))
       (setq Z (caddr P))
       (setq P (list (car P) (cadr P)))
       (setq STA nil)
       (setq STAAVE 0.0)
       (setq STAC 0)
       (if (/= nil Z)
        (setq STA (RFL:STAOFF P))
       )
       (if (/= nil STA)
        (setq SECTLIST (append SECTLIST (list (list (car STA) (cadr STA) (car P) (cadr P) Z))))
       )
      )
     )
    )
    (setq C (+ C 1))
   )
   (if (= nil SECTLIST)
    (princ "\nNo valid points found!\n")
    (progn
     (princ (strcat "\nTotal valid points found : " (itoa (length SECTLIST))))
     (princ "\nSorting...")
     (setq SECTLIST (vl-sort SECTLIST '(lambda (S1 S2) (< (car S1) (car S2)))))
     
     (setq PC (getpoint "\nSelect first section insertion point : "))
     (setq PC (list (car PC) (cadr PC)))
     
     (setq SECTINC (cdr (assoc "SECTINC" RFL:QUICKTRAINDEF)))
     (setq GROUPTOL (cdr (assoc "GROUPTOL" RFL:QUICKTRAINDEF)))
     (setq C 0 STAAVE 0.0 STAMIN nil STAMAX nil STA nil STALIST nil PLIST nil)
     (foreach NODE SECTLIST
      (progn
       (setq STA (car NODE))
       (if (= nil STAMIN) (setq STAMIN (car NODE)))
       (if (< (- STA STAMIN) GROUPTOL)
        (progn
         (setq C (+ C 1))
         (setq STAMAX (car NODE))
         (setq STAAVE (+ STAAVE (car NODE)))
         (setq PLIST (append PLIST (list (cddr NODE))))
         (setq STALIST (append STALIST (list (list (cadr NODE) (last NODE)))))
        )
        (progn
         (setq STAAVE (/ STAAVE C))
         (RFL:DRAWTUNNELSECTION PC (list STAAVE STAMIN STAMAX) STALIST PLIST SECTYPE)
         (setq C 1)
         (setq STAMIN (car NODE))
         (setq STAMAX (car NODE))
         (setq STAAVE (car NODE))
         (setq PLIST  (list (cddr NODE)))
         (setq STALIST (list (list (cadr NODE) (last NODE))))
         (setq PC (list (car PC) (+ (cadr PC) SECTINC)))
        )
       )
      )
     )
     (setq STAAVE (/ STAAVE C))
     (RFL:DRAWTUNNELSECTION PC (list STAAVE STAMIN STAMAX) STALIST PLIST SECTYPE)
    )
   )
  )
 )

 (setvar "ATTREQ" ATTREQ)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
 (setvar "CMDECHO" CMDECHO)
 (setvar "OSMODE" OSMODE)
 (setvar "ORTHOMODE" ORTHOMODE)
)