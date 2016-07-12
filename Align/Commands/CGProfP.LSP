;
;
;   Program written by Robert Livingston, 98/05/13
;
;   C:GPROFP extracts an vertical alignment from a selected polyline
;
;
(defun C:GPROFP (/ ANGBASE ANGDIR CMDECHO ENT ENTLIST ELEV LR NODE NODEPREV OGLIST P TOL)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 0)

 (setq TOL 0.0001)
 (setq NODEPREV nil)
 
 (RFL:PROFDEF)

 (setq OGLIST nil)
 (if (/= nil RFL:PROFDEFLIST)
  (progn
   (princ "\nSelect polyline:")
   (setq ENT (car (entsel)))
   (if (= ENT nil)
    (setq ENTLIST nil)
    (setq ENTLIST (entget ENT))
   )
   (if (= nil ENT)
    (princ "\n*** NO ENTITY SELECTED ***\n")
    (if (/= (cdr (assoc 0 ENTLIST)) "LWPOLYLINE")
     (princ "\n*** NOT A R14 POLYLINE ***\n")
     (progn
      (while (/= ENTLIST nil)
       (setq NODE (car ENTLIST))
       (setq ENTLIST (cdr ENTLIST))
       (if (= (car NODE) 10)
        (if (or (= NODEPREV nil) (> (distance (cdr NODEPREV) (cdr NODE)) TOL))
         (progn
          (setq STA (+ (* (- (nth 0 (cdr NODE))
                             (nth 0 (cdr (assoc "BPOINT" RFL:PROFDEFLIST))))
                          (if (or (= (cdr (assoc "DIRECTION" RFL:PROFDEFLIST)) 1) (= (assoc "DIRECTION" RFL:PROFDEFLIST) nil)) 1.0 -1.0)
                       )
                       (cdr (assoc "STA" RFL:PROFDEFLIST))
                    )
          )
          (setq ELEV (+ (/ (- (nth 1 (cdr NODE))
                              (nth 1 (cdr (assoc "BPOINT" RFL:PROFDEFLIST))))
                           (cdr (assoc "VEXAG" RFL:PROFDEFLIST)))
                        (cdr (assoc "ELEV" RFL:PROFDEFLIST))))
          (setq OGLIST (append (list (list STA ELEV)) OGLIST))
          (setq NODEPREV NODE)
         )
        )
       )
      )
      (if (> (nth 0 (car OGLIST)) (nth 0 (last OGLIST)))
       (setq OGLIST (reverse OGLIST))
      )
     )
    )
   )
  )
 )
 (if (/= nil OGLIST)
  (progn
   (setq PVILIST nil)
   (foreach NODE OGLIST
    (progn
     (setq PVILIST (append PVILIST (list (list (car NODE) (cadr NODE) "L" 0.0))))
    )
   )
  )
 )

 (setvar "CMDECHO" CMDECHO)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
)