;
;
;   Program written by Robert Livingston, 98/05/13
;
;   C:GPROFOG extracts an OG vertical alignment from the current drawing
;
;
(defun C:GPROFOG (/ ANGBASE ANGDIR CMDECHO ENT ENTLIST ELEV LR NODE NODEPREV P TOL)
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
 (if (/= nil PROFDEF)
  (progn
   (princ "\nSelect OG polyline:")
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
                             (nth 0 (cdr (assoc "BPOINT" PROFDEF))))
                          (if (or (= (cdr (assoc "DIRECTION" PROFDEF)) 1) (= (assoc "DIRECTION" PROFDEF) nil)) 1.0 -1.0)
                       )
                       (cdr (assoc "STA" PROFDEF))
                    )
          )
          (setq ELEV (+ (/ (- (nth 1 (cdr NODE))
                              (nth 1 (cdr (assoc "BPOINT" PROFDEF))))
                           (cdr (assoc "VEXAG" PROFDEF)))
                        (cdr (assoc "ELEV" PROFDEF))))
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

 (setvar "CMDECHO" CMDECHO)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
)
