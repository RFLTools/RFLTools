;
;
;     Program written by Robert Livingston, 2018-03-12
;
;     C:QSADD adds a surface to a selected cross section
;
;
(defun C:QSADD (/ *error* ANGBASE ANGDIR C CMDECHO ENT ENTLIST ENTSET GRIDLIST OBSURFACE ORTHOMODE OSMODE P P1 P2 STA)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0.0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 1)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq OSMODE (getvar "OSMODE"))
 (setq ORTHOMODE (getvar "ORTHOMODE"))
 
 (defun *error* (msg)
  (setvar "ANGBASE" ANGBASE)
  (setvar "ANGDIR" ANGDIR)
  (setvar "CMDECHO" CMDECHO)
  (setvar "OSMODE" OSMODE)
  (setvar "ORTHOMODE" ORTHOMODE)
  (print msg)
 )
 
 (if RFL:ALIGNLIST
  (if (setq OBSURFACE (RFL:GETC3DSURFACE))
   (progn
    (setq C 0)
    (setq ENTSET (ssget '((0 . "INSERT"))))
    (while (< C (sslength ENTSET))
     (setq ENT (ssname ENTSET C)
           C (1+ C)
     )
     (if (setq GRIDLIST (RFL:GETGRID ENT))
      (progn
       (setq STA (atof (vl-string-subst "" "+" (vl-string-left-trim "Sta: " (cdr (assoc "TITLE" GRIDLIST))))))
       (if (and (>= STA (caar RFL:ALIGNLIST))
                (<= STA (+ (caar RFL:ALIGNLIST) (RFL:GETALIGNLENGTH)))
           )
        (progn
         (setq P1 (RFL:XY (list STA (cdr (assoc "BX" GRIDLIST)))))
         (setq P2 (RFL:XY (list STA (+ (cdr (assoc "BX" GRIDLIST)) (cdr (assoc "W" GRIDLIST))))))
         (if (and P1 P2)
          (progn
           (setq PLIST (RFL:GETSURFACELINE P1 P2 OBSURFACE))
           (setq OSLIST nil)
           (foreach P PLIST
            (setq OSLIST (append OSLIST (list (list (+ (distance (list (car P) (cadr P)) P1) (cdr (assoc "BX" GRIDLIST))) (caddr P)))))
           )
           (if OSLIST
            (progn
             (command "._PLINE")
             (foreach P OSLIST
              (progn
               (command (list (+ (- (car (cdr (assoc "BP" GRIDLIST))) (cdr (assoc "BX" GRIDLIST))) (car P))
                              (+ (cadr (cdr (assoc "BP" GRIDLIST))) (* (cdr (assoc "VEXAG" GRIDLIST)) (- (cadr P) (cdr (assoc "BY" GRIDLIST)))))
                        )
               )
              )
             )
             (command "")
            )
           )
          )
         )
        )
        (princ "\n!!! STATION NOT WITHIN ALIGNMENT !!!")
       )
      )
     )
    )
   )
  )
  (princ "\n!!! No Alignment Defined !!!")
 )
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
 (setvar "CMDECHO" CMDECHO)
 (setvar "OSMODE" OSMODE)
 (setvar "ORTHOMODE" ORTHOMODE)
 T
)