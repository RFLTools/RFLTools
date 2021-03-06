;
;
;   Program written by Robert Livingston, 98/05/13
;
;   C:GPROF extracts a vertical alignment from the current drawing
;
;
(defun C:GPROF (/ ANGBASE ANGDIR C CMDECHO ENT ENTSET PVIENT PVISET STA ELEV LR VAL)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 0)

 (RFL:PROFDEF)

 (setq RFL:PVILIST nil)
 (if (/= nil RFL:PROFDEFLIST)
  (progn
   (princ "Select PVI blocks (or <return> to find all based on profile definition block) : ")
   (setq ENTSET (ssget))
   (if (= nil ENTSET)
    (progn
     (setq PVISET (ssget "X" (list (cons 0 "INSERT")
                                   (cons -4 "<OR")
                                   (cons 2 "RFLPVI")
                                   (cons 2 "PVI2")
                                   (cons -4 "OR>")
                                   (cons 8 (cdr (assoc "PLAYER" RFL:PROFDEFLIST))))))
    )
    (progn
     (setq C 0)
     (setq PVISET nil)
     (while (< C (sslength ENTSET))
      (setq ENT (ssname ENTSET C))
      (if (and (/= nil (cdr (assoc 2 (entget ENT))))
               (or (= "RFLPVI" (strcase (cdr (assoc 2 (entget ENT)))))
                   (= "PVI2" (strcase (cdr (assoc 2 (entget ENT)))))
               )
          )
       (if (= nil PVISET)
        (setq PVISET (ssadd ENT))
        (ssadd ENT PVISET)
       )
      )
      (setq C (+ C 1))
     )
    )
   )

   (if (= PVISET nil)
    (princ "\n*** NO PVI's EXIST ***\n")
    (if (= (sslength PVISET) 1)
     (princ "\n*** ONLY ONE PVI EXISTS ***\n")
     (progn
      (while (> (sslength PVISET) 0)
       (setq C 1)
       (setq PVIENT (ssname PVISET 0))
       (while (< C (sslength PVISET))
        (if (or (= (cdr (assoc "DIRECTION" RFL:PROFDEFLIST)) 1) (= (assoc "DIRECTION" RFL:PROFDEFLIST) nil))
         (if (< (nth 0 (cdr (assoc 10 (entget PVIENT))))
                (nth 0 (cdr (assoc 10 (entget (ssname PVISET C))))))
          (setq PVIENT (ssname PVISET C))
         )
         (if (> (nth 0 (cdr (assoc 10 (entget PVIENT))))
                (nth 0 (cdr (assoc 10 (entget (ssname PVISET C))))))
          (setq PVIENT (ssname PVISET C))
         )
        )
        (setq C (+ C 1))
       )
       (setq PVISET (ssdel PVIENT PVISET))
       (setq STA (+ (* (- (nth 0 (cdr (assoc 10 (entget PVIENT))))
                          (nth 0 (cdr (assoc "BPOINT" RFL:PROFDEFLIST))))
                       (if (or (= (cdr (assoc "DIRECTION" RFL:PROFDEFLIST)) 1) (= (assoc "DIRECTION" RFL:PROFDEFLIST) nil)) 1.0 -1.0)
                    )
                    (cdr (assoc "STA" RFL:PROFDEFLIST))
                 )
       )
       (setq ELEV (+ (/ (- (nth 1 (cdr (assoc 10 (entget PVIENT))))
                           (nth 1 (cdr (assoc "BPOINT" RFL:PROFDEFLIST))))
                        (cdr (assoc "VEXAG" RFL:PROFDEFLIST)))
                     (cdr (assoc "ELEV" RFL:PROFDEFLIST))))
       (setq PVIENT (entnext PVIENT))
       (while (/= "SEQEND" (cdr (assoc 0 (entget PVIENT))))
        (if (= "R" (cdr (assoc 2 (entget PVIENT))))
         (if (/= "" (cdr (assoc 1 (entget PVIENT))))
          (progn
           (setq LR "R")
           (setq VAL (atof (cdr (assoc 1 (entget PVIENT)))))
          )
         )
        )
        (if (= "L" (cdr (assoc 2 (entget PVIENT))))
         (if (/= "" (cdr (assoc 1 (entget PVIENT))))
          (progn
           (setq LR "L")
           (setq VAL (atof (cdr (assoc 1 (entget PVIENT)))))
          )
         )
        )
        (if (= "LENGTH" (cdr (assoc 2 (entget PVIENT))))
         (progn
          (setq LR "L")
          (setq VAL (atof (cdr (assoc 1 (entget PVIENT)))))
         )
        )
        (setq PVIENT (entnext PVIENT))
       )
       (setq RFL:PVILIST (append (list (list STA ELEV LR VAL)) RFL:PVILIST))
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
