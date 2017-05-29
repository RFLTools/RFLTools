;
;
;   Program written by Robert Livingston, 2009/05/20
;
;   C:TPROFB is a utility for drawing drawing one alignments profile onto another
;          Modified to TPROFBN 2012/06/13 to work with nested RAB blocks
;
;
(defun C:TPROFBN (/ ALIGNLISTFROM ALIGNLISTTO ENT CMDECHO INC P PROFDEFTO PVILISTFROM STA STAFROM STATO Z)
;(defun C:TPROFN ()
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (command "._UNDO" "M")

 (setq ENT (car (nentsel "\nSelect 'FROM' nested alignment block :\n")))
 (setq ENT (cdr (assoc 330 (entget ENT))))
 (RFL:RALIGNBN ENT)
 (if (/= RFL:ALIGNLIST nil)
  (progn
   (setq ALIGNLISTFROM RFL:ALIGNLIST)
   (RFL:RPROFBN ENT)
   (if (/= RFL:PVILIST nil)
    (progn
     (setq PVILISTFROM RFL:PVILIST)
     (setq ENT (car (nentsel "\nSelect 'TO' nested alignment block :\n")))
     (setq ENT (cdr (assoc 330 (entget ENT))))
     (RFL:RALIGNB ENT)
     (if (/= RFL:ALIGNLIST nil)
      (progn
       (setq ALIGNLISTTO RFL:ALIGNLIST)
       (princ "\nSelect 'TO' profile :\n")
       (RFL:PROFDEF)
       (if (/= RFL:PROFDEFLIST nil)
        (progn
         (setq PROFDEFTO RFL:PROFDEFLIST)
         (setq STAFROM (getreal "\nEnter start station ('FROM' alignment, <return> for alignment start) :"))
         (if (= STAFROM nil)
          (setq STAFROM (max (car (car ALIGNLISTFROM)) (car (car PVILISTFROM))))
         )
         (setq STATO (getreal "\nEnter end station ('FROM' alignment, <return> for alignment end) :"))
         (if (= STATO nil)
          (progn
           (setq RFL:ALIGNLIST ALIGNLISTFROM)
           (setq STATO (min (+ (car (car ALIGNLISTFROM)) (RFL:GETALIGNLENGTH)) (car (last PVILISTFROM))))
          )
         )
         (setq INC (getreal "\nEnter incrament :"))
         (setq STA STAFROM)
         (command "._PLINE")
         (while (<= STA STATO)
          (setq RFL:ALIGNLIST ALIGNLISTFROM)
          (setq P (RFL:XY (list STA 0.0)))
          (if (/= P nil)
           (progn
            (setq RFL:PVILIST PVILISTFROM)
            (setq Z (RFL:ELEVATION STA))
            (if (/= Z nil)
             (progn
              (setq RFL:ALIGNLIST ALIGNLISTTO)
              (setq P (RFL:STAOFF P))
              (if (/= P nil)
               (progn
                (setq RFL:PROFDEFLIST PROFDEFTO)
                (command "_NON" (RFL:PROFPOINT (nth 0 P) Z))
               )
              )
             )
            )
           )
          )
          (setq STA (+ STA INC))
         )
         (command "")
        )
       )
      )
     )
    )
   )
  )
 )
 (setvar "CMDECHO" CMDECHO)
)