;
;
;    Program Written by Robert Livingston, 99/07/14
;    C:FITSPIRAL draws a reverse engineered DCA spiral between two selected objects (lines and arcs)
;
;
(defun C:FITSPIRAL (/ CMDECHO ENT1 ENT2 ENTLIST1 ENTLIST2 GETLS LS1 LS2 R)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (defun GETLS (R MSG / LS AL)
  (setq LS nil)
  (setq AL "L")
  (if (= R 0.0)
   (progn
    (princ "\n*** Zero length arc selected - only spiral length valid!")
    (setq LS (getreal (strcat MSG " length :")))
   )
   (progn
    (while (= LS nil)
     (if (= AL "L")
      (progn
       (setq LS (getreal (strcat MSG " length <return for A>:")))
       (if (= LS nil)
        (progn
         (setq AL "A")
        )
       )
      )
      (progn
       (setq LS (getreal (strcat MSG " A <return for length>:")))
       (if (= LS nil)
        (progn
         (setq AL "L")
        )
        (progn
         (setq LS (/ (* LS LS) R))
        )
       )
      )
     )
    )
   )
  )
  (eval LS)
 )
 
 (if (/= (setq ENT1 (car (entsel "\nSelect first entity : "))) nil)
  (if (/= (setq ENT2 (car (entsel "\nSelect second entity : "))) nil)
   (progn
    (setq ENTLIST1 (entget ENT1))
    (setq ENTLIST2 (entget ENT2))
    (if (and (= (cdr (assoc 0 ENTLIST1)) "LINE") (= (cdr (assoc 0 ENTLIST2)) "LINE"))
     (progn
      (if (/= (setq R (getreal "\nEnter radius (0 for Spiral/Spiral) : ")) nil)
       (if (/= (setq LS1 (GETLS R "\nSpiral IN")) nil)
        (if (/= (setq LS2 (GETLS R "\nSpiral OUT")) nil)
         (RFL:FITSPIRALLL ENT1 ENT2 LS1 R LS2)
        )
       )
      )
     )
     (if (and (= (cdr (assoc 0 ENTLIST1)) "LINE") (= (cdr (assoc 0 ENTLIST2)) "ARC"))
      (progn
       (RFL:FITSPIRALLA ENT1 ENT2)
      )
      (if (and (= (cdr (assoc 0 ENTLIST1)) "ARC") (= (cdr (assoc 0 ENTLIST2)) "LINE"))
       (progn
        (RFL:FITSPIRALLA ENT2 ENT1)
       )
       (if (and (= (cdr (assoc 0 ENTLIST1)) "ARC") (= (cdr (assoc 0 ENTLIST2)) "ARC"))
        (progn
;         (RFL:FITSPIRALAA ENT1 ENT2)
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
