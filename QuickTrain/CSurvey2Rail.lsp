;
;
;     Program written by Robert Livingston, 2017-04-10
;
;     C:SURVEY2RAIL estimates a rail alignment (Hor, Ver and Super) based on two selected 3D Rail Polylines
;
;
(defun C:SURVEY2RAIL (/ 2DDIST AL1 AL2 ALM C CMDECHO ORTHOMODE ELM ENT ENT1 ENT2 ENTLIST ENTLIST1 ENTLIST2 NODE OSMODE P P1 P2 PLIST1 PLIST2 PLISTM PVIL1 PVIL2 PVILM RFL:ALIGNLIST RFL:PVILIST RFL:SUPERLIST STA TMP ZL ZR)
;(defun C:SURVEY2RAIL ()
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq ORTHOMODE (getvar "ORTHOMODE"))
 (setvar "ORTHOMODE" 0)
 (setq OSMODE (getvar "OSMODE"))
 (setvar "OSMODE" 0)
 
 (command "._UNDO" "M")
 
 (defun 2DDIST (P1 P2)
  (distance (list (car P1) (cadr P1)) (list (car P2) (cadr P2)))
 )
 
 (setq PLISTM nil)
 (if (setq ENT1 (car (entsel "\nSelect first polyline : ")))
  (if (setq PLIST1 (RFL:GETPLIST ENT1))
   (if (setq ENT2 (car (entsel "\nSelect second polyline : ")))
    (if (setq PLIST2 (RFL:GETPLIST ENT2))
     (progn
      (setq PLISTM (RFL:MIDPLIST PLIST1 PLIST2)
            ENTLIST1 (entget ENT1)
            ENTLIST2 (entget ENT2)
      )
      (RFL:3DP2ALIGN ENT1 0.0)
      (setq AL1 RFL:ALIGNLIST
            PVIL1 RFL:PVILIST
      )
      (RFL:3DP2ALIGN ENT2 0.0)
      (setq AL2 RFL:ALIGNLIST
            PVIL2 RFL:PVILIST
      )
     )
    )
   )
  )
 )
 (if PLISTM
  (if (or (= (float (/ (cdr (assoc 70 ENTLIST1)) 2 2 2 2)) (/ (cdr (assoc 70 ENTLIST1)) 16.0))
          (= (float (/ (cdr (assoc 70 ENTLIST2)) 2 2 2 2)) (/ (cdr (assoc 70 ENTLIST2)) 16.0))
      )
   (princ "\nSelected rail polylines not 3D!\n")
   (if (setq ENT (car (entsel "\nSelect RAB alignment block : ")))
    (if (/= "RFLALIGN" (strcase (cdr (assoc 2 (setq ENTLIST (entget ENT))))))
     (princ "\nNot an RFL Alignment block!\n")
     (progn
      (setq RFL:ALIGNLIST nil
            RFL:PVILIST nil
            RFL:SUPERLIST nil
            STA 0.0
            ALM nil
            PVILM nil
            ELM nil
      )
      (setq P2 (car PLISTM))
      (setq C 1)
      (while (< C (length PLISTM))
       (setq P1 P2
             P2 (nth C PLISTM)
             ALM (append ALM (list (list STA (list (car P1) (cadr P1)) (list (car P2) (cadr P2)) 0.0)))
             STA (+ STA (2DDIST P1 P2))
             C (1+ C)
       )
      )
      (foreach P PLISTM
       (progn
        (setq RFL:ALIGNLIST AL1
              RFL:PVILIST PVIL1
              P1 (RFL:STAOFF P)
        )
        (if (and P1 (setq Z1 (RFL:ELEVATION (car P1))))
         (progn
          (setq RFL:ALIGNLIST AL2
                RFL:PVILIST PVIL2
                P2 (RFL:STAOFF P)
          )
          (if (and P2 (setq Z2 (RFL:ELEVATION (car P2))))
           (progn
            (if (< (cadr P1) 0.0)
             (setq TMP P1
                   P1 P2
                   P2 TMP
                   TMP Z1
                   Z1 Z2
                   Z2 TMP
             )
            )
            (setq RFL:ALIGNLIST ALM)
            (setq STA (car (RFL:STAOFF P)))
            (setq PVILM (append PVILM (list (list STA (min Z1 Z2) "L" 0.0))))
            (if (> Z1 Z2)
             (setq ELM (append ELM (list (list STA (* 1000.0 (- Z1 Z2)) 0.0))))
             (setq ELM (append ELM (list (list STA 0.0 (* 1000.0 (- Z2 Z1))))))
            )
           )
          )
         )
        )
       )
      )
      (setq RFL:ALIGNLIST ALM
            RFL:PVILIST PVILM
            RFL:SUPERLIST ELM
      )
      (setq ENT (RFL:WALIGNB ENT))
      (setq ENT (RFL:WPROFB ENT))
      (setq ENT (RFL:WSUPERB ENT))
     )
    )
   )
  )
  (princ "\nError solving for mid points!")
 )


 (setvar "CMDECHO" CMDECHO)
 (setvar "ORTHOMODE" ORTHOMODE)
 (setvar "OSMODE" OSMODE)
 T
)