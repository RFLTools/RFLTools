;
;
;     Program written by Robert Livingston, 2013-05-01
;
;     RFL:UPDATETRAIN Updates a train set at STA
;
;
(defun RFL:UPDATETRAIN (STA ENTSET / *error* AC AT1 AT2 ANGBASE ANGDIR ATTREQ CMDECHO DX1 DX2 DY DZ1 DZ2 ENT ENT2 ENTC ENTC2 ENTT1 ENTT2 ENTT3 ENTLIST ENTSET2 GAUGE ORTHOMODE OSMODE P P2 PC PC1 PC2 PT1 PT2 STA2 WB WBT1 WBT2)
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

 (setq ENTC (ssname ENTSET 0))
 (setq ENTT1 (ssname ENTSET 1))
 (setq ENTT2 (ssname ENTSET 2))
 (if (> (sslength ENTSET) 3)
  (progn
   (setq ENTC2 (ssname ENTSET 3))
   (setq ENTT3 (ssname ENTSET 4))
   (setq ENTSET2 (ssadd))
   (setq ENTSET2 (ssadd ENTC2 ENTSET2))
   (setq ENTSET2 (ssadd ENTT2 ENTSET2))
   (setq ENTSET2 (ssadd ENTT3 ENTSET2))
  )
  (progn
   (setq ENTC2 nil)
   (setq ENTT3 nil)
   (setq ENTSET2 nil)
  )
 )
  
 (setq AC nil AT1 nil AT2 nil DY 0.0 PC1 nil PC2 nil PT1 nil PT2 nil)
 (setq ENT2 (entnext ENTT1))
 (setq ENTLIST (entget ENT2))
 (setq WBT1 nil GAUGE nil DX1 nil DZ1 nil DX2 nil DZ2 nil)
 (while (= "ATTRIB" (cdr (assoc 0 ENTLIST)))
  (if (= "D" (cdr (assoc 2 ENTLIST))) (setq WBT1 (atof (cdr (assoc 1 ENTLIST)))))
  (if (= "G" (cdr (assoc 2 ENTLIST))) (setq GAUGE (* 1000.0 (atof (cdr (assoc 1 ENTLIST))))))
  (if (= "DX" (cdr (assoc 2 ENTLIST))) (setq DX1 (atof (cdr (assoc 1 ENTLIST)))))
  (if (= "DZ" (cdr (assoc 2 ENTLIST))) (setq DZ1 (atof (cdr (assoc 1 ENTLIST)))))
  (if (= "DX2" (cdr (assoc 2 ENTLIST))) (setq DX2 (atof (cdr (assoc 1 ENTLIST)))))
  (if (= "DZ2" (cdr (assoc 2 ENTLIST))) (setq DZ2 (atof (cdr (assoc 1 ENTLIST)))))
  (setq ENT2 (entnext ENT2))
  (setq ENTLIST (entget ENT2))
 )
 (if (/= nil DX2) (setq DX1 DX2))
 (if (/= nil DZ2) (setq DZ1 DZ2))
 (if (= nil DX1) (setq DX1 (/ WBT1 2.0) DZ1 0.0))
 (setq ENT2 (entnext ENTT2))
 (setq ENTLIST (entget ENT2))
 (setq WBT2 nil GAUGE nil DX2 nil DZ2 nil)
 (while (= "ATTRIB" (cdr (assoc 0 ENTLIST)))
  (if (= "D" (cdr (assoc 2 ENTLIST))) (setq WBT2 (atof (cdr (assoc 1 ENTLIST)))))
  (if (= "G" (cdr (assoc 2 ENTLIST))) (setq GAUGE (* 1000.0 (atof (cdr (assoc 1 ENTLIST))))))
  (if (= "DX" (cdr (assoc 2 ENTLIST))) (setq DX2 (atof (cdr (assoc 1 ENTLIST)))))
  (if (= "DZ" (cdr (assoc 2 ENTLIST))) (setq DZ2 (atof (cdr (assoc 1 ENTLIST)))))
  (setq ENT2 (entnext ENT2))
  (setq ENTLIST (entget ENT2))
 )
 (if (= nil DX2) (setq DX2 (/ WBT2 2.0) DZ2 0.0))
 (setq ENT2 (entnext ENTC))
 (setq ENTLIST (entget ENT2))
 (setq WB nil GAUGE nil)
 (while (= "ATTRIB" (cdr (assoc 0 ENTLIST)))
  (if (= "D" (cdr (assoc 2 ENTLIST))) (setq WB (atof (cdr (assoc 1 ENTLIST)))))
  (if (= "G" (cdr (assoc 2 ENTLIST))) (setq GAUGE (* 1000.0 (atof (cdr (assoc 1 ENTLIST))))))
  (setq ENT2 (entnext ENT2))
  (setq ENTLIST (entget ENT2))
 )

(setq XPT1 nil XPC1 nil XSTA nil XPT2 nil XSTA2 nil XAT2 nil XAC nil)
 (if (/= nil (setq PT1 (RFL:POINTATSTATION STA)))
  (if (/= nil (setq STA2 (RFL:FINDFRONTSTA STA WBT1)))
   (progn
    (setq AT1 (RFL:GETTRUCKANGS STA STA2 GAUGE))
    (if (/= nil (setq PC1 (RFL:GETTRUCKPOUT STA WBT1 GAUGE DX1 DY DZ1)))
     (if (/= nil (setq STA (RFL:FINDFRONTTRUCK PC1 WB WBT2 DX2 DZ2 GAUGE)))
      (if (/= nil (setq PT2 (RFL:POINTATSTATION STA)))
       (if (/= nil (setq STA2 (RFL:FINDFRONTSTA STA WBT2)))
        (progn
         (setq AT2 (RFL:GETTRUCKANGS STA STA2 GAUGE))
         (if (/= nil (setq PC2 (RFL:GETTRUCKPOUT STA WBT2 GAUGE DX2 DY DZ2)))
          (progn
           (setq AC (RFL:GETCARANGS PC1 PC2 GAUGE))
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

 (setvar "ATTREQ" ATTREQ)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
 (setvar "CMDECHO" CMDECHO)
 (setvar "OSMODE" OSMODE)
 (setvar "ORTHOMODE" ORTHOMODE)

 (if (= nil AT2)
  (progn
   nil
  )
  (progn
   (if (or (= nil ENTSET2) (/= nil (RFL:UPDATETRAIN STA ENTSET2)))
    (progn
     (RFL:UPDATETRAINENT ENTT1 PT1 (car AT1) (cadr AT1) (caddr AT1))
     (RFL:UPDATETRAINENT ENTT2 PT2 (car AT2) (cadr AT2) (caddr AT2))
     (RFL:UPDATETRAINENT ENTC PC1 (car AC) (cadr AC) (caddr AC))
     ;(eval T)
     (append (list PC1) AC)
    )
    nil
   )
  )
 )
)
