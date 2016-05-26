;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:SPIRALXY returns the station and offset of the supplied point to the supplied entity
;
;
(defun RFL:SPIRALXY (P ENT / LO PLT PLTST PST SPIRALLIST STAOFFVAL PXY)
 (setq SPIRALLIST (RFL:GETSPIRALDATA ENT))
 (if (= SPIRALLIST nil)
  (setq PXY nil)
  (setq PLT (car SPIRALLIST)
        PLTST (cadr SPIRALLIST)
        PST (caddr SPIRALLIST)
        LO (cadddr SPIRALLIST)
        PXY (RFL:SPIRALXY2 P PLT PLTST PST)
  )
 )
 PXY
)
