;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:SPIRALSTAOFF returns the station and offset of a point for a given entity
;
;
(defun RFL:SPIRALSTAOFF (P ENT / LO PLT PLTST PST SPIRALLIST STAOFFVAL)
 (setq SPIRALLIST (RFL:GETSPIRALDATA ENT))
 (if (= SPIRALLIST nil)
  (setq STAOFFVAL nil)
  (setq PLT (car SPIRALLIST)
        PLTST (cadr SPIRALLIST)
        PST (caddr SPIRALLIST)
        LO (cadddr SPIRALLIST)
        STAOFFVAL (RFL:SPIRALSTAOFF2 P PLT PLTST PST LO)
  )
 )
 STAOFFVAL
)
