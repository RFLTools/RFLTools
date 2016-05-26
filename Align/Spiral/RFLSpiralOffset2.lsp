;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:SPIRALOFFSET2 returns the offset of an offset spiral based on supplied spiral data
;
;
(defun SPIRALOFFSET2 (P1 P2 PLT PLTST PST LO / OS P)
 (if (< (distance PST P1) (distance PST P2))
  (setq P P1)
  (setq P P2)
 )
 (setq OS (distance PST P))
 (setq OS (* OS
             -1.0
             (RFL:SIGN (sin (- (angle PLTST PST) (angle PLT PLTST))))
             (RFL:SIGN (- (sin (- (angle PLTST P) (angle PLT PLTST)))
                          (sin (- (angle PLTST PST) (angle PLT PLTST)))
                       )
             )
          )
 )
)
