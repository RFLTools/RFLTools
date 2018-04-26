;
;
;     Program written by Robert Livingston, 2015/03/16
;
;     BESTVCURVE is a utility for finding best fit vertical curve along a selected polyline
;
;
(defun C:BESTVCURVE (/ C D1 D2 ENT ENTLIST FLAG NODE P P1 P2 P3 OSMODE PLIST PLISTTMP PLINETYPE SPLINESEGS SPLINETYPE)
 (setq OSMODE (getvar "OSMODE"))
 (setvar "OSMODE" 0)
 (setq SPLINETYPE (getvar "SPLINETYPE"))
 (setvar "SPLINETYPE" 5)
 (setq SPLINESEGS (getvar "SPLINESEGS"))
 (setvar "SPLINESEGS" 65)
 (setq PLINETYPE (getvar "PLINETYPE"))
 (setvar "PLINETYPE" 0)
 (setq PLIST nil)
 (setq ENT (car (entsel "\nSelect polyline to fit parabolic vertical curve : ")))
 (setq PLIST (RFL:GETPLIST ENT))
 (if (/= nil (setq P1 (getpoint "\nPick a point near to start point (<return> for entire polyline) : ")))
  (if (/= nil (setq P2 (getpoint "\nPick a point near to end point : ")))
   (progn
    (setq PLIST (RFL:GETSUBPLIST PLIST P1 P2))
   )
  )
 )
 (if (/= nil (setq P (RFL:BESTVCURVE PLIST)))
  (progn
   (setq P1 (car P))
   (setq P2 (cadr P))
   (setq P3 (caddr P))
   (command "._PLINE" P1 P2 P3 "")
   (command "._PEDIT" (entlast) "S" "")
  )
 )
 (setvar "OSMODE" OSMODE)
 (setvar "SPLINETYPE" SPLINETYPE)
 (setvar "SPLINESEGS" SPLINESEGS)
 (setvar "PLINETYPE" PLINETYPE)
 (last P)
)
