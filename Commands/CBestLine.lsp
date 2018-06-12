;
;
;     Program written by Robert Livingston, 2015/03/16
;
;     BESTLINE is a utility for finding best fit line along a selected polyline
;
;
(defun C:BESTLINE (/ D1 D2 ENT ENTLIST FLAG ORTHOMODE OSMODE P P1 P2 P3 PLIST PLISTTMP)
 (setq OSMODE (getvar "OSMODE"))
 (setvar "OSMODE" 0)
 (setq ORTHOMODE (getvar "ORTHOMODE"))
 (setvar "ORTHOMODE" 0)
 (setq PLIST nil)
 (setq ENT (car (entsel "\nSelect polyline to fit line : ")))
 (if (/= (setq PLIST (RFL:GETPLIST ENT)) nil)
  (progn
   (setq P1 (getpoint "\nPick point near desired start vertex (<return> for entire polyline) : "))
   (if (/= P1 nil)
    (progn
     (setq P2 (getpoint "\nPick point near desired end vertex : "))
     (if (/= P2 nil)
      (setq PLIST (RFL:GETSUBPLIST PLIST P1 P2))
     )
    )
   )
   (if (/= nil (setq P (RFL:BESTLINE PLIST)))
    (progn
     (setq P1 (car P))
     (setq P2 (cadr P))
     (command "._LINE" P1 P2 "")
    )
   )
  )
 )
 (setvar "OSMODE" OSMODE)
 (setvar "ORTHOMODE" ORTHOMODE)
 (last P)
)
