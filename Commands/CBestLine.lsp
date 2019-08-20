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
; (setq ENT (car (entsel "\nSelect polyline to fit line : ")))
 (setq ENT (car (nentsel "\nSelect polyline to fit line : ")))
 (if (= "VERTEX" (cdr (assoc 0 (entget ENT))))
  (setq ENT (cdr (assoc 330 (entget ENT))))
 )
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
     (entmake (list (cons 0 "LINE")
                    (cons 10 P1)
                    (cons 11 P2)
              )
     )
     (if (last P) (princ (strcat "\nMax Offset = " (rtos (last P)) "\n")))
    )
   )
  )
 )
 (setvar "OSMODE" OSMODE)
 (setvar "ORTHOMODE" ORTHOMODE)
 (last P)
)
