;
;
;     Program written by Robert Livingston 2014-11-24
;
;     DRAPEPOLY is a utility for draping a LWPolyline onto a surface to create a 3DPolyline
;
;
(defun C:DRAPEPOLY (/ ALSAVE CMDECHO ENT ENTLIST P)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq ALSAVE RFL:ALIGNLIST)
 (princ "\NSelect LWPolyline : ")
 (setq ENT (entsel))
 (setq P (cadr ENT))
 (setq ENT (car ENT))
 (setq ENTLIST (entget ENT))
 (if (= "POLYLINE" (cdr (assoc 0 ENTLIST)))
  (progn
   (command "._CONVERT" "P" "S" ENT "")
   (setq ENTLIST (entget ENT))
  )
 )
 (if (/= "LWPOLYLINE" (cdr (assoc 0 ENTLIST)))
  (princ "\n*** Not an usable POLYLINE! ***")
  (progn
   (if (< (distance P (cdr (assoc 10 ENTLIST)))
          (distance P (cdr (assoc 10 (reverse ENTLIST))))
       )
    (setq RFL:ALIGNLIST (RFL:ALIGNDEF ENT (cdr (assoc 10 ENTLIST)) 0.0))
    (setq RFL:ALIGNLIST (RFL:ALIGNDEF ENT (cdr (assoc 10 (reverse ENTLIST))) 0.0))
   )
   (C:DRAPEALIGN)
  )
 )
 (setq RFL:ALIGNLIST ALSAVE)
 (setvar "CMDECHO" CMDECHO)
 nil
)
