;
;
;    Program Written by Robert Livingston 99/08/10
;    PFIX is a utility for updating entities to a LWPolylines at a specified elevation
;
;
(defun C:PFIX (/ C CMDECHO ENT ENTLIST ENT2 ENT2LIST ENTSET PROCESS USEFIRST Z)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (setq USEFIRST nil)
 (setq Z (getreal (strcat "Enter elevtion <return for first vertex elevation> :")))
 (if (= Z nil) (setq USEFIRST 1))
 (initget "All Polylines")
 (setq PROCESS (getkword "\nProcess all entities or just polylines (All / Polylines) : "))
 (if (= PROCESS nil) (setq PROCESS "All"))
 (setq C 0)
 (setq ENTSET (ssget))
 (while (< C (sslength ENTSET))
  (setq ENT (ssname ENTSET C))
  (setq ENTLIST (entget ENT))
  (if (or (= PROCESS "All") (and (= PROCESS "Polylines") (or (= (cdr (assoc 0 ENTLIST)) "POLYLINE") (= (cdr (assoc 0 ENTLIST)) "LWPOLYLINE"))))
   (progn
    (if (= USEFIRST 1)
     (progn
      (if (= (cdr (assoc 0 ENTLIST)) "POLYLINE")
       (progn
        (setq ENT2 (entnext ENT))
        (setq ENT2LIST (entget ENT2))
        (setq Z (last (assoc 10 ENT2LIST)))
       )
       (progn
        (setq Z (last (assoc 10 ENTLIST)))
       )
      )
     )
    )
    (RFL:ELEVFIX ENT Z)
   )
  )
  (setq C (+ C 1))
 )

 (setvar "CMDECHO" CMDECHO)
 T
)