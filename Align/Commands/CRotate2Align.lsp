;
;
;     Program written by Robert LIvingston, 02/02/28
;
;     ROTATE2ALIGN changes the rotation of selected block entities to match the current alignment
;
;
(defun C:ROTATE2ALIGN (/ *error* ANGBASE ANGDIR ANG C CMDECHO DIRECTION ENT ENTLIST ENTSET OSMODE P STA)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq OSMODE (getvar "OSMODE"))
 (setvar "OSMODE" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0.0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 0)

 (defun *error* (msg)
  (if (>= (atof (getvar "ACADVER")) 18.2)
   (command-s "._UCS" "P")
   (command "._UCS" "P")
  )
  (setvar "CMDECHO" CMDECHO)
  (setvar "OSMODE" OSMODE)
  (setvar "ANGBASE" ANGBASE)
  (setvar "ANGDIR" ANGDIR)
  (princ msg)
  ;(setq *error* nil)
 )

 (command "._UNDO" "M")
 (command "._UCS" "W")

 (initget "Forward Reverse")
 (setq DIRECTION (getkword "\n<Forward> or Reverse ? ")) 
 (if (= DIRECTION nil) (setq DIRECTION "Forward"))
 (setq ENTSET (ssget))
 (setq C 0)
 (while (< C (sslength ENTSET))
  (setq ENT (ssname ENTSET C))
  (setq ENTLIST (entget ENT))
  (if (= (cdr (assoc 0 ENTLIST)) "INSERT")
   (progn
    (setq P (cdr (assoc 10 ENTLIST)))
    (setq STA (RFL:STAOFF P))
    (if (/= STA nil)
     (progn
      (setq ANG (angle (RFL:XY (list (nth 0 STA) (nth 1 STA))) (RFL:XY (list (nth 0 STA) (+ (nth 1 STA) 100.0)))))
      (if (= DIRECTION "Reverse")
       (setq ANG (- ANG (/ pi 2.0)))
       (setq ANG (+ ANG (/ pi 2.0)))
      )
      (command "._ROTATE" ENT "" P (* (/ 180.0 pi) (- ANG (cdr (assoc 50 ENTLIST)))))
     )
    )
   )
  )
  (setq C (+ C 1))
 )

 (command "._UCS" "P")
 (setvar "CMDECHO" CMDECHO)
 (setvar "OSMODE" OSMODE)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
)