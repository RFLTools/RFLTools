;
;
;     Program written by Robert Livingston, 04-12-08
;
;     LVCURVE is a utility for labelling Vertical Profile entities
;
;
(defun C:LVCURVE (/ *error* ANGBASE ANGDIR CMDECHO ENT ENTLIST G1 G2 K L P P1 P2 P3 STR TMP VEXAG)
 (command "._UNDO" "M")

 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0.0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 0)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq OSMODE (getvar "OSMODE"))
 (setvar "OSMODE" 0)

 (defun *error* (msg)
  (setvar "ANGBASE" ANGBASE)
  (setvar "ANGDIR" ANGDIR)
  (setvar "CMDECHO" CMDECHO)
  (setvar "OSMODE" OSMODE)
  (print msg)
  (setq *error* nil)
 )

 (setq H nil)
 (setq ENTLIST (tblsearch "STYLE" (getvar "TEXTSTYLE")))
 (setq H (cdr (assoc 40 ENTLIST)))

 (setq VEXAG (getreal "\nEnter vertical exageration <10.000> : "))
 (if (= VEXAG nil) (setq VEXAG 10.0))

 (while (/= nil (setq ENT (car (entsel "\nSelect profile entity : "))))
  (setq ENTLIST (entget ENT))
  (if (= "LINE" (cdr (assoc 0 ENTLIST)))
   (progn
    (setq P1 (cdr (assoc 10 ENTLIST)))
    (setq P2 (cdr (assoc 11 ENTLIST)))
    (setq P (list (/ (+ (car P1) (car P2)) 2.0) (/ (+ (cadr P1) (cadr P2)) 2.0)))
    (if (< (car P2) (car P1))
     (progn
      (setq TMP P1)
      (setq P1 P2)
      (setq P2 TMP)
     )
    )
    (setq TMP (/ (- (cadr P2) (cadr P1)) (- (car P2) (car P1)) VEXAG 0.01))
    (if (= H 0.0)
     (command "._TEXT" "BC" P 5.0 P2 (strcat (if (< TMP 0.0) "-" "+") (rtos (abs TMP)) "%"))
     (command "._TEXT" "BC" P P2 (strcat (if (< TMP 0.0) "-" "+") (rtos (abs TMP)) "%"))
    )
   )
   (if (= "POLYLINE" (cdr (assoc 0 ENTLIST)))
    (progn
     (setq ENT (entnext ENT))
     (setq ENTLIST (entget ENT))
     (setq P1 (cdr (assoc 10 ENTLIST)))
     (setq ENT (entnext ENT))
     (setq ENTLIST (entget ENT))
     (setq P2 (cdr (assoc 10 ENTLIST)))
     (if (/= nil P2)
      (progn
       (setq ENT (entnext ENT))
       (setq ENTLIST (entget ENT))
       (setq P3 (cdr (assoc 10 ENTLIST)))
       (if (/= nil P3)
        (progn
         (setq ENT (entnext ENT))
         (setq ENTLIST (entget ENT))
         (while (/= "SEQEND" (cdr (assoc 0 ENTLIST)))
          (setq P2 P3)
          (setq P3 (cdr (assoc 10 ENTLIST)))
          (setq ENT (entnext ENT))
          (setq ENTLIST (entget ENT))
         )
        )
       )
      )
     )
     (if (and (/= nil P1) (/= nil P2) (/= nil P3))
      (progn
       (setq P (list (car P2) (/ (+ (cadr P1) (cadr P2) (cadr P3)) 3.0)))
       (setq L (abs (- (nth 0 P3) (nth 0 P1))))
       (setq G1 (/ (- (nth 1 P2) (nth 1 P1))
                   (- (nth 0 P2) (nth 0 P1))
                   VEXAG
                )
       )
       (setq G2 (/ (- (nth 1 P3) (nth 1 P2))
                   (- (nth 0 P3) (nth 0 P2))
                   VEXAG
                )
       )
       (setq K (abs (/ L (- G2 G1) 100.0)))
       (if (= H 0.0)
        (progn
         (command "._TEXT" "M" P 5.0 0.0 (strcat "K = " (rtos (abs K))))
         (command "._TEXT" "" (strcat "L = " (rtos (abs L))))
        )
        (progn
         (command "._TEXT" "M" P 0.0 (strcat "K = " (rtos (abs K))))
         (command "._TEXT" "" (strcat "L = " (rtos (abs L))))
        )
       )
      )
     )
    )
   )
  )
 )

 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
 (setvar "CMDECHO" CMDECHO)
 (setvar "OSMODE" OSMODE)
)