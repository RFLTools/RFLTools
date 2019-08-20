;
;
;     Program written by Robert Livingston, 2019-08-16
;
;     C:LISTK is a simple routine for displaying the K value for a selected vertical curve
;
;
(defun C:LISTK (/ ENT ENTLIST G1 G2 K L P1 P2 P3 X1 X2 X3 Y1 Y2 Y3)
 (setq K nil)
 (setq ENT (car (entsel "\nSelect Vertical curve : ")))
 (setq ENTLIST (entget ENT))
 (if (/= "POLYLINE" (cdr (assoc 0 ENTLIST)))
  (princ "\n*** Entity not a polyline ***")
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
     (setq VEXAG (getreal (strcat "\nEnter vertical exageration (" (rtos 10.0) ") : ")))
     (if (= nil VEXAG) (setq VEXAG 10.0))
     (setq X1 (nth 0 P1))
     (setq Y1 (/ (nth 1 P1) VEXAG))
     (setq X2 (nth 0 P2))
     (setq Y2 (/ (nth 1 P2) VEXAG))
     (setq X3 (nth 0 P3))
     (setq Y3 (/ (nth 1 P3) VEXAG))
     (setq G1 (/ (- Y2 Y1) (- X2 X1)))
     (setq G2 (/ (- Y3 Y2) (- X3 X2)))
     (setq L (- X3 X1))
     (setq K (abs (/ L (- G2 G1) 100.0)))
    )
   )
  )
 )
 (princ "\n")
 (if K
  (rtos K 2 8)
  nil
 )
)