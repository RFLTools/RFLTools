;
;
;   Program written by Robert Livingston, 99/10/21
;
;   CPI returns the PI point for a selected curve
;
;
(defun C:CPI (/ ANG ANG1 ANG2 GSPIRALPI D ENT ENTLIST P PC R THETA)
 (defun GSPIRALPI (ENT / ENTLIST ENTLIST2 SPI)
  (setq ENTLIST2 (cdr (assoc -3 (entget ENT '("*")))))
  (setq ENTLIST (cdr (assoc "DCA_FIGURE_XENT" ENTLIST2)))
  (if (or (= ENTLIST nil) (= (assoc 1011 ENTLIST) nil))
   (progn
    (setq SPIRALLIST nil)
   )
   (progn
    (while (/= (car (car ENTLIST)) 1011)
     (setq ENTLIST (cdr ENTLIST))
    )
    (setq ENTLIST (cdr ENTLIST))
    (while (/= (car (car ENTLIST)) 1011)
     (setq ENTLIST (cdr ENTLIST))
    )
    (setq SPI (cdr (car ENTLIST)))
   )
  )
  (setq SPI SPI)
 )
 (setq P nil)
 (if (/= nil (setq ENT (car (entsel "\nSelect arc or LDD spiral : "))))
  (progn
   (setq ENTLIST (entget ENT))
   (if (/= (cdr (assoc 0 ENTLIST)) "ARC")
    (progn
     (setq P (GSPIRALPI ENT))
     (if (/= nil P)
      (command "_NON" P)
     )
    )
    (progn
     (setq PC (cdr (assoc 10 ENTLIST)))
     (setq ANG1 (cdr (assoc 50 ENTLIST)))
     (setq ANG2 (cdr (assoc 51 ENTLIST)))
     (setq ANG (/ (+ ANG1 ANG2) 2.0))
     (setq THETA (- ANG2 ANG1))
     (if (or (= THETA pi) (= THETA (* 2.0 pi)))
      (princ "\n*** No PI exists ***")
      (progn
       (setq R (cdr (assoc 40 ENTLIST)))
       (setq D (/ R (cos (/ THETA 2.0))))
       (setq P (list (+ (nth 0 PC) (* D (cos ANG)))
                     (+ (nth 1 PC) (* D (sin ANG)))))
      )
     )
    )
   )
  )
 )
(command "_NON" P))