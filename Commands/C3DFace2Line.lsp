;
;
;     Program written by Robert Livingston, 2020-04-27
;
;     3DFACE2LINE converts 3d Faces to Lines (and removes duplicates)
;
;
(defun C:3DFACE2LINE (/ C ENT ENTLIST ENTSET P1 P2 P3 P4 PB1 PB2 REP TOL)
 (setq C 0)
 (setq TOL 0.00001)
 (setq PB1 (getpoint "\nFirst point of selection box :"))
 (setq PB2 PB1)
 (while (/= (car (setq REP (grread T))) 3)
  (grdraw PB1 (list (car PB1) (cadr PB2)) -7)(grdraw (list (car PB1) (cadr PB2)) PB2 -7)(grdraw PB2 (list (car PB2) (cadr PB1)) -7)(grdraw (list (car PB2) (cadr PB1)) PB1 -7)
  (setq PB2 (cadr REP))
  (grdraw PB1 (list (car PB1) (cadr PB2)) -7)(grdraw (list (car PB1) (cadr PB2)) PB2 -7)(grdraw PB2 (list (car PB2) (cadr PB1)) -7)(grdraw (list (car PB2) (cadr PB1)) PB1 -7)
 )
 (redraw)
 (if (setq ENTSET (ssget "_C" PB1 PB2 '((0 . "3DFACE"))))
  (while (< C (sslength ENTSET))
   (if (= (rem C 1000) 0) (princ (strcat (itoa C) " of " (itoa (sslength ENTSET)) "\r")))
   (setq ENT (ssname ENTSET C))
   (setq ENTLIST (entget ENT))
   (if (= "3DFACE" (cdr (assoc 0 ENTLIST)))
    (progn
     (setq P1 (cdr (assoc 10 ENTLIST)))
     (setq P2 (cdr (assoc 11 ENTLIST)))
     (setq P3 (cdr (assoc 12 ENTLIST)))
     (setq P4 (cdr (assoc 13 ENTLIST)))
     (if (> (distance P1 P2) TOL) (entmake (list (cons 0 "LINE") (cons 10 P1) (cons 11 P2))))
     (if (> (distance P2 P3) TOL) (entmake (list (cons 0 "LINE") (cons 10 P2) (cons 11 P3))))
     (if (> (distance P3 P4) TOL) (entmake (list (cons 0 "LINE") (cons 10 P3) (cons 11 P4))))
     (if (> (distance P4 P1) TOL) (entmake (list (cons 0 "LINE") (cons 10 P4) (cons 11 P1))))
     (entdel ENT)
    )
   )
   (setq C (1+ C))
  )
 )
 (command "-OVERKILL" "C" PB1 PB2 "" "")
)