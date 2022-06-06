;
;
;     Program written by Robert Livingston, 2022-06-03
;
;     RFL:PL2TANGENT returns (list P1 P2 PLIST1 PLIST2) where:
;          P1 P2 are the longest line less than the min offset
;          PLIST1 the list of points before P1
;
;
(setq RFL:PL2TANGENTLIST nil)
(defun RFL:PL2TANGENT (PLIST D / P PA PB P1 P2 PT1 PT2 PLIST0 PLIST1 PLIST2 PLISTTMP)
 (setq PLIST0 PLIST)
 (if (>= (length PLIST) 2)
  (progn
   (setq PA (car PLIST))
   (setq PB (last PLIST))
   (setq P1 (list (/ (+ (car PA) (car PB)) 2.0) (/ (+ (cadr PA) (cadr PB)) 2.0)))
   (setq PLISTTMP nil)
   (setq P2 (car PLIST))
   (setq PLISTTMP (vl-sort PLIST '(lambda(PT1 PT2) (< (distance P1 PT1) (distance P1 PT2)))))
   (setq PLIST (list (car PLISTTMP)))
   (setq PLISTTMP (cdr PLISTTMP))
   (setq PLIST (append PLIST (list (car PLISTTMP))))
   (setq PLIST (vl-sort PLIST '(lambda(PT1 PT2) (< (distance P2 PT1) (distance P2 PT2)))))
   (setq PLISTTMP (cdr PLISTTMP))
   (while (and PLISTTMP (< (last (setq P (RFL:BESTLINE PLIST))) D))
    (setq PLIST2 PLIST)
;    (princ (strcat (rtos (last P) 2 3) "\n"))
    (setq PLIST (append PLIST (list (car PLISTTMP))))
    (setq PLIST (vl-sort PLIST '(lambda(PT1 PT2) (< (distance P2 PT1) (distance P2 PT2)))))
    (setq PLISTTMP (cdr PLISTTMP))
   )
   (if (> (last P) D)
    (progn
     (setq PLIST (reverse (cdr (reverse PLIST))))
     (setq P (RFL:BESTLINE PLIST))
    )
   )
   (if (and P PA) (setq PLIST1 (RFL:GETSUBPLIST PLIST0 PA (car P))))
   (if (and P PB) (setq PLIST2 (RFL:GETSUBPLIST PLIST0 (cadr P) PB)))
   (if P (setq RFL:PL2TANGENTLIST (append RFL:PL2TANGENTLIST (list (list (car P) (cadr P))))))
   (if (>= (length PLIST1) 2) (RFL:PL2TANGENT PLIST1 D))
   (if (>= (length PLIST2) 2) (RFL:PL2TANGENT PLIST2 D))
  )
  nil
 )
)