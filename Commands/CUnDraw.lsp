;
;
;     Program written by Robert Livingston, 02/09/05
;
;     UNDRAW makes the selected entity invisible
;
;
(defun C:UNUNDRAW (/ C)
 (if (/= RFL:UNDRAWSET nil)
  (progn
   (setq C 0)
   (while (< C (sslength RFL:UNDRAWSET))
    (redraw (ssname RFL:UNDRAWSET C) 1)
    (setq C (1+ C))
   )
   (setq RFL:UNDRAWSET nil)
  )
 )
)
(defun C:UNDRAW (/ C ENT ENTSET FLAG)
 (setq FLAG 0)
 (while (= FLAG 0)
  (if (/= (setq ENT (car (entsel))) nil)
   (progn
    (if (= RFL:UNDRAWSET nil) (setq RFL:UNDRAWSET (ssadd)))
    (ssadd ENT RFL:UNDRAWSET)
    (redraw ENT 2)
   )
   (if (= 0 (sslength (setq ENTSET (ssget))))
    (setq FLAG 1)
    (progn
     (setq C 0)
     (while (< C (sslength ENTSET))
      (setq ENT (ssname ENTSET C))
      (if (= RFL:UNDRAWSET nil) (setq RFL:UNDRAWSET (ssadd)))
      (ssadd ENT RFL:UNDRAWSET)
      (redraw ENT 2)
      (setq C (1+ C))
     )
    )
   )
  )
 )
)