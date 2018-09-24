;
;
;     Program written by Robert Livingston, 99/04/21
;
;     STRIPZ removes the 'Z' coordinate of a selected set of entities
;
;
(defun C:STRIPZ (/ CMDECHO C CLAYER ENTSET ENT ENT2 ENTLIST NEWENTLIST NODE)
;(defun C:STRIPZ ()
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq CLAYER (getvar "CLAYER"))

 (setq C 0)
 (setq ENTSET (ssget))
 (while (< C (sslength ENTSET))
  (setq ENT (ssname ENTSET C))
  (setq ENTLIST (entget ENT))
  (if (= (cdr (assoc 0 ENTLIST)) "POLYLINE")
   (progn
    (setvar "CLAYER" (cdr (assoc 8 ENTLIST)))
    (RFL:ELEVFIX ENT 0.0)
   )
   (progn
    (setq NEWENTLIST nil)
    (while (/= ENTLIST nil)
     (setq NODE (car ENTLIST))
     (setq ENTLIST (cdr ENTLIST))
     (if (and (>= (car NODE) 10)
              (< (car NODE) 40)
         )
      (progn
       (if (= (length NODE) 4)
        (setq NODE (list (nth 0 NODE) (nth 1 NODE) (nth 2 NODE) 0.0))
       )
      )
     )
     (if (= NEWENTLIST nil)
      (setq NEWENTLIST (list NODE))
      (setq NEWENTLIST (append NEWENTLIST (list NODE)))
     )
    )
    (entmod NEWENTLIST)
    (entupd ENT)
   )
  )
  (setq C (+ C 1))
 )

 (setvar "CMDECHO" CMDECHO)
 (setvar "CLAYER" CLAYER)
)