;
;
;     Program written by Robert Livingston
;
;
(defun RFL:GETSECTIONSET (STASTART STAEND SWATH STEP OBSURFACE RFL:ALIGNLIST / P1 P2 PLIST SECTIONSET STA SLIST)
 (princ "\nGetting sections : ")
 (setq STA STASTART)
 (while (<= STA STAEND)
  (princ (strcat "\n" (RFL:STATXT STA) "..."))
  (setq P1 (RFL:XY (list STA (/ SWATH -2.0))))
  (setq P2 (RFL:XY (list STA (/ SWATH 2.0))))
  (if (and (/= nil P1) (/= nil P2))
   (progn
    (setq PLIST (RFL:GETSURFACELINE P1 P2 OBSURFACE))
    (setq SLIST nil)
    (foreach NODE PLIST
     (progn
      (setq P (list (car NODE) (cadr NODE)))
      (setq SLIST (append SLIST (list (list (- (distance P1 P) (/ SWATH 2.0)) (last NODE)))))
     )
    )
    (setq SECTIONSET (append SECTIONSET (list (list STA SLIST))))
   )
  )
  (setq STA (+ STA STEP))
 )
 SECTIONSET
)
