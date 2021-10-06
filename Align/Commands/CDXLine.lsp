;
;
;     Program written by Robert Livingston, 2021-07-29
;
;     DXLINE draws a section line (XLINE) at a selected point or at a specified chainage
;
;
(defun C:DXLINE (/ ANG ENTLIST P1 P2 STA)
 (setq ENTLIST T)
 (if RFL:ALIGNLIST
  (while ENTLIST
   (setq P1 (getpoint "\nSelect point or <return> to enter chainage : "))
   (if (or (not P1)
           (not (setq STA (car (RFL:STAOFF P1))))
       )
    (setq STA (getreal "\nEnter chainage : "))
   )
   (if (and STA
            (>= STA (caar RFL:ALIGNLIST))
            (<= STA (+ (caar RFL:ALIGNLIST) (RFL:GETALIGNLENGTH)))
       )
    (progn
     (setq P1 (RFL:XY (list STA 0.0)) P2 (RFL:XY (list STA 1.0)) ANG (angle P1 P2))
     (setq ENTLIST (list (cons 0 "XLINE")
                         (cons 100 "AcDbEntity")
                         (cons 100 "AcDbXline")
                         (append (list 10) P1 (list 0.0))
                         (list 11 (cos ANG) (sin ANG) 0.0)
                   )
     )
     (entmake ENTLIST)
    )
    (progn
     (princ "\nError with section!")
     (setq ENTLIST nil)
    )
   )
  )
  (princ "\nNo alignment defined!")
 )
 nil
)