;
;
;     Program written by Robert Livingston, 2020-05-22
;
;     PROFILEGRID2GROUND converts a vertical profile from grid to ground by scaling the stations and curve lengths the invers of the combined factor
;
;
(defun C:PROFILEGRID2GROUND (/ FACTOR PVILIST STARTSTA)
 (if RFL:PVILIST
  (progn
   (setq FACTOR (getreal "\nEnter scale factor : "))
   (setq STASTART (caar RFL:PVILIST))
   (setq PVILIST RFL:PVILIST)
   (setq RFL:PVILIST nil)
   (while PVILIST
    (setq RFL:PVILIST (append RFL:PVILIST (list (list (+ STASTART (* (- (car (car PVILIST)) STASTART) FACTOR))
                                                      (cadr (car PVILIST))
                                                      (caddr (car PVILIST))
                                                      (* (cadddr (car PVILIST)) FACTOR)
                                                )
                                          )
                      )
    )
    (setq PVILIST (cdr PVILIST))
   )
  )
 )
)