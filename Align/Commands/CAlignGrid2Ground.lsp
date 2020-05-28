;
;
;     Program written by Robert Livingston, 2020-05-22
;
;     ALIGNGRID2GROUND converts an alignment from grid to ground by scaling the stations, curve lengths and coordinates the inverse of the combined factor
;
;
(defun C:ALIGNGRID2GROUND (/ ALIGNLIST FACTOR STARTSTA)
 (if RFL:ALIGNLIST
  (progn
   (setq FACTOR (getreal "\nEnter scale factor : "))
   (setq STASTART (caar RFL:ALIGNLIST))
   (setq ALIGNLIST RFL:ALIGNLIST)
   (setq RFL:ALIGNLIST nil)
   (while ALIGNLIST
    (setq RFL:ALIGNLIST (append RFL:ALIGNLIST (list (list (+ STASTART (/ (- (car (car ALIGNLIST)) STASTART) FACTOR))
                                                          (list (/ (car (cadr (car ALIGNLIST))) FACTOR)
                                                                (/ (cadr (cadr (car ALIGNLIST))) FACTOR)
                                                          )
                                                          (list (/ (car (caddr (car ALIGNLIST))) FACTOR)
                                                                (/ (cadr (caddr (car ALIGNLIST))) FACTOR)
                                                          )
                                                          (if (listp (cadddr (car ALIGNLIST)))
                                                           (list (list (/ (car (nth 0 (cadddr (car ALIGNLIST)))) FACTOR)
                                                                       (/ (cadr (nth 0 (cadddr (car ALIGNLIST)))) FACTOR)
                                                                 )
                                                                 (list (/ (car (nth 1 (cadddr (car ALIGNLIST)))) FACTOR)
                                                                       (/ (cadr (nth 1 (cadddr (car ALIGNLIST)))) FACTOR)
                                                                 )
                                                                 (list (/ (car (nth 2 (cadddr (car ALIGNLIST)))) FACTOR)
                                                                       (/ (cadr (nth 2 (cadddr (car ALIGNLIST)))) FACTOR)
                                                                 )
                                                                 (/ (nth 3 (cadddr (car ALIGNLIST))) FACTOR)
                                                           )
                                                           (cadddr (car ALIGNLIST))
                                                          )
                                                    )
                                              )
                        )
    )
    (setq ALIGNLIST (cdr ALIGNLIST))
   )
  )
 )
)