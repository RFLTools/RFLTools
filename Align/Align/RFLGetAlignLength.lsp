;
;
;     Program written by Robert Livingston, 2016/07/07
;
;     RFL:GETALIGNLENGTH returns the length the alignment defined by RFL:ALIGNLIST
;
;
(if RFL:GETALIGNLENGTH (princ "\nRFL:GETALIGNLENGTH already loaded...")
(defun RFL:GETALIGNLENGTH ()
 (if (= RFL:ALIGNLIST nil)
  (progn
   nil
  )
  (progn
   (- (+ (car (last RFL:ALIGNLIST))
         (RFL:DIST (cadr (last RFL:ALIGNLIST)) (caddr (last RFL:ALIGNLIST)) (cadddr (last RFL:ALIGNLIST)))
      )
      (car (car RFL:ALIGNLIST))
   )
  )
 )
)
)
