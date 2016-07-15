;
;
;   Program written by Robert Livingston, 99/10/08
;
;   RFL:SUPER returns a list (left super , right super) for the given station
;
;
(if RFL:SUPER (princ "\nRFL:SUPER already loaded...")
(defun RFL:SUPER (STA / C NODE1 NODE2 S1 S2 STA1 STA2 VAL)
 (setq VAL nil)
 (if (/= RFL:SUPERLIST nil)
  (progn
   (if (and (>= STA (car (car RFL:SUPERLIST))) (<= STA (car (last RFL:SUPERLIST))))
    (progn
     (setq C 0)
     (while (>= STA (car (nth C RFL:SUPERLIST)))
      (setq C (+ C 1))
     )
     (setq NODE1 (nth (- C 1) RFL:SUPERLIST))
     (setq NODE2 (nth C RFL:SUPERLIST))
     (setq STA1 (car NODE1))
     (setq STA2 (car NODE2))
     (setq S1 (cadr NODE1))
     (setq S2 (cadr NODE2))
     (setq VAL (list (+ S1 (* (- S2 S1) (/ (- STA STA1) (- STA2 STA1))))))
     (setq S1 (caddr NODE1))
     (setq S2 (caddr NODE2))
     (setq VAL (append VAL (list (+ S1 (* (- S2 S1) (/ (- STA STA1) (- STA2 STA1)))))))
    )
   )
  )
 )
 VAL
)
)
