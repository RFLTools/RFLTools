;
;
;     Program written by Robert Livingston, 2015-03-13
;
;     RFL:FIX+ modifies a text entity to adjust it's '+' to align with its insertion point.
;
;
(defun RFL:FIX+ (ENT / ANG ANG1 CODE ENTLIST D H P P0 STR TB TB1 TB2 W WL WP WR)
 (setq ENTLIST (entget ENT))
 (if (= "TEXT" (cdr (assoc 0 ENTLIST)))
  (if (/= nil (vl-string-search "+" (setq STR (cdr (assoc 1 ENTLIST)))))
   (progn
    (setq ANG (cdr (assoc 50 ENTLIST)))
    (if (or (/= 0 (cdr (assoc 72 ENTLIST))) (/= 0 (cdr (assoc 73 ENTLIST))))
     (setq CODE 11)
     (setq CODE 10)
    )
    (setq P (cdr (assoc CODE ENTLIST)))
    (setq P0 (cdr (assoc 10 ENTLIST)))
    (setq D (distance P0 P))
    (setq ANG1 (angle P0 P))
    (setq TB (textbox ENTLIST))
    (setq W (- (caadr TB) (caar TB)))
    (setq H (* D (sin (- ANG1 ANG))))
    (setq WP (* D (cos (- ANG1 ANG))))
    (setq TBL (textbox (subst (cons 1 (substr STR 1 (+ (vl-string-search "+" STR) 1))) (assoc 1 ENTLIST) ENTLIST)))
    (setq WL (- (caadr TBL) (caar TBL)))
    (setq TBR (textbox (subst (cons 1 (substr STR (+ (vl-string-search "+" STR) 1))) (assoc 1 ENTLIST) ENTLIST)))
    (setq WR (- (caadr TBR) (caar TBR)))
    (setq W+ (- (+ WR WL) W))
    (setq ENTLIST (subst (list CODE
                               (+ (car P) (* 1.0 (- WP (+ (- WL (/ W+ 2.0)) (caar TBL))) (cos ANG)))
                               (+ (cadr P) (* 1.0 (- WP (+ (- WL (/ W+ 2.0)) (caar TBL))) (sin ANG)))
                               (caddr P)
                         )
                         (assoc CODE ENTLIST)
                         ENTLIST
                  )
    )
    (entmod ENTLIST)
    (entupd ENT)
   )
  )
 )
)
