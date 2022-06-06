;
;
;     Program written by Robert Livingston 2022-06-03
;
;     C:PL2TANGENT draws tangents along a polyline that are grater than a minimum length and less than a given offset
;
;
(defun C:PL2TANGENT (/ ENT D L NODE P1 P2 PLIST PREVENT)
 (setq RFL:PL2TANGENTLIST nil)
 (if (= nil (setq D (getdist "\nEnter minimum offset <0.1> : "))) (setq D 0.1))
 (if (= nil (setq L (getdist "\nEnter minimum length <50.0> : "))) (setq L 50.0))
 (setq PLIST (RFL:GETPLIST (car (entsel "\nSelect polyline : "))))
 (RFL:PL2TANGENT PLIST D)
 (foreach NODE RFL:PL2TANGENTLIST
  (progn
   (setq P1 (car NODE))
   (setq P2 (cadr NODE))
   (if (and P1 P2 (>= (distance P1 P2) L))
    (progn
     (entmake (list (cons 0 "LINE")
                    (cons 10 P1)
                    (cons 11 P2)
              )
     )
     (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
    )
   )
  )
 )
 nil
)