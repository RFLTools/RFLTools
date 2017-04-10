;
;
;     Program written by Robert Livingston, 2017-04-10
;
;     RFL:MIDPLIST computes a the mid points between 2 point lists
;
;
(defun RFL:MIDPLIST (PLIST1 PLIST2 / 2D D D1 D2 NODE P1 P2 PLAST PLISTM TMP)
 (setq 2D nil)
 (if (or (= nil (caddar PLIST1))
         (= nil (caddar PLIST2))
     )
  (progn
   (setq 2D T)
   (if (caddar PLIST1)
    (progn
     (setq TMP nil)
     (foreach NODE PLIST1
      (setq TMP (append TMP (list (list (car NODE) (cadr NODE)))))
     )
     (setq PLIST1 TMP)
    )
   )
   (if (caddar PLIST2)
    (progn
     (setq TMP nil)
     (foreach NODE PLIST2
      (setq TMP (append TMP (list (list (car NODE) (cadr NODE)))))
     )
     (setq PLIST2 TMP)
    )
   )
  )
 )
 (if (< (distance (car PLIST1) (last PLIST2))
        (distance (car PLIST1) (car PLIST2))
     )
  (setq PLIST2 (reverse PLIST2))
 )
 (setq P1 (car PLIST1)
       PLIST1 (cdr PLIST1)
       P2 (car PLIST2)
       PLIST2 (cdr PLIST2)
 )
 (if 2D
  (setq PLISTM (append PLISTM (list (list (/ (+ (car P1) (car P2)) 2.0) (/ (+ (cadr P1) (cadr P2)) 2.0)))))
  (setq PLISTM (append PLISTM (list (list (/ (+ (car P1) (car P2)) 2.0) (/ (+ (cadr P1) (cadr P2)) 2.0) (/ (+ (caddr P1) (caddr P2)) 2.0)))))
 )
 (if 2D
  (setq PLAST (list (/ (+ (car (last PLIST1)) (car (last PLIST2))) 2.0) (/ (+ (cadr (last PLIST1)) (cadr (last PLIST2))) 2.0)))
  (setq PLAST (list (/ (+ (car (last PLIST1)) (car (last PLIST2))) 2.0) (/ (+ (cadr (last PLIST1)) (cadr (last PLIST2))) 2.0) (/ (+ (caddr (last PLIST1)) (caddr (last PLIST2))) 2.0)))
 )
 (while (and PLIST1 PLIST2)
  (cond ((and (/= nil PLIST1) (= nil PLIST2))
         (setq D nil
               D1 nil
               D2 (distance P2 (car PLIST1))
         )
        )
        ((and (= nil PLIST1) (/= nil PLIST2))
         (setq D nil
               D1 (distance P1 (car PLIST2))
               D2 nil
         )
        )
        (T
         (setq D (distance (car PLIST1) (car PLIST2))
               D1 (distance P1 (car PLIST2))
               D2 (distance P2 (car PLIST1))
         )
        )
  )
  (if D
   (cond ((and (< D1 D) (< D1 D2))
          (progn
           (setq P2 (car PLIST2)
                 PLIST2 (cdr PLIST2)
           )
           (if 2D
            (setq PLISTM (append PLISTM (list (list (/ (+ (car P1) (car P2)) 2.0) (/ (+ (cadr P1) (cadr P2)) 2.0)))))
            (setq PLISTM (append PLISTM (list (list (/ (+ (car P1) (car P2)) 2.0) (/ (+ (cadr P1) (cadr P2)) 2.0) (/ (+ (caddr P1) (caddr P2)) 2.0)))))
           )
          )
         )
         ((and (< D2 D) (< D2 D1))
          (progn
           (setq P1 (car PLIST1)
                 PLIST1 (cdr PLIST1)
           )
           (if 2D
            (setq PLISTM (append PLISTM (list (list (/ (+ (car P1) (car P2)) 2.0) (/ (+ (cadr P1) (cadr P2)) 2.0)))))
            (setq PLISTM (append PLISTM (list (list (/ (+ (car P1) (car P2)) 2.0) (/ (+ (cadr P1) (cadr P2)) 2.0) (/ (+ (caddr P1) (caddr P2)) 2.0)))))
           )
          )
         )
         (T
          (progn
           (setq P1 (car PLIST1)
                 PLIST1 (cdr PLIST1)
                 P2 (car PLIST2)
                 PLIST2 (cdr PLIST2)
           )
           (if 2D
            (setq PLISTM (append PLISTM (list (list (/ (+ (car P1) (car P2)) 2.0) (/ (+ (cadr P1) (cadr P2)) 2.0)))))
            (setq PLISTM (append PLISTM (list (list (/ (+ (car P1) (car P2)) 2.0) (/ (+ (cadr P1) (cadr P2)) 2.0) (/ (+ (caddr P1) (caddr P2)) 2.0)))))
           )
          )
         )
   )
   (if D2
    (progn
     (setq P1 (car PLIST1)
           PLIST1 (cdr PLIST1)
     )
     (if 2D
      (setq PLISTM (append PLISTM (list (list (/ (+ (car P1) (car P2)) 2.0) (/ (+ (cadr P1) (cadr P2)) 2.0)))))
      (setq PLISTM (append PLISTM (list (list (/ (+ (car P1) (car P2)) 2.0) (/ (+ (cadr P1) (cadr P2)) 2.0) (/ (+ (caddr P1) (caddr P2)) 2.0)))))
     )
    )
    (progn
     (setq P2 (car PLIST2)
           PLIST2 (cdr PLIST2)
     )
     (if 2D
      (setq PLISTM (append PLISTM (list (list (/ (+ (car P1) (car P2)) 2.0) (/ (+ (cadr P1) (cadr P2)) 2.0)))))
      (setq PLISTM (append PLISTM (list (list (/ (+ (car P1) (car P2)) 2.0) (/ (+ (cadr P1) (cadr P2)) 2.0) (/ (+ (caddr P1) (caddr P2)) 2.0)))))
     )
    )
   )
  )
 )
 (setq PLISTM (append PLISTM (list PLAST)))
 PLISTM
)