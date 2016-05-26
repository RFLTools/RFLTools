;
;
;     Program written by Robert Livingston, 2016-05-26
;
;     Collection of common routines, functions and constants
;
;
(setq RFL:TOL 0.000001
      RFL:TOLFINE 1e-16
)
(defun RFL:ANGLE3P (P1 P2 P3 / ANG)
 (setq ANG (- (angle P2 P1) (angle P2 P3)))
 (if (< ANG 0.0) (setq ANG (* -1.0 ANG)))
 (if (> ANG pi) (setq ANG (- (* 2.0 pi) ANG)))
 (eval ANG)
)
(defun RFL:FACT (N / F)
 (setq F 1)
 (while (> N 0)
  (setq F (* F N))
  (setq N (- N 1))
 )
 F
)
(defun RFL:SIGN (X)
 (if (< X 0.0)
  -1.0
  1.0
 )
)
(defun RFL:TAN (X)
 (/ (sin X) (cos X))
)
(defun RFL:CENTER (P1 P2 BULGE / ANG ATOTAL CHORD D R X Y)
 (setq ATOTAL (* 4.0 (atan (abs BULGE))))
 (setq CHORD (distance P1 P2))
 (if (< (abs BULGE) RFL:TOLFINE)
  nil
  (progn
   (setq R (/ CHORD (* 2 (sin (/ ATOTAL 2)))))
   (setq ANG (angle P1 P2))
   (setq D (distance P1 P2))
   (setq X (/ D 2.0))
   (setq Y (* (sqrt (- (* R R) (* X X))) (RFL:SIGN BULGE) (RFL:SIGN (- (abs BULGE) 1.0))))
   (list (+ (+ (car P1) (* X (cos ANG))) (* Y (sin ANG)))
         (- (+ (cadr P1) (* X (sin ANG))) (* Y (cos ANG)))
   )
  )
 )
)
(defun RFL:RADIUS (P1 P2 BULGE / ATOTAL CHORD)
 (setq ATOTAL (* 4.0 (atan (abs BULGE))))
 (setq CHORD (distance P1 P2))
 (if (< (abs BULGE) RFL:TOLFINE)
  nil
  (/ CHORD (* 2 (sin (/ ATOTAL 2))))
 )
)
;
;
;   Program written by Robert Livingston, 98/06/12
;
;   RFL:ALIGNDEF returns an RFL Alignment list based on either a single polyline of a set of entities.
;
;
;
(defun RFL:ALIGNDEF (ALIGNENT PSTART STASTART / ALIGNENTLIST ALIGNENTSET AL BULGE FINDENT P P1 P2 R RFLAG)
 (setq AL nil)
 (setq RFL:TOL 0.000001)
 (setq RFLAG 1.0)
 (setq STA STASTART)
 (defun FINDENT (P ALIGNENTSET / ANG ANG1 ANG2 BULGE C ENT ENT2 ENT3 ENTLIST FOUND L P1 P2 PC R TMP)
  (setq C 0)
  (setq ENT nil)
  (setq FOUND 0)
  (while (and (= FOUND 0) (< C (sslength ALIGNENTSET)))
   (setq ENT2 (ssname ALIGNENTSET C))
   (setq ENTLIST (entget ENT2))
   (if (= (cdr (assoc 0 ENTLIST)) "LINE")
    (progn
     (setq P1 (cdr (assoc 10 ENTLIST)))
     (setq P1 (list (car P1) (cadr P1)))
     (setq P2 (cdr (assoc 11 ENTLIST)))
     (setq P2 (list (car P2) (cadr P2)))
     (setq L (distance P1 P2))
     (setq BULGE 0.0)
     (if (< (distance P P1) RFL:TOL)
      (progn
       (setq FOUND 1)
       (setq ENT ENT2)
      )
     )
     (if (< (distance P P2) RFL:TOL)
      (progn
       (setq FOUND 1)
       (setq ENT ENT2)
       (setq TMP P1)
       (setq P1 P2)
       (setq P2 TMP)
      )
     )
    )
   )
   (if (= (cdr (assoc 0 ENTLIST)) "ARC")
    (progn
     (setq PC (cdr (assoc 10 ENTLIST)))
     (setq R (cdr (assoc 40 ENTLIST)))
     (setq ANG1 (cdr (assoc 50 ENTLIST)))
     (setq P1 (list (+ (car PC) (* R (cos ANG1)))
                    (+ (cadr PC) (* R (sin ANG1)))))
     (setq ANG2 (cdr (assoc 51 ENTLIST)))
     (setq P2 (list (+ (car PC) (* R (cos ANG2)))
                    (+ (cadr PC) (* R (sin ANG2)))))
     (setq ANG (- ANG2 ANG1))
     (if (< ANG 0.0)
      (setq ANG (+ ANG (* 2.0 pi)))
     )
     (setq L (* R ANG))
     (setq BULGE (RFL:TAN (/ ANG 4.0)))

     (if (< (distance P P1) RFL:TOL)
      (progn
       (setq FOUND 1)
       (setq ENT ENT2)
      )
     )
     (if (< (distance P P2) RFL:TOL)
      (progn
       (setq FOUND 1)
       (setq ENT ENT2)
       (setq BULGE (* -1.0 BULGE))
       (setq TMP P1)
       (setq P1 P2)
       (setq P2 TMP)
      )
     )
    )
   )
   (if (= (cdr (assoc 0 ENTLIST)) "POLYLINE")
    (progn
     (setq L (RFL:GETSPIRALLS ENT2))
     (if (/= L nil)
      (progn
       (setq ENT3 (entnext ENT2))
       (setq ENTLIST (entget ENT3))
       (setq P1 (cdr (assoc 10 ENTLIST)))
       (setq P1 (list (car P1) (cadr P1)))
       (setq ENT3 (entnext ENT3))
       (setq ENTLIST (entget ENT3))
       (while (/= (cdr (assoc 0 ENTLIST)) "SEQEND")
        (setq P2 (cdr (assoc 10 ENTLIST)))
        (setq P2 (list (car P2) (cadr P2)))
        (setq ENT3 (entnext ENT3))
        (setq ENTLIST (entget ENT3))
       )
       (setq BULGE (RFL:GETSPIRALDATA ENT2))
       (setq L (- L (last BULGE)))
       (if (< (distance P P1) RFL:TOL)
        (progn
         (setq FOUND 1)
         (setq ENT ENT2)
        )
       )
       (if (< (distance P P2) RFL:TOL)
        (progn
         (setq FOUND 1)
         (setq ENT ENT2)
         (setq TMP P1)
         (setq P1 P2)
         (setq P2 TMP)
        )
       )
      )
     )
    )
   )
   (if (= (cdr (assoc 0 ENTLIST)) "LWPOLYLINE")
    (progn
     (setq L (RFL:GETSPIRALLS ENT2))
     (if (/= L nil)
      (progn
       (setq P1 (cdr (assoc 10 ENTLIST)))
       (setq P2 (cdr (assoc 10 (reverse ENTLIST))))
       (setq BULGE (RFL:GETSPIRALDATA ENT2))
       (setq L (- L (last BULGE)))
       (if (< (distance P P1) RFL:TOL)
        (progn
         (setq FOUND 1)
         (setq ENT ENT2)
        )
       )
       (if (< (distance P P2) RFL:TOL)
        (progn
         (setq FOUND 1)
         (setq ENT ENT2)
         (setq TMP P1)
         (setq P1 P2)
         (setq P2 TMP)
        )
       )
      )
     )
    )
   )
   (setq C (+ C 1))
  )
  (if (= FOUND 0)
   (eval nil)
   (list ENT P1 P2 BULGE L)
  )
 )
 (if (listp ALIGNENT)
  (progn
   (setq ALIGNENTSET (car ALIGNENT))
   (setq P PSTART)
   (while (/= (setq ALIGNENT (FINDENT P ALIGNENTSET)) nil)
    (setq ALIGNENTSET (ssdel (car ALIGNENT) ALIGNENTSET))
    (setq P1 (cadr ALIGNENT))
    (setq P2 (caddr ALIGNENT))
    (setq BULGE (cadddr ALIGNENT))
    (setq AL (append AL (list (list STA P1 P2 BULGE))))
    (setq STA (+ STA (nth 4 ALIGNENT)))
    (setq P P2)
   )
  )
  (progn
   (setq ALIGNENTLIST (entget ALIGNENT))
   (if (= (cdr (assoc 0 ALIGNENTLIST)) "LWPOLYLINE")
    (progn
     (setq P1 (cdr (assoc 10 ALIGNENTLIST)))
     (if (> (distance P1 PSTART) RFL:TOL)
      (progn
       (setq RFLAG -1.0)
       (setq ALIGNENTLIST (reverse ALIGNENTLIST))
      )
     )
     (setq P1 (cdr (assoc 10 ALIGNENTLIST)))
     (if (< (distance P1 PSTART) RFL:TOL)
      (progn
       (while (/= (car (car ALIGNENTLIST)) 10)
        (setq ALIGNENTLIST (cdr ALIGNENTLIST))
       )
       (setq P1 (cdr (car ALIGNENTLIST)))
       (setq P1 (list (car P1) (cadr P1)))
       (setq ALIGNENTLIST (cdr ALIGNENTLIST))
       (setq BULGE 0.0)
       (while (/= ALIGNENTLIST nil)
        (cond ((= (car (car ALIGNENTLIST)) 42)
               (setq BULGE (* RFLAG (cdr (car ALIGNENTLIST))))
              )
              ((= (car (car ALIGNENTLIST)) 10)
               (progn
                (setq P2 (cdr (car ALIGNENTLIST)))
                (setq P2 (list (car P2) (cadr P2)))
                (setq AL (append AL (list (list STA P1 P2 BULGE))))
                (setq STA (+ STA (RFL:ARCLENGTH P1 P2 BULGE)))
                (setq P1 P2)
                (setq BULGE 0.0)
               )
              )
        )
        (setq ALIGNENTLIST (cdr ALIGNENTLIST))
       )
      )
      (princ "\n**** POINT NOT AT START OF ALIGNMENT ****")
     )
    )
    (progn
     (eval nil)
    )
   )
  )
 )
 (setq AL AL)
)
;
;
;     Program written by Robert Livingston, 98/06/12
;
;     RFL:ARCLENGTH returns the length of an arc defined by 2 points and a bulge
;
;
(defun RFL:ARCLENGTH (P1 P2 BULGE / ATOTAL CHORD R)
 (setq ATOTAL (* 4 (atan (abs BULGE)))
       CHORD (distance P1 P2)
 )
 (if (= 0.0 BULGE)
  CHORD
  (progn 
   (setq R (/ CHORD (* 2 (sin (/ ATOTAL 2)))))
   (* R ATOTAL)
  )
 )
)
(defun RFL:AXY (AL STA SWATH / ALSAVE ENTLIST OFFSET1 OFFSET2 OFFSET3 P1 P2 P3)
 (setq ENTLIST (entget ENT))
 (if (= (cdr (assoc 0 ENTLIST)) "LWPOLYLINE")
  (progn
   (setq P1 (XY (list STA (/ SWATH -2.0))))
   (setq P2 (XY (list STA (/ SWATH 2.0))))
   (if (and (/= P1 nil) (/= P2 nil))
    (progn
     (setq ALSAVE ALIGNLIST)
     (setq ALIGNLIST AL)
     (if (= nil ALIGNLIST)
      (progn
       (setq ALIGNLIST ALSAVE)
       (eval nil)
      )
      (progn
       (setq OFFSET1 (STAOFF P1))
       (setq OFFSET2 (STAOFF P2))
       (setq P3 (list (/ (+ (car P1) (car P2)) 2.0) (/ (+ (cadr P1) (cadr P2)) 2.0)))
       (setq OFFSET3 (STAOFF P3))
       (if (= OFFSET1 nil)
        (progn
         (setq P1 P3)
         (setq OFFSET1 OFFSET3)
        )
       )
       (if (= OFFSET2 nil)
        (progn
         (setq P2 P3)
         (setq OFFSET2 OFFSET3)
        )
       )
       (if (and (/= OFFSET1 nil) (/= OFFSET2 nil))
        (progn
         (setq OFFSET1 (cadr OFFSET1))
         (setq OFFSET2 (cadr OFFSET2))
         (if (> (* OFFSET1 OFFSET2) 0.0)
          (progn
           (setq ALIGNLIST ALSAVE)
           (eval nil)
          )
          (progn
           (while (> (distance P1 P2) RFL:TOL)
            (setq P3 (list (/ (+ (car P1) (car P2)) 2.0) (/ (+ (cadr P1) (cadr P2)) 2.0)))
            (setq OFFSET3 (cadr (STAOFF P3)))
            (if (> (* OFFSET1 OFFSET3) 0.0)
             (setq P1 P3)
             (setq P2 P3)
            )
           )
           (setq ALIGNLIST ALSAVE)
           (setq P3 P3)
          )
         )
        )
        (progn
         (eval nil)
        )
       )
      )
     )
    )
    (progn
     (eval nil)
    )
   )
  )
  (progn
   (eval nil)
  )
 )
)
;
;
;   Program written by Robert Livingston, 98/06/12
;
;   RFL:DRAWALIGN draws the current alignmnet for alignments without spirals
;
;
;
(defun RFL:DRAWALIGN (/ ALLIST ALENT ENTLIST)
 (setq ALLIST ALIGNLIST)
 (entmake)
 (setq ENTLIST (list (cons 0 "POLYLINE")
                     (cons 66 1)))
 (entmake ENTLIST)
 (while (/= ALLIST nil)
  (setq ALENT (car ALLIST))
  (setq ALLIST (cdr ALLIST))
  (setq ENTLIST (list (cons 0 "VERTEX")
                      (append (list 10) (nth 1 ALENT))
                      (cons 42 (nth 3 ALENT))
                )
  )
  (entmake ENTLIST)
  (if (= ALLIST nil)
   (progn
    (setq ENTLIST (list (cons 0 "VERTEX")
                        (append (list 10) (nth 2 ALENT))
                  )
    )
    (entmake ENTLIST)
   )
  )
 )
 (setq ENTLIST (list (cons 0 "SEQEND")))
 (entmake ENTLIST)
 (command "._convert" "P" "S" (entlast) "")
);
;
;   Program written by Robert Livingston, 98/06/12
;
;   RFL:DRAWALIGN2 draws the current alignmnet
;
;
;
(defun RFL:DRAWALIGN2 (/ ANG1 ANG2 ALLIST ALENT ENTLIST PC R)
 (setq ALLIST ALIGNLIST)
 (entmake)
 (while (/= ALLIST nil)
  (setq ALENT (car ALLIST))
  (setq ALLIST (cdr ALLIST))
  (if (listp (last ALENT))
   (progn
    (RFL:DRAWSPIRAL (nth 0 (last ALENT)) (nth 1 (last ALENT)) (nth 2 (last ALENT)) (nth 3 (last ALENT)) 0.0)
   )
   (progn
    (if (> (abs (last ALENT)) RFL:TOLFINE)
     (progn
      (setq PC (RFL:CENTER (nth 1 ALENT) (nth 2 ALENT) (nth 3 ALENT)))
      (setq R (RFL:RADIUS (nth 1 ALENT) (nth 2 ALENT) (nth 3 ALENT)))
      (if (< (last ALENT) 0.0)
       (progn
        (setq ANG2 (angle PC (nth 1 ALENT)))
        (setq ANG1 (angle PC (nth 2 ALENT)))
       )
       (progn
        (setq ANG1 (angle PC (nth 1 ALENT)))
        (setq ANG2 (angle PC (nth 2 ALENT)))
       )
      )
      (setq ENTLIST (list (cons 0 "ARC")
                          (list 10 (nth 0 PC) (nth 1 PC) 0.0)
                          (cons 40 R)
                          (cons 50 ANG1)
                          (cons 51 ANG2)
                    )
      )
      (entmake ENTLIST)
     )
     (progn
      (setq ENTLIST (list (cons 0 "LINE")
                          (list 10 (nth 0 (nth 1 ALENT)) (nth 1 (nth 1 ALENT)) 0.0)
                          (list 11 (nth 0 (nth 2 ALENT)) (nth 1 (nth 2 ALENT)) 0.0)
                    )
      )
      (entmake ENTLIST)
     )
    )
   )
  )
 )
)
;
;
;   Program written by Robert Livingston, 98/06/12
;
;   RFL:DRAWALIGNOS draws the current alignmnet at the specified offset
;
;
;
(defun RFL:DRAWALIGNOS (OS / ALLIST ALENT ENTLIST)
 (RFL:DRAWALIGNOS2 OS)
)
;
;
;   Program written by Robert Livingston, 98/06/12
;
;   RFL:DRAWALIGNOS2 draws the current alignmnet at the specified offset
;
;
;
(defun RFL:DRAWALIGNOS2 (OS / ANG ANG1 ANG2 ALLIST ALENT ENTLIST OS2 P1X P1Y P2X P2Y PC R)
 (setq ALLIST ALIGNLIST)
 (entmake)
 (while (/= ALLIST nil)
  (setq ALENT (car ALLIST))
  (setq ALLIST (cdr ALLIST))
  (if (listp (last ALENT))
   (progn
    (if (< (distance (nth 2 ALENT) (nth 2 (last ALENT))) (distance (nth 1 ALENT) (nth 2 (last ALENT))))
     (setq OS2 OS)
     (setq OS2 (* -1.0 OS))
    )
    (RFL:DRAWSPIRAL (nth 0 (last ALENT)) (nth 1 (last ALENT)) (nth 2 (last ALENT)) (nth 3 (last ALENT)) OS2)
   )
   (progn
    (if (> (abs (last ALENT)) RFL:TOLFINE)
     (progn
      (setq PC (RFL:CENTER (nth 1 ALENT) (nth 2 ALENT) (nth 3 ALENT)))
      (setq R (RFL:RADIUS (nth 1 ALENT) (nth 2 ALENT) (nth 3 ALENT)))
      (if (> (last ALENT) 0.0)
       (progn
        (setq OS2 OS)
        (setq ANG1 (angle PC (nth 1 ALENT)))
        (setq ANG2 (angle PC (nth 2 ALENT)))
       )
       (progn
        (setq OS2 (* -1.0 OS))
        (setq ANG2 (angle PC (nth 1 ALENT)))
        (setq ANG1 (angle PC (nth 2 ALENT)))
       )
      )
      (setq ENTLIST (list (cons 0 "ARC")
                          (list 10 (nth 0 PC) (nth 1 PC) 0.0)
                          (cons 40 (+ R OS2))
                          (cons 50 ANG1)
                          (cons 51 ANG2)
                    )
      )
      (entmake ENTLIST)
     )
     (progn
      (setq ANG (angle (nth 1 ALENT) (nth 2 ALENT)))
      (setq P1X (+ (nth 0 (nth 1 ALENT)) (* OS (sin ANG))))
      (setq P1Y (- (nth 1 (nth 1 ALENT)) (* OS (cos ANG))))
      (setq P2X (+ (nth 0 (nth 2 ALENT)) (* OS (sin ANG))))
      (setq P2Y (- (nth 1 (nth 2 ALENT)) (* OS (cos ANG))))
      (setq ENTLIST (list (cons 0 "LINE")
                          (list 10 P1X P1Y 0.0)
                          (list 11 P2X P2Y 0.0)
                    )
      )
      (entmake ENTLIST)
     )
    )
   )
  )
 )
)
;
;
;   Program written by Robert Livingston, 98/06/12
;
;   RFL:GETRADIUS returns the radius at a specified station
;
;
;
(defun RFL:GETRADIUS (STA / AL C DIR R)
 (if (/= nil ALIGNLIST)
  (progn
   (setq AL (last ALIGNLIST))
   (if (<= STA (+ (car AL) (RFL:ARCLENGTH (cadr AL) (caddr AL) (cadddr AL))))
    (progn
     (setq C 0)
     (setq AL (nth C ALIGNLIST))
     (if (>= STA (car AL))
      (progn
       (while (> STA (+ (car AL) (RFL:ARCLENGTH (cadr AL) (caddr AL) (cadddr AL))))
        (setq C (+ C 1))
        (setq AL (nth C ALIGNLIST))
       )
       (if (listp (cadddr AL))
        (progn
         (if (< (distance (caddr AL) (caddr (cadddr AL))) (distance (cadr AL) (caddr (cadddr AL))))
          (progn
           (setq R (GETSPIRALRADIUS (+ (- STA
                                          (car AL)
                                       )
                                       (cadddr (cadddr AL))
                                    )
                               (car (cadddr AL))
                               (cadr (cadddr AL))
                               (caddr (cadddr AL))
                   )
           )
          )
          (progn
           (setq R (* -1.0
                      (GETSPIRALRADIUS (- (GETSPIRALLS2 (car (cadddr AL))
                                                        (cadr (cadddr AL))
                                                        (caddr (cadddr AL))
                                          )
                                          (- STA
                                             (car AL)
                                          )
                                       )
                                       (car (cadddr AL))
                                       (cadr (cadddr AL))
                                       (caddr (cadddr AL))
                   )
                   )
           )
          )
         )
        )
        (progn
         (if (< (abs (cadddr AL)) RFL:TOL)
          (progn
           (setq R 0.0)
          )
          (progn
           (setq DIR (RFL:SIGN (cadddr AL)))
           (setq R (* DIR (RFL:RADIUS (cadr AL) (caddr AL) (cadddr AL))))
          )
         )
        )
       )
      )
      (progn
       (princ "\n**** STATION OUT OF RANGE ****")
       (eval nil)
      )
     )
    )
    (progn
     (princ "\n**** STATION OUT OF RANGE ****")
     (eval nil)
    )
   )
  )
  (progn
   (princ "\n**** NO ALIGNMENT DEFINED ****")
   (eval nil)
  )
 )
)
(defun RFL:INTERSA (P1 P2 P3 P4 BULGE / ANG1 ANG2 D D1 D2 D3 D4 OFFSET PA PB PCEN R)
 (setq P1 (list (car P1) (cadr P1)))
 (setq P2 (list (car P2) (cadr P2)))
 (setq P3 (list (car P3) (cadr P3)))
 (setq P4 (list (car P4) (cadr P4)))
 (if (< (abs BULGE) RFL:TOL)
  (progn
   (setq PA (inters P1 P2 P3 P4 nil))
   (if (/= PA nil)
    (progn
     (if (or (> (distance P1 PA) (distance P1 P2))
             (> (distance P2 PA) (distance P1 P2))
         )
      (progn
       (setq PA nil)
      )
     )
    )
   )
   (setq PB PA)
  )
  (progn
   (setq PCEN (CENTER P3 P4 BULGE))
   (setq R (RFL:RADIUS P3 P4 BULGE))
   (setq D1 (distance P1 PCEN))
   (setq D2 (distance PCEN P2))
   (setq D (distance P1 P2))
   (setq D3 (/ (+ (- (* D1 D1) (* D2 D2)) (* D D))
               (* 2.0 D)
            )
   )
   (setq D4 (/ (+ (- (* D2 D2) (* D1 D1)) (* D D))
               (* 2.0 D)
            )
   )
   (setq OFFSET (sqrt (abs (- (* D1 D1) (* D3 D3)))))
   (if (> OFFSET (+ R RFL:TOL))
    (progn
     (setq PA nil)
     (setq PB PA)
    )
    (progn
     (if (and (<= OFFSET (+ R RFL:TOL)) (>= OFFSET (- R RFL:TOL)))
      (progn
       (setq PA (list (+ (car P1) (* D3 (/ (- (car P2) (car P1)) D)))
                      (+ (cadr P1) (* D3 (/ (- (cadr P2) (cadr P1)) D)))
                )
       )
       (setq PB PA)
      )
      (progn
       (setq D5 (- D3 (sqrt (- (* R R) (* OFFSET OFFSET)))))
       (setq PA (list (+ (car P1) (* D5 (/ (- (car P2) (car P1)) D)))
                      (+ (cadr P1) (* D5 (/ (- (cadr P2) (cadr P1)) D)))
                )
       )
       (setq D6 (+ D3 (sqrt (- (* R R) (* OFFSET OFFSET)))))
       (setq PB (list (+ (car P1) (* D6 (/ (- (car P2) (car P1)) D)))
                      (+ (cadr P1) (* D6 (/ (- (cadr P2) (cadr P1)) D)))
                )
       )
      )
     )
     (if (< BULGE 0.0)
      (setq ANG1 (- (angle PCEN P3) (angle PCEN PA)))
      (setq ANG1 (- (angle PCEN PA) (angle PCEN P3)))
     )
     (while (< ANG1 0.0) (setq ANG1 (+ ANG1 (* 2.0 pi))))
     (if (< BULGE 0.0)
      (setq ANG2 (- (angle PCEN P3) (angle PCEN P4)))
      (setq ANG2 (- (angle PCEN P4) (angle PCEN P3)))
     )
     (while (< ANG2 0.0) (setq ANG2 (+ ANG2 (* 2.0 pi))))
     (if (> ANG1 ANG2)
      (progn
       (setq PA nil)
      )
     )
     (if (< BULGE 0.0)
      (setq ANG1 (- (angle PCEN P3) (angle PCEN PB)))
      (setq ANG1 (- (angle PCEN PB) (angle PCEN P3)))
     )
     (while (< ANG1 0.0) (setq ANG1 (+ ANG1 (* 2.0 pi))))
     (if (< BULGE 0.0)
      (setq ANG2 (- (angle PCEN P3) (angle PCEN P4)))
      (setq ANG2 (- (angle PCEN P4) (angle PCEN P3)))
     )
     (while (< ANG2 0.0) (setq ANG2 (+ ANG2 (* 2.0 pi))))
     (if (> ANG1 ANG2)
      (progn
       (setq PB nil)
      )
     )
    )
   )
  )
 )
 (list PA PB)
)
;
;
;   Program written by Robert Livingston, 98/06/11
;
;   RFL:RALIGN reads a horizontal alignment from the specifiedfile
;
;
(defun RFL:RALIGN (INFILENAME / ANGBASE ANGDIR CMDECHO INFILE INLINE LO P1X P1Y P2X P2Y
                                PLTX PLTY PLTSTX PLTSTY PSTX PSTY BULGE)
 (if (/= INFILENAME nil) (setq INFILENAME (findfile INFILENAME)))
 (if (/= INFILENAME nil)
  (progn
   (vl-registry-write "HKEY_CURRENT_USER\\rflAlignDirectory" "" (strcat (vl-filename-directory INFILENAME) "\\"))
   (setq INFILE (open INFILENAME "r"))
   (setq ALIGNLIST nil)
   (setq INLINE (read-line INFILE))
   (if (/= INLINE "#RFL HORIZONTAL ALIGNMENT FILE")
    (progn
     (princ "\n*** FILE NOT FORMATTED CORRECTLY ***\n")
    )
    (progn
     (setq INLINE (read-line INFILE))
     (while (and (/= nil INLINE) (/= INLINE "#END DEFINITION"))
      (setq STA (atof INLINE))
      (setq INLINE (read-line INFILE))
      (setq P1X (atof INLINE))
      (setq INLINE (read-line INFILE))
      (setq P1Y (atof INLINE))
      (setq INLINE (read-line INFILE))
      (setq P2X (atof INLINE))
      (setq INLINE (read-line INFILE))
      (setq P2Y (atof INLINE))
      (setq INLINE (read-line INFILE))
      (if (= INLINE "SPIRAL")
       (progn
        (setq INLINE (read-line INFILE))
        (setq PLTX (atof INLINE))
        (setq INLINE (read-line INFILE))
        (setq PLTY (atof INLINE))
        (setq INLINE (read-line INFILE))
        (setq PLTSTX (atof INLINE))
        (setq INLINE (read-line INFILE))
        (setq PLTSTY (atof INLINE))
        (setq INLINE (read-line INFILE))
        (setq PSTX (atof INLINE))
        (setq INLINE (read-line INFILE))
        (setq PSTY (atof INLINE))
        (setq INLINE (read-line INFILE))
        (setq LO (atof INLINE))
        (setq BULGE (list (list PLTX PLTY) (list PLTSTX PLTSTY) (list PSTX PSTY) LO))
       )
       (progn
        (setq BULGE (atof INLINE))
       )
      )
      (setq INLINE (read-line INFILE))
      (setq ALIGNLIST (append ALIGNLIST (list (list STA (list P1X P1Y) (list P2X P2Y) BULGE))))
     )
    )
   )
   (close INFILE)
  )
 )
);
;
;   Program written by Robert Livingston, 98/06/11
;
;   RFL:WALIGN writes a horizontal alignment to the specifiedfile
;
;
(defun RFL:WALIGN (OUTFILENAME / C OUTFILE)
 (if (/= OUTFILENAME nil)
  (progn
   (if (/= ".HOR" (strcase (substr OUTFILENAME (- (strlen OUTFILENAME) 3))))
    (setq OUTFILENAME (strcat OUTFILENAME ".HOR"))
   )
   (vl-registry-write "HKEY_CURRENT_USER\\rflAlignDirectory" "" (strcat (vl-filename-directory OUTFILENAME) "\\"))
   (setq C 0)
   (while (and (= nil (setq OUTFILE (open OUTFILENAME "w"))) (< C 5))
    (setq C (+ C 1))
    (princ (strcat "\nProblem openning file for writing : " (itoa C)))
   )
   (if (= nil OUTFILE)
    (alert (strcat "Error openning file for writing : " OUTFILENAME))
    (progn
     (princ "#RFL HORIZONTAL ALIGNMENT FILE\n" OUTFILE)
     (setq C 0)
     (while (< C (length ALIGNLIST))
      (princ (rtos (nth 0 (nth C ALIGNLIST)) 2 16) OUTFILE)
      (princ "\n" OUTFILE)
      (princ (rtos (nth 0 (nth 1 (nth C ALIGNLIST))) 2 16) OUTFILE)
      (princ "\n" OUTFILE)
      (princ (rtos (nth 1 (nth 1 (nth C ALIGNLIST))) 2 16) OUTFILE)
      (princ "\n" OUTFILE)
      (princ (rtos (nth 0 (nth 2 (nth C ALIGNLIST))) 2 16) OUTFILE)
      (princ "\n" OUTFILE)
      (princ (rtos (nth 1 (nth 2 (nth C ALIGNLIST))) 2 16) OUTFILE)
      (princ "\n" OUTFILE)
      (if (listp (nth 3 (nth C ALIGNLIST)))
       (progn
        (princ "SPIRAL\n" OUTFILE)
        (princ (rtos (nth 0 (nth 0 (nth 3 (nth C ALIGNLIST)))) 2 16) OUTFILE)
        (princ "\n" OUTFILE)
        (princ (rtos (nth 1 (nth 0 (nth 3 (nth C ALIGNLIST)))) 2 16) OUTFILE)
        (princ "\n" OUTFILE)
        (princ (rtos (nth 0 (nth 1 (nth 3 (nth C ALIGNLIST)))) 2 16) OUTFILE)
        (princ "\n" OUTFILE)
        (princ (rtos (nth 1 (nth 1 (nth 3 (nth C ALIGNLIST)))) 2 16) OUTFILE)
        (princ "\n" OUTFILE)
        (princ (rtos (nth 0 (nth 2 (nth 3 (nth C ALIGNLIST)))) 2 16) OUTFILE)
        (princ "\n" OUTFILE)
        (princ (rtos (nth 1 (nth 2 (nth 3 (nth C ALIGNLIST)))) 2 16) OUTFILE)
        (princ "\n" OUTFILE)
        (princ (rtos (nth 3 (nth 3 (nth C ALIGNLIST))) 2 16) OUTFILE)
       )
       (progn
        (princ (rtos (nth 3 (nth C ALIGNLIST)) 2 16) OUTFILE)
       )
      )
      (princ "\n" OUTFILE)
      (setq C (+ C 1))
     )
     (princ "#END DEFINITION\n" OUTFILE)
     (close OUTFILE)
    )
   )
  )
 )
)
;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:DRAWSPIRAL draws a reverse engineered DCA spiral
;
;
(defun RFL:DRAWSPIRAL (PLT PLTST PST LO OS / ANG BULGE C D DIR ENTLIST ENTLISTX H
                                             L LS PT PT2 PT3 R RMAX THETA THETAMAX V X Y)
 (if (= (tblsearch "APPID" "DCA_FIGURE_XENT") nil)
  (regapp "DCA_FIGURE_XENT")
 )
 (setq ANG (angle PLT PLTST))
 (if (> (sin (- (angle PLTST PST) (angle PLT PLTST))) 0.0)
  (setq DIR 1.0)
  (setq DIR -1.0)
 )
 (setq THETAMAX (RFL:GETSPIRALTHETA2 PLT PLTST PST))
 (setq LS (RFL:GETSPIRALLS2 PLT PLTST PST))
 (setq V 10.0)
 (setq ENTLISTX (list -3 (list "DCA_FIGURE_XENT"
                               (cons 1070 200)
                               (cons 1070 400)
                               (cons 1070 600)
                               (list 1011 (car PLT) (cadr PLT) 0.0)
                               (cons 1070 601)
                               (list 1011 (car PLTST) (cadr PLTST) 0.0)
                               (cons 1070 602)
                               (list 1011 (car PST) (cadr PST) 0.0)
                               (cons 1070 300)
                               (cons 1040 LO)
                         )
                )
 )
 (setq ENTLIST (list (cons 0 "LWPOLYLINE")
                     (cons 100 "AcDbEntity")
                     (cons 100 "AcDbPolyline")
                     (cons 90 (fix (+ V 1.0)))
                     (cons 43 0.0)
                     (cons 70 128)
               )
 )
 (setq RMAX (RFL:GETSPIRALR2 PLT PLTST PST))
 (setq C 0.0)
 (while (< C (+ V 1.0))
  (setq L (+ LO (* (/ C V) (- LS LO))))
  (if (= L 0.0)
   (progn
    (setq THETA 0.0)
    (setq R 0.0)
    (setq X 0.0)
    (setq Y 0.0)
   )
   (progn
    (setq THETA (* THETAMAX (expt (/ L LS) 2)))
    (setq R (* RMAX (/ LS L)))
    (setq X (* R (RFL:SPIRALFXR THETA)))
    (setq Y (* DIR R (RFL:SPIRALFYR THETA)))
   )
  )
  (setq X (+ X (* OS DIR (sin THETA))))
  (setq Y (- Y (* OS (cos THETA))))
  (setq PT (list (+ (car PLT) (* X (cos ANG)) (* -1.0 Y (sin ANG)))
                 (+ (cadr PLT) (* X (sin ANG)) (* Y (cos ANG)))
           )
  )
  (setq L (+ LO (* (/ (+ C 0.5) V) (- LS LO))))
  (if (= L 0.0)
   (progn
    (setq THETA 0.0)
    (setq R 0.0)
    (setq X 0.0)
    (setq Y 0.0)
   )
   (progn
    (setq THETA (* THETAMAX (expt (/ L LS) 2)))
    (setq R (* RMAX (/ LS L)))
    (setq X (* R (RFL:SPIRALFXR THETA)))
    (setq Y (* DIR R (RFL:SPIRALFYR THETA)))
   )
  )
  (setq X (+ X (* OS DIR (sin THETA))))
  (setq Y (- Y (* OS (cos THETA))))
  (setq PT2 (list (+ (car PLT) (* X (cos ANG)) (* -1.0 Y (sin ANG)))
                  (+ (cadr PLT) (* X (sin ANG)) (* Y (cos ANG)))
            )
  )
  (setq L (+ LO (* (/ (+ C 1.0) V) (- LS LO))))
  (if (= L 0.0)
   (progn
    (setq THETA 0.0)
    (setq R 0.0)
    (setq X 0.0)
    (setq Y 0.0)
   )
   (progn
    (setq THETA (* THETAMAX (expt (/ L LS) 2)))
    (setq R (* RMAX (/ LS L)))
    (setq X (* R (RFL:SPIRALFXR THETA)))
    (setq Y (* DIR R (RFL:SPIRALFYR THETA)))
   )
  )
  (setq X (+ X (* OS DIR (sin THETA))))
  (setq Y (- Y (* OS (cos THETA))))
  (setq PT3 (list (+ (car PLT) (* X (cos ANG)) (* -1.0 Y (sin ANG)))
                  (+ (cadr PLT) (* X (sin ANG)) (* Y (cos ANG)))
            )
  )
  (setq D (distance PT PT3))
  (setq H (distance PT2 (list (/ (+ (car PT) (car PT3)) 2.0) (/ (+ (cadr PT) (cadr PT3)) 2.0) 0.0)))
  (setq BULGE (* DIR 2.0 (/ H D)))
  (setq ENTLIST (append ENTLIST
                        (list (append (list 10) PT)
                        (cons 42 BULGE)
                        )
                )
  )
  (setq C (+ C 1.0))
 ) 
 (setq ENTLIST (append ENTLIST (list ENTLISTX)))
 (entmake ENTLIST)
)
;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:FITSPIRALAA Fits a reverse engineered DCA spiral between two arcs
;
;    Note:  Not working at this time
;
;
(defun RFL:FITSPIRALAA (ENT1 ENT2 / A ANG ANG1 ANG2 ANG3 B C D1 D2 D3 DIR ENTLIST1 ENTLIST2
                                    K P P1 P2 PC PC1 PC2 PP PLT PLT1 PLT2 PLTST PLTST1 PLTST2 PST PST1 PST2
                                    R THETA THETA1 THETA2)
 (setq ENTLIST1 (entget ENT1))
 (setq ENTLIST2 (entget ENT2))
 (setq P1 (cdr (assoc 10 ENTLIST1)))
 (setq P1 (list (car P1) (cadr P1)))
 (setq P2 (cdr (assoc 11 ENTLIST1)))
 (setq P2 (list (car P2) (cadr P2)))
 (setq PC (cdr (assoc 10 ENTLIST2)))
 (setq PC (list (car PC) (cadr PC)))
 (setq R (cdr (assoc 40 ENTLIST2)))
 (setq ANG1 (cdr (assoc 50 ENTLIST2)))
 (setq PC1 (list (+ (car PC) (* R (cos ANG1)))
                 (+ (cadr PC) (* R (sin ANG1)))))
 (setq ANG2 (cdr (assoc 51 ENTLIST2)))
 (setq PC2 (list (+ (car PC) (* R (cos ANG2)))
                 (+ (cadr PC) (* R (sin ANG2)))))
 (if (> (sin (- (angle P1 PC) (angle P1 P2))) 0.0)
  (setq DIR 1.0)
  (setq DIR -1.0)
 )
 (setq D1 (distance P1 PC))
 (setq D2 (distance P2 PC))
 (setq D3 (distance P1 P2))
 (setq A (/ (+ (- (expt D1 2) (expt D2 2)) (expt D3 2)) (* 2 D3)))
 (setq B (/ (+ (- (expt D2 2) (expt D1 2)) (expt D3 2)) (* 2 D3)))
 (setq C (sqrt (- (expt D1 2) (expt A 2))))
 (if (> A B)
  (setq PP (list (+ (car P1) (* (/ A D3) (- (car P2) (car P1))))
                 (+ (cadr P1) (* (/ A D3) (- (cadr P2) (cadr P1))))
           )
  )
  (setq PP (list (+ (car P2) (* (/ B D3) (- (car P1) (car P2))))
                 (+ (cadr P2) (* (/ B D3) (- (cadr P1) (cadr P2))))
           )
  )
 )
 (if (< C R)
  (eval nil)
  (progn
   (setq P (- C R))
   (setq THETA1 0.0)
   (setq THETA2 pi)
   (setq THETA (/ (+ THETA1 THETA2) 2.0))
   (while (AND (/= THETA THETA1) (/= THETA THETA2))
    (if (< (RFL:SPIRALPR THETA) (/ P R))
     (setq THETA1 THETA)
     (setq THETA2 THETA)
    )
    (setq THETA (/ (+ THETA1 THETA2) 2.0))
   )
   (setq K (* R (RFL:SPIRALKR THETA)))
   (setq PLT1 (list (+ (car PP)
                       (* (/ K (distance PP P1))
                          (- (car P1) (car PP))
                       )
                    )
                    (+ (cadr PP)
                       (* (/ K (distance PP P1))
                          (- (cadr P1) (cadr PP))
                       )
                    )
              )
   )
   (setq PLTST1 (list (+ (car PLT1)
                         (* R (- (RFL:SPIRALFXR THETA) (/ (RFL:SPIRALFYR THETA) (RFL:TAN THETA))) (cos (angle P1 P2)))
                      )
                      (+ (cadr PLT1)
                         (* R (- (RFL:SPIRALFXR THETA) (/ (RFL:SPIRALFYR THETA) (RFL:TAN THETA))) (sin (angle P1 P2)))
                      )
                )
   )
   (setq PST1 (list (+ (car PLT1)
                       (* R (RFL:SPIRALFXR THETA) (cos (angle P1 P2)))
                       (* -1.0 DIR R (RFL:SPIRALFYR THETA) (sin (angle P1 P2)))
                    )
                    (+ (cadr PLT1)
                       (* R (RFL:SPIRALFXR THETA) (sin (angle P1 P2)))
                       (* DIR R (RFL:SPIRALFYR THETA) (cos (angle P1 P2)))
                    )
              )
   )
   (setq PLT2 (list (+ (car PP)
                       (* (/ K (distance PP P2))
                          (- (car P2) (car PP))
                       )
                    )
                    (+ (cadr PP)
                       (* (/ K (distance PP P2))
                          (- (cadr P2) (cadr PP))
                       )
                    )
              )
   )
   (setq PLTST2 (list (+ (car PLT2)
                         (* R (- (RFL:SPIRALFXR THETA) (/ (RFL:SPIRALFYR THETA) (RFL:TAN THETA))) (cos (angle P2 P1)))
                      )
                      (+ (cadr PLT2)
                         (* R (- (RFL:SPIRALFXR THETA) (/ (RFL:SPIRALFYR THETA) (RFL:TAN THETA))) (sin (angle P2 P1)))
                      )
                )
   )
   (setq PST2 (list (+ (car PLT2)
                       (* R (RFL:SPIRALFXR THETA) (cos (angle P2 P1)))
                       (* DIR R (RFL:SPIRALFYR THETA) (sin (angle P2 P1)))
                    )
                    (+ (cadr PLT2)
                       (* R (RFL:SPIRALFXR THETA) (sin (angle P2 P1)))
                       (* -1.0 DIR R (RFL:SPIRALFYR THETA) (cos (angle P2 P1)))
                    )
              )
   )
   (if (= DIR 1.0)
    (progn
     (if (< (RFL:ANGLE3P PST1 PC PC1) (RFL:ANGLE3P PST1 PC PC2))
      (progn
       (RFL:DRAWSPIRAL PLT1 PLTST1 PST1 0.0 0.0)
       (setq ENTLIST1 (subst (list 11 (car PLT1) (cadr PLT1))
                             (assoc 11 ENTLIST1)
                             ENTLIST1))
       (entmod ENTLIST1)
       (entupd ENT1)
       (setq ENTLIST2 (subst (cons 50 (angle PC PST1))
                             (assoc 50 ENTLIST2)
                             ENTLIST2))
       (entmod ENTLIST2)
       (entupd ENT2)
      )
      (progn
       (RFL:DRAWSPIRAL PLT2 PLTST2 PST2 0.0 0.0)
       (setq ENTLIST1 (subst (list 10 (car PLT2) (cadr PLT2))
                             (assoc 10 ENTLIST1)
                             ENTLIST1))
       (entmod ENTLIST1)
       (entupd ENT1)
       (setq ENTLIST2 (subst (cons 51 (angle PC PST2))
                             (assoc 51 ENTLIST2)
                             ENTLIST2))
       (entmod ENTLIST2)
       (entupd ENT2)
      )
     )
    )
    (progn
     (if (> (RFL:ANGLE3P PST1 PC PC1) (RFL:ANGLE3P PST1 PC PC2))
      (progn
       (RFL:DRAWSPIRAL PLT1 PLTST1 PST1 0.0 0.0)
       (setq ENTLIST1 (subst (list 11 (car PLT1) (cadr PLT1))
                             (assoc 11 ENTLIST1)
                             ENTLIST1))
       (entmod ENTLIST1)
       (entupd ENT1)
       (setq ENTLIST2 (subst (cons 51 (angle PC PST1))
                             (assoc 51 ENTLIST2)
                             ENTLIST2))
       (entmod ENTLIST2)
       (entupd ENT2)
      )
      (progn
       (RFL:DRAWSPIRAL PLT2 PLTST2 PST2 0.0 0.0)
       (setq ENTLIST1 (subst (list 10 (car PLT2) (cadr PLT2))
                             (assoc 10 ENTLIST1)
                             ENTLIST1))
       (entmod ENTLIST1)
       (entupd ENT1)
       (setq ENTLIST2 (subst (cons 50 (angle PC PST2))
                             (assoc 50 ENTLIST2)
                             ENTLIST2))
       (entmod ENTLIST2)
       (entupd ENT2)
      )
     )
    )
   )
  )
 )
)
;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:FITSPIRALLA Fits a reverse engineered DCA spiral between a line and an arc
;
;
(defun FITSPIRALLA (ENT1 ENT2 / A ANG ANG1 ANG2 ANG3 B C D1 D2 D3 DIR ENTLIST1 ENTLIST2
                                K P P1 P2 PC PC1 PC2 PP PLT PLT1 PLT2 PLTST PLTST1 PLTST2 PST PST1 PST2
                                R THETA THETA1 THETA2)
 (setq ENTLIST1 (entget ENT1))
 (setq ENTLIST2 (entget ENT2))
 (setq P1 (cdr (assoc 10 ENTLIST1)))
 (setq P1 (list (car P1) (cadr P1)))
 (setq P2 (cdr (assoc 11 ENTLIST1)))
 (setq P2 (list (car P2) (cadr P2)))
 (setq PC (cdr (assoc 10 ENTLIST2)))
 (setq PC (list (car PC) (cadr PC)))
 (setq R (cdr (assoc 40 ENTLIST2)))
 (setq ANG1 (cdr (assoc 50 ENTLIST2)))
 (setq PC1 (list (+ (car PC) (* R (cos ANG1)))
                 (+ (cadr PC) (* R (sin ANG1)))))
 (setq ANG2 (cdr (assoc 51 ENTLIST2)))
 (setq PC2 (list (+ (car PC) (* R (cos ANG2)))
                 (+ (cadr PC) (* R (sin ANG2)))))
 (if (> (sin (- (angle P1 PC) (angle P1 P2))) 0.0)
  (setq DIR 1.0)
  (setq DIR -1.0)
 )
 (setq D1 (distance P1 PC))
 (setq D2 (distance P2 PC))
 (setq D3 (distance P1 P2))
 (setq A (/ (+ (- (expt D1 2) (expt D2 2)) (expt D3 2)) (* 2 D3)))
 (setq B (/ (+ (- (expt D2 2) (expt D1 2)) (expt D3 2)) (* 2 D3)))
 (setq C (sqrt (- (expt D1 2) (expt A 2))))
 (if (> A B)
  (setq PP (list (+ (car P1) (* (/ A D3) (- (car P2) (car P1))))
                 (+ (cadr P1) (* (/ A D3) (- (cadr P2) (cadr P1))))
           )
  )
  (setq PP (list (+ (car P2) (* (/ B D3) (- (car P1) (car P2))))
                 (+ (cadr P2) (* (/ B D3) (- (cadr P1) (cadr P2))))
           )
  )
 )
 (if (< (distance PP P1) RFL:TOLFINE)
  (setq P1 (list (+ (car P2) (* 0.9 (- (car P1) (car P2))))
                 (+ (cadr P2) (* 0.9 (- (cadr P1) (cadr P2))))))
 )
 (if (< (distance PP P2) RFL:TOLFINE)
  (setq P2 (list (+ (car P1) (* 0.9 (- (car P2) (car P1))))
                 (+ (cadr P1) (* 0.9 (- (cadr P2) (cadr P1))))))
 )
 (if (< C R)
  (eval nil)
  (progn
   (setq P (- C R))
   (setq THETA1 0.0)
   (setq THETA2 pi)
   (setq THETA (/ (+ THETA1 THETA2) 2.0))
   (while (> (abs (- THETA2 THETA1)) RFL:TOLFINE)
    (if (< (RFL:SPIRALPR THETA) (/ P R))
     (setq THETA1 THETA)
     (setq THETA2 THETA)
    )
    (setq THETA (/ (+ THETA1 THETA2) 2.0))
   )
   (setq K (* R (RFL:SPIRALKR THETA)))
   (setq PLT1 (list (+ (car PP)
                       (* (/ K (distance PP P1))
                          (- (car P1) (car PP))
                       )
                    )
                    (+ (cadr PP)
                       (* (/ K (distance PP P1))
                          (- (cadr P1) (cadr PP))
                       )
                    )
              )
   )
   (setq PLTST1 (list (+ (car PLT1)
                         (* R (- (RFL:SPIRALFXR THETA) (/ (RFL:SPIRALFYR THETA) (RFL:TAN THETA))) (cos (angle P1 P2)))
                      )
                      (+ (cadr PLT1)
                         (* R (- (RFL:SPIRALFXR THETA) (/ (RFL:SPIRALFYR THETA) (RFL:TAN THETA))) (sin (angle P1 P2)))
                      )
                )
   )
   (setq PST1 (list (+ (car PLT1)
                       (* R (RFL:SPIRALFXR THETA) (cos (angle P1 P2)))
                       (* -1.0 DIR R (RFL:SPIRALFYR THETA) (sin (angle P1 P2)))
                    )
                    (+ (cadr PLT1)
                       (* R (RFL:SPIRALFXR THETA) (sin (angle P1 P2)))
                       (* DIR R (RFL:SPIRALFYR THETA) (cos (angle P1 P2)))
                    )
              )
   )
   (setq PLT2 (list (+ (car PP)
                       (* (/ K (distance PP P2))
                          (- (car P2) (car PP))
                       )
                    )
                    (+ (cadr PP)
                       (* (/ K (distance PP P2))
                          (- (cadr P2) (cadr PP))
                       )
                    )
              )
   )
   (setq PLTST2 (list (+ (car PLT2)
                         (* R (- (RFL:SPIRALFXR THETA) (/ (RFL:SPIRALFYR THETA) (RFL:TAN THETA))) (cos (angle P2 P1)))
                      )
                      (+ (cadr PLT2)
                         (* R (- (RFL:SPIRALFXR THETA) (/ (RFL:SPIRALFYR THETA) (RFL:TAN THETA))) (sin (angle P2 P1)))
                      )
                )
   )
   (setq PST2 (list (+ (car PLT2)
                       (* R (RFL:SPIRALFXR THETA) (cos (angle P2 P1)))
                       (* DIR R (RFL:SPIRALFYR THETA) (sin (angle P2 P1)))
                    )
                    (+ (cadr PLT2)
                       (* R (RFL:SPIRALFXR THETA) (sin (angle P2 P1)))
                       (* -1.0 DIR R (RFL:SPIRALFYR THETA) (cos (angle P2 P1)))
                    )
              )
   )
   (if (= DIR 1.0)
    (progn
     (if (< (RFL:ANGLE3P PST1 PC PC1) (RFL:ANGLE3P PST1 PC PC2))
      (progn
       (RFL:DRAWSPIRAL PLT1 PLTST1 PST1 0.0 0.0)
       (setq ENTLIST1 (subst (list 11 (car PLT1) (cadr PLT1))
                             (assoc 11 ENTLIST1)
                             ENTLIST1))
       (entmod ENTLIST1)
       (entupd ENT1)
       (setq ENTLIST2 (subst (cons 50 (angle PC PST1))
                             (assoc 50 ENTLIST2)
                             ENTLIST2))
       (entmod ENTLIST2)
       (entupd ENT2)
      )
      (progn
       (RFL:DRAWSPIRAL PLT2 PLTST2 PST2 0.0 0.0)
       (setq ENTLIST1 (subst (list 10 (car PLT2) (cadr PLT2))
                             (assoc 10 ENTLIST1)
                             ENTLIST1))
       (entmod ENTLIST1)
       (entupd ENT1)
       (setq ENTLIST2 (subst (cons 51 (angle PC PST2))
                             (assoc 51 ENTLIST2)
                             ENTLIST2))
       (entmod ENTLIST2)
       (entupd ENT2)
      )
     )
    )
    (progn
     (if (> (RFL:ANGLE3P PST1 PC PC1) (RFL:ANGLE3P PST1 PC PC2))
      (progn
       (RFL:DRAWSPIRAL PLT1 PLTST1 PST1 0.0 0.0)
       (setq ENTLIST1 (subst (list 11 (car PLT1) (cadr PLT1))
                             (assoc 11 ENTLIST1)
                             ENTLIST1))
       (entmod ENTLIST1)
       (entupd ENT1)
       (setq ENTLIST2 (subst (cons 51 (angle PC PST1))
                             (assoc 51 ENTLIST2)
                             ENTLIST2))
       (entmod ENTLIST2)
       (entupd ENT2)
      )
      (progn
       (RFL:DRAWSPIRAL PLT2 PLTST2 PST2 0.0 0.0)
       (setq ENTLIST1 (subst (list 10 (car PLT2) (cadr PLT2))
                             (assoc 10 ENTLIST1)
                             ENTLIST1))
       (entmod ENTLIST1)
       (entupd ENT1)
       (setq ENTLIST2 (subst (cons 50 (angle PC PST2))
                             (assoc 50 ENTLIST2)
                             ENTLIST2))
       (entmod ENTLIST2)
       (entupd ENT2)
      )
     )
    )
   )
  )
 )
)
;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:FITSPIRALLL Fits a reverse engineered DCA spiral between two lines
;
;
(defun RFL:FITSPIRALLL (ENT1 ENT2 LS1 R LS2 / ANG ANG1 ANG2 DELTA DIR ENTLIST1 ENTLIST2
                                              P P1 P2 P3 P4 PLT1 PLT2 PLTST1 PLTST2 PST1 PST2
                                              R T1 T2 THETA1 THETA2 VAL1 VAL2)
 (setq ENTLIST1 (entget ENT1))
 (setq ENTLIST2 (entget ENT2))
 (setq P1 (cdr (assoc 10 ENTLIST1)))
 (setq P1 (list (car P1) (cadr P1)))
 (setq P2 (cdr (assoc 11 ENTLIST1)))
 (setq P2 (list (car P2) (cadr P2)))
 (setq P3 (cdr (assoc 10 ENTLIST2)))
 (setq P3 (list (car P3) (cadr P3)))
 (setq P4 (cdr (assoc 11 ENTLIST2)))
 (setq P4 (list (car P4) (cadr P4)))
 (setq P (inters P1 P2 P3 P4 nil))
 (setq ANG (RFL:ANGLE3P (if (> (distance P1 P) (distance P2 P)) P1 P2)
                        P
                        (if (> (distance P3 P) (distance P4 P)) P3 P4)
           )
 )
 (if (> (distance P1 P) (distance P2 P))
  (setq ANG1 (angle P P1))
  (setq ANG1 (angle P P2))
 )
 (if (> (distance P3 P) (distance P4 P))
  (setq ANG2 (angle P P3))
  (setq ANG2 (angle P P4))
 )
 (if (> (sin (- ANG2 (+ ANG1 pi))) 0.0)
  (setq DIR 1.0)
  (setq DIR -1.0)
 )
 (if (= R 0.0)
  (progn
   (setq DELTA 0.0)
   (setq THETA1 (/ (- pi ANG) (+ 1.0 (/ LS2 LS1))))
   (setq THETA2 (/ (- pi ANG) (+ 1.0 (/ LS1 LS2))))
   (setq R (/ LS1 (* 2.0 THETA1)))
  )
  (progn
   (setq THETA1 (/ LS1 (* 2.0 R)))
   (setq THETA2 (/ LS2 (* 2.0 R)))
   (setq DELTA (- (- pi ANG) (+ THETA1 THETA2)))
  )
 )
 (if (>= DELTA 0.0)
  (progn
   (setq VAL1 (* R (- (+ (* (RFL:SPIRALFYR THETA1) (sin ANG))
                            (cos (+ DELTA THETA2 (/ pi -2.0)))
                         )
                      (sin THETA2)
                   )
              )
   )
   (setq VAL2 (* R (- (+ (* (RFL:SPIRALFYR THETA2) (sin ANG))
                            (cos (+ DELTA THETA1 (/ pi -2.0)))
                         )
                      (sin THETA1)
                   )
              )
   )
   (setq T1 (/ (+ (* VAL1 (cos ANG))
                  VAL2
               )
               (expt (sin ANG) 2)
            )
   )
   (setq T2 (/ (+ (* VAL2 (cos ANG))
                  VAL1
               )
               (expt (sin ANG) 2)
            )
   )
   (setq PLT1 (list (+ (car P)
                       (* (+ T1
                             (* R (RFL:SPIRALFXR THETA1))
                          )
                          (cos ANG1)
                       )
                    )
                    (+ (cadr P)
                       (* (+ T1
                             (* R (RFL:SPIRALFXR THETA1))
                          )
                          (sin ANG1)
                       )
                    )
              )
   )
   (setq PLTST1 (list (+ (car P)
                         (* (+ T1
                               (if (> THETA1 0.0 )
                                (* R (/ (RFL:SPIRALFYR THETA1) (RFL:TAN THETA1)))
                                0.0
                               )
                            )
                            (cos ANG1)
                         )
                      )
                      (+ (cadr P)
                         (* (+ T1
                               (if (> THETA1 0.0 )
                                (* R (/ (RFL:SPIRALFYR THETA1) (RFL:TAN THETA1)))
                                0.0
                               )
                            )
                            (sin ANG1)
                         )
                      )
                )
   )
   (setq PST1 (list (+ (car P)
                       (* T1
                          (cos ANG1)
                       )
                       (* 1.0
                          DIR
                          (* R (RFL:SPIRALFYR THETA1))
                          (sin ANG1)
                       )
                    )
                    (+ (cadr P)
                       (* T1
                          (sin ANG1)
                       )
                       (* -1.0
                          DIR
                          (* R (RFL:SPIRALFYR THETA1))
                          (cos ANG1)
                       )
                    )
              )
   )
   (if (> THETA1 0.0)
    (RFL:DRAWSPIRAL PLT1 PLTST1 PST1 0.0 0.0)
   )
   (if (> (distance P1 P) (distance P2 P))
    (setq ENTLIST1 (subst (list 11 (car PLT1) (cadr PLT1)) (assoc 11 ENTLIST1) ENTLIST1))
    (setq ENTLIST1 (subst (list 10 (car PLT1) (cadr PLT1)) (assoc 10 ENTLIST1) ENTLIST1))
   )
   (entmod ENTLIST1)
   (entupd ENT1)
   (setq PLT2 (list (+ (car P)
                       (* (+ T2
                             (* R (RFL:SPIRALFXR THETA2))
                          )
                          (cos ANG2)
                       )
                    )
                    (+ (cadr P)
                       (* (+ T2
                             (* R (RFL:SPIRALFXR THETA2))
                          )
                          (sin ANG2)
                       )
                    )
              )
   )
   (setq PLTST2 (list (+ (car P)
                         (* (+ T2
                               (if (> THETA2 0.0)
                                (* R (/ (RFL:SPIRALFYR THETA2) (RFL:TAN THETA2)))
                                0.0
                               )
                            )
                            (cos ANG2)
                         )
                      )
                      (+ (cadr P)
                         (* (+ T2
                               (if (> THETA2 0.0)
                                (* R (/ (RFL:SPIRALFYR THETA2) (RFL:TAN THETA2)))
                                0.0
                               )
                            )
                            (sin ANG2)
                         )
                      )
                )
   )
   (setq PST2 (list (+ (car P)
                       (* T2
                          (cos ANG2)
                       )
                       (* -1.0
                          DIR
                          (* R (RFL:SPIRALFYR THETA2))
                          (sin ANG2)
                       )
                    )
                    (+ (cadr P)
                       (* T2
                          (sin ANG2)
                       )
                       (* 1.0
                          DIR
                          (* R (RFL:SPIRALFYR THETA2))
                          (cos ANG2)
                       )
                    )
              )
   )
   (if (> THETA2 0.0)
    (RFL:DRAWSPIRAL PLT2 PLTST2 PST2 0.0 0.0)
   )
   (if (> (distance P3 P) (distance P4 P))
    (setq ENTLIST2 (subst (list 11 (car PLT2) (cadr PLT2)) (assoc 11 ENTLIST2) ENTLIST2))
    (setq ENTLIST2 (subst (list 10 (car PLT2) (cadr PLT2)) (assoc 10 ENTLIST2) ENTLIST2))
   )
   (entmod ENTLIST2)
   (entupd ENT2)
   (if (> DELTA 0.0)
    (if (= DIR 1.0)
     (command "._ARC" PST1 "E" PST2 "R" R)
     (command "._ARC" PST2 "E" PST1 "R" R)
    )
   )
  )
 )
)
;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:GETSPIRALA returns the spiral 'A' for an entity
;
;
(defun RFL:GETSPIRALA (ENT / R LS)
 (setq R (RFL:GETSPIRALR ENT))
 (setq LS (RFL:GETSPIRALLS ENT))
 (if (= LS nil)
  nil
  (sqrt (* LS R))
 )
)
;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:GETSPIRALA2 returns the spiral 'A' for given long pi and short tangent points
;
;
(defun RFL:GETSPIRALA2 (PLT PLTST PST / R LS)
 (setq R (RFL:GETSPIRALR2 PLT PLTST PST))
 (setq LS (RFL:GETSPIRALLS2 PLT PLTST PST))
 (if (= LS nil)
  nil
  (sqrt (* LS R))
 )
)
;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:GETSPIRALDATA returns the extended spiral data for an entity (reverse engineered DCA spiral)
;
;
(defun RFL:GETSPIRALDATA (ENT / ENTLIST ENTLIST2 PLT PLTST PST LS SPIRALLIST TMP)
 (setq ENTLIST2 (cdr (assoc -3 (entget ENT '("*")))))
 (setq ENTLIST (cdr (assoc "DCA_FIGURE_XENT" ENTLIST2)))
 (if (or (= ENTLIST nil) (= (assoc 1011 ENTLIST) nil))
  (progn
   (setq SPIRALLIST nil)
  )
  (progn
   (while (/= (car (car ENTLIST)) 1011)
    (setq ENTLIST (cdr ENTLIST))
   )
   (setq PLT (cdr (car ENTLIST)))
   (setq PLT (list (car PLT) (cadr PLT)))
   (setq ENTLIST (cdr ENTLIST))
   (while (/= (car (car ENTLIST)) 1011)
    (setq ENTLIST (cdr ENTLIST))
   )
   (setq PLTST (cdr (car ENTLIST)))
   (setq PLTST (list (car PLTST) (cadr PLTST)))
   (setq ENTLIST (cdr ENTLIST))
   (while (/= (car (car ENTLIST)) 1011)
    (setq ENTLIST (cdr ENTLIST))
   )
   (setq PST (cdr (car ENTLIST)))
   (setq PST (list (car PST) (cadr PST)))
   (while (/= (car (car ENTLIST)) 1040)
    (setq ENTLIST (cdr ENTLIST))
   )
   (setq LS (cdr (car ENTLIST)))
   (setq ENTLIST (cdr ENTLIST))
   (if (< (distance PLT PLTST) (distance PST PLTST))
    (progn
     (setq TMP PST)
     (setq PST PLT)
     (setq PLT TMP)
    )
   )
   (setq SPIRALLIST (list PLT PLTST PST LS))
  )
 )
 SPIRALLIST
)
;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:GETSPIRALLS returns the spiral length for an entity
;
;
(defun RFL:GETSPIRALLS (ENT / THETA R)
 (setq THETA (RFL:GETSPIRALTHETA ENT))
 (setq R (RFL:GETSPIRALR ENT))
 (if (= THETA nil)
  nil
  (* 2.0 THETA R)
 )
)
;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:GETSPIRALLS2 returns the spiral length for given long pi and short tangent points
;
;
(defun RFL:GETSPIRALLS2 (PLT PLTST PST / THETA R)
 (setq THETA (RFL:GETSPIRALTHETA2 PLT PLTST PST))
 (setq R (RFL:GETSPIRALR2 PLT PLTST PST))
 (if (= THETA nil)
  nil
  (* 2.0 THETA R)
 )
)
;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:GETSPIRALPI2 returns the spiral PI for given length long pi and short tangent points
;
;
(defun RFL:GETSPIRALPI2 (L PLT PLTST PST / A P P1 P2 THETA)
 (if (< L RFL:TOLFINE)
  (setq P PLTST)
  (progn
   (setq P1 (RFL:SPIRALXY2 (list L 0.0) PLT PLTST PST))
   (setq A (RFL:GETSPIRALA2 PLT PLTST PST))
   (setq THETA (/ (* L L) (* A A) 2.0))
   (if (> (sin (- (angle PLTST PST) (angle PLT PLTST))) 0.0)
    (setq THETA (+ (angle PLT PLTST) THETA))
    (setq THETA (- (angle PLT PLTST) THETA))
   )
   (setq P2 (list (+ (car P1) (cos THETA)) (+ (cadr P1) (sin THETA))))
   (setq P (inters P1 P2 PLTST PST nil))
  )
 )
 P
)
;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:GETSPIRALR returns the spiral radius for an entity
;
;
(defun RFL:GETSPIRALR (ENT / PLTST PST R SPIRALLIST)
 (setq SPIRALLIST (RFL:GETSPIRALDATA ENT))
 (if (= SPIRALLIST nil)
  (progn
   (setq R nil)
  )
  (progn
   (setq PLTST (cadr SPIRALLIST))
   (setq PST (caddr SPIRALLIST))
   (setq THETA (RFL:GETSPIRALTHETA ENT))
   (setq R (/ (* (distance PLTST PST) (sin THETA)) (RFL:SPIRALFYR THETA)))
  )
 )
 R
)
;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:GETSPIRALR2 returns the spiral radius for given long pi and short tangent points
;
;
(defun RFL:GETSPIRALR2 (PLT PLTST PST / R)
 (setq THETA (RFL:GETSPIRALTHETA2 PLT PLTST PST))
 (setq R (/ (* (distance PLTST PST) (sin THETA)) (RFL:SPIRALFYR THETA)))
 R
);
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:GETSPIRALRADIUS returns the radius of the spiral data
;
;
(defun RFL:GETSPIRALRADIUS (L PLT PLTST PST / DIR LS R RMAX THETAMAX)
 (if (> (sin (- (angle PLTST PST) (angle PLT PLTST))) 0.0)
  (setq DIR 1.0)
  (setq DIR -1.0)
 )
 (setq LS (RFL:GETSPIRALLS2 PLT PLTST PST))
 (setq RMAX (RFL:GETSPIRALR2 PLT PLTST PST))
 (if (< L RFL:TOLFINE)
  (progn
   (setq PS PLT)
   (setq THETA 0.0)
  )
  (progn
   (if (< L RFL:TOLFINE)
    (progn
     (setq R 0.0)
    )
    (progn
     (setq R (* RMAX (/ LS L)))
    )
   )
  )
 )
 (* DIR R)
);
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:GETSPIRALTHETA returns the spiral deflection for an entity
;
;
(defun RFL:GETSPIRALTHETA (ENT / ENTLIST ENTLIST2 LS PLT PLTST PST SPIRALLIST THETA)
 (setq SPIRALLIST (RFL:GETSPIRALDATA ENT))
 (if (= SPIRALLIST nil)
  (progn
   (setq THETA nil)
  )
  (progn
   (setq PLT (car SPIRALLIST))
   (setq PLTST (cadr SPIRALLIST))
   (setq PST (caddr SPIRALLIST))
   (setq THETA (abs (- (angle PST PLTST) (angle PLTST PLT))))
   (if (< THETA 0.0)
    (progn
     (setq THETA (+ THETA (* 2.0 pi)))
    )
   )
   (if (> THETA pi)
    (progn
     (setq THETA (- (* 2.0 pi) THETA))
    )
   )
  )
 )
 THETA
)
;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:GETSPIRALTHETA2 returns the spiral deflection for given long pi and short tangent points
;
;
(defun RFL:GETSPIRALTHETA2 (PLT PLTST PST / ENTLIST ENTLIST2 LS THETA)
 (setq THETA (abs (- (angle PST PLTST) (angle PLTST PLT))))
 (if (< THETA 0.0)
  (progn
   (setq THETA (+ THETA (* 2.0 pi)))
  )
 )
 (if (> THETA pi)
  (progn
   (setq THETA (- (* 2.0 pi) THETA))
  )
 )
 THETA
)
;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:SPIRALFXR returns (R *  Spiral 'X') for a given deflection
;
;
(defun RFL:SPIRALFXR (THETA / AR2 DENOMINATOR N NUMERATOR SUM SUM2)
 (setq SUM -1.0)
 (setq SUM2 0.0)
 (setq AR2 (* 2.0 THETA))
 (setq N 1.0)
 (while (/= SUM SUM2)
  (setq SUM SUM2)
  (if (> THETA RFL:TOLFINE)
   (setq NUMERATOR (* (expt -1.0 (+ N 1.0)) (expt AR2 (* 2.0 (- N 1.0)))))
   (setq NUMERATOR 0.0)
  )
  (setq DENOMINATOR (* (expt 2.0 (* 2.0 (- N 1.0))) (- (* 4.0 N) 3.0) (RFL:FACT (* 2.0 (- N 1.0)))))
  (setq SUM2 (+ SUM2 (/ NUMERATOR DENOMINATOR)))
  (setq N (+ N 1))
 )
 (setq SUM (* SUM AR2))
 SUM
)
;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:SPIRALFYR returns (R *  Spiral 'Y') for a given deflection
;
;
(defun RFL:SPIRALFYR (THETA / AR2 DENOMINATOR N NUMERATOR SUM SUM2)
 (setq SUM -1.0)
 (setq SUM2 0.0)
 (setq AR2 (* 2.0 THETA))
 (setq N 1.0)
 (while (/= SUM SUM2)
  (setq SUM SUM2)
  (setq NUMERATOR (* (expt -1.0 (+ N 1.0)) (expt AR2 (- (* 2.0 N) 1.0))))
  (setq DENOMINATOR (* (expt 2.0 (- (* 2.0 N) 1.0)) (- (* 4.0 N) 1.0) (RFL:FACT (- (* 2.0 N) 1.0))))
  (setq SUM2 (+ SUM2 (/ NUMERATOR DENOMINATOR)))
  (setq N (+ N 1))
 )
 (setq SUM (* SUM AR2))
 SUM
)
;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:SPIRALK returns the spiral 'K' value for a given radius and length
;
;
(defun RFL:SPIRALK (R LS / THETA)
 (setq THETA (/ LS R 2.0))
 (* R (- (SPIRALFXR THETA) (sin THETA)))
)
;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:SPIRALKR returns the spiral 'K' value for a given deflection
;
;
(defun RFL:SPIRALKR (THETA)
 (- (RFL:SPIRALFXR THETA) (sin THETA))
)
;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:SPIRALOFFSET returns the offset of an offset spiral
;
;
(defun RFL:SPIRALOFFSET (ENT / ENT2 ENTLIST OS P P1 P2 PLT PLTST PST SDATA)
 (if (= (setq SDATA (RFL:GETSPIRALDATA ENT)) nil)
  nil
  (progn
   (setq ENTLIST (entget ENT))
   (if (= (cdr (assoc 0 ENTLIST)) "POLYLINE")
    (progn
     (setq ENT2 (entnext ENT))
     (setq ENTLIST (entget ENT2))
     (setq P1 (cdr (assoc 10 ENTLIST)))
     (while (/= "SEQEND" (cdr (assoc 0 ENTLIST)))
      (setq P2 (cdr (assoc 10 ENTLIST)))
      (setq ENT2 (entnext ENT2))
      (setq ENTLIST (entget ENT2))
     )
    )
    (progn
     (setq P1 (cdr (assoc 10 ENTLIST)))
     (setq P2 (cdr (assoc 10 (reverse ENTLIST))))
    )
   )
   (setq PLT (car SDATA))
   (setq PLTST (cadr SDATA))
   (setq PST (caddr SDATA))
   (if (< (distance PST P1) (distance PST P2))
    (setq P P1)
    (setq P P2)
   )
   (setq OS (distance PST P))

   (setq OS (* OS
               -1.0
               (RFL:SIGN (sin (- (angle PLTST PST) (angle PLT PLTST))))
               (RFL:SIGN (- (sin (- (angle PLTST P) (angle PLT PLTST)))
                            (sin (- (angle PLTST PST) (angle PLT PLTST)))
                         )
               )
            )
   )
  )
 )
)
;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:SPIRALOFFSET2 returns the offset of an offset spiral based on supplied spiral data
;
;
(defun SPIRALOFFSET2 (P1 P2 PLT PLTST PST LO / OS P)
 (if (< (distance PST P1) (distance PST P2))
  (setq P P1)
  (setq P P2)
 )
 (setq OS (distance PST P))
 (setq OS (* OS
             -1.0
             (RFL:SIGN (sin (- (angle PLTST PST) (angle PLT PLTST))))
             (RFL:SIGN (- (sin (- (angle PLTST P) (angle PLT PLTST)))
                          (sin (- (angle PLTST PST) (angle PLT PLTST)))
                       )
             )
          )
 )
)
;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:SPIRALP returns the spiral 'P' offset for a given length and radius
;
;
(defun RFL:SPIRALP (R LS / THETA)
 (setq THETA (/ LS R 2.0))
 (* R (- (RFL:SPIRALFYR THETA) (- 1.0 (cos THETA))))
)
;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:SPIRALPOINTON Checks is the entered point is within the spiral limits
;
;
(defun RFL:SPIRALPOINTON (P PLT PLTST PST LO / A2 ALPHA F1 F2 FCTN GETR PX PY
                                               RMAX SPIRALDIRECTION
                                               THETA1 THETA2 THETAMAX)
 (defun GETR (VAL)
  (if (< (abs VAL) RFL:TOLFINE)
   (eval 0.0)
   (sqrt (/ A2 VAL 2.0))
  )
 )
 (defun FCTN (VAL)
  (if (< (abs VAL) RFL:TOLFINE)
   PX
   (+ (* (- PX (* (GETR VAL) (RFL:SPIRALFXR VAL))) (cos VAL))
         (* SPIRALDIRECTION (- PY (* SPIRALDIRECTION (GETR VAL) (RFL:SPIRALFYR VAL))) (sin VAL))
   )
  )
 )
 (if (> (sin (- (angle PLTST PST) (angle PLT PLTST))) 0.0)
  (setq SPIRALDIRECTION 1.0)
  (setq SPIRALDIRECTION -1.0)
 )
 (setq ALPHA (angle PLT PLTST))
 (setq PX (+ (* (- (cadr P) (cadr PLT)) (sin ALPHA)) (* (- (car P) (car PLT)) (cos ALPHA))))
 (setq PY (- (* (- (cadr P) (cadr PLT)) (cos ALPHA)) (* (- (car P) (car PLT)) (sin ALPHA))))
 (setq THETAMAX (RFL:GETSPIRALTHETA2 PLT PLTST PST))
 (setq RMAX (RFL:GETSPIRALR2 PLT PLTST PST))
 (setq A2 (* 2.0 RMAX RMAX THETAMAX))
 (setq THETA1 (/ (* LO LO) A2 2.0))
 (setq THETA2 THETAMAX)
 (setq F1 (FCTN THETA1))
 (setq F2 (FCTN THETA2))
 (if (> (* F1 F2) RFL:TOLFINE)
  0
  1
 )
)
;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:SPIRALPR returns (R * spiral 'P') for a given deflection
;
;
(defun RFL:SPIRALPR (THETA)
 (- (RFL:SPIRALFYR THETA) (- 1.0 (cos THETA)))
)
;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:SPIRALSTAOFF returns the station and offset of a point for a given entity
;
;
(defun RFL:SPIRALSTAOFF (P ENT / LO PLT PLTST PST SPIRALLIST STAOFFVAL)
 (setq SPIRALLIST (RFL:GETSPIRALDATA ENT))
 (if (= SPIRALLIST nil)
  (setq STAOFFVAL nil)
  (setq PLT (car SPIRALLIST)
        PLTST (cadr SPIRALLIST)
        PST (caddr SPIRALLIST)
        LO (cadddr SPIRALLIST)
        STAOFFVAL (RFL:SPIRALSTAOFF2 P PLT PLTST PST LO)
  )
 )
 STAOFFVAL
)
;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:SPIRALSTAOFF2 returns the station and offset of a point for given data
;
;
(defun RFL:SPIRALSTAOFF2 (P PLT PLTST PST LO / A2 ALPHA F F1 F2 FCTN LS GETR OFFSET OFFSETDIRECTION P0 P1 PX PY
                                               R R1 R2 RMAX SPIRALDIRECTION SPIRALLIST STAOFFVAL STATION
                                               THETA THETA1 THETA2 THETAMAX THETAOLD TMP)
 (setq P (list (car P) (cadr P)))
 (defun GETR (VAL)
  (if (< (abs VAL) RFL:TOLFINE)
   (eval 0.0)
   (sqrt (/ A2 VAL 2.0))
  )
 )
 (defun FCTN (VAL)
  (if (< (abs VAL) RFL:TOLFINE)
   (progn
    (setq TMP PX)
   )
   (progn
    (setq TMP (+ (* (- PX (* (GETR VAL) (RFL:SPIRALFXR VAL))) (cos VAL))
                 (* SPIRALDIRECTION (- PY (* SPIRALDIRECTION (GETR VAL) (RFL:SPIRALFYR VAL))) (sin VAL))))
   )
  )
  (eval TMP)
 )
 (if (> (sin (- (angle PLTST PST) (angle PLT PLTST))) 0.0)
  (setq SPIRALDIRECTION 1.0)
  (setq SPIRALDIRECTION -1.0)
 )
 (setq ALPHA (angle PLT PLTST))
 (setq PX (+ (* (- (cadr P) (cadr PLT)) (sin ALPHA)) (* (- (car P) (car PLT)) (cos ALPHA))))
 (setq PY (- (* (- (cadr P) (cadr PLT)) (cos ALPHA)) (* (- (car P) (car PLT)) (sin ALPHA))))
 (setq THETAMAX (RFL:GETSPIRALTHETA2 PLT PLTST PST))
 (setq RMAX (RFL:GETSPIRALR2 PLT PLTST PST))
 (setq A2 (* 2.0 RMAX RMAX THETAMAX))
 (if (< (distance P PST) RFL:TOLFINE)
  (progn
   (setq THETA THETAMAX)
  )
  (progn
   (if (< (distance P PLT) RFL:TOLFINE)
    (progn
     (setq THETA 0.0)
    )
    (progn
     (setq THETA1 (/ (* LO LO) A2 2.0))
     (setq THETA2 THETAMAX)
     (setq THETA (/ (+ THETA1 THETA2) 2.0))
     (setq THETAOLD -1.0)
     (setq F1 (FCTN THETA1))
     (setq F2 (FCTN THETA2))
     (setq F (FCTN THETA))
     (while (> (abs (- THETA THETAOLD)) RFL:TOLFINE)
      (if (> (* F F2) 0.0)
       (setq THETA2 THETA)
       (setq THETA1 THETA)
      )
      (setq THETAOLD THETA)
      (setq THETA (/ (+ THETA1 THETA2) 2.0))
      (setq F1 (FCTN THETA1))
      (setq F2 (FCTN THETA2))
      (setq F (FCTN THETA))
     )
    )
   )
  )
 )
 (setq R (GETR THETA))
 (if (< (abs R) RFL:TOLFINE)
  (setq STATION 0.0)
  (setq STATION (/ A2 R))
 )
 (setq P0 (list (* R (RFL:SPIRALFXR THETA)) (* SPIRALDIRECTION R (RFL:SPIRALFYR THETA)) 0.0))
 (setq P1 (list PX PY 0.0))
 (if (> (sin (angle P0 P1)) 0.0)
  (setq OFFSETDIRECTION -1.0)
  (setq OFFSETDIRECTION 1.0)
 )
 (setq OFFSET (* OFFSETDIRECTION (distance P0 P1)))
 (setq STAOFFVAL (list STATION OFFSET))
 STAOFFVAL
)
;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:SPIRALXY returns the station and offset of the supplied point to the supplied entity
;
;
(defun RFL:SPIRALXY (P ENT / LO PLT PLTST PST SPIRALLIST STAOFFVAL PXY)
 (setq SPIRALLIST (RFL:GETSPIRALDATA ENT))
 (if (= SPIRALLIST nil)
  (setq PXY nil)
  (setq PLT (car SPIRALLIST)
        PLTST (cadr SPIRALLIST)
        PST (caddr SPIRALLIST)
        LO (cadddr SPIRALLIST)
        PXY (RFL:SPIRALXY2 P PLT PLTST PST)
  )
 )
 PXY
)
;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:SPIRALXY2 returns the station and offset of the supplied point to the supplied spiral data
;
;
(defun RFL:SPIRALXY2 (P PLT PLTST PST / ANG ANG2 DIR L LS OFFSET PS PXY R RMAX THETAMAX X Y)
 (setq ANG (angle PLT PLTST))
 (if (> (sin (- (angle PLTST PST) (angle PLT PLTST))) 0.0)
  (setq DIR 1.0)
  (setq DIR -1.0)
 )
 (setq L (car P))
 (setq OFFSET (cadr P))
 (setq LS (RFL:GETSPIRALLS2 PLT PLTST PST))
 (setq THETAMAX (RFL:GETSPIRALTHETA2 PLT PLTST PST))
 (setq RMAX (RFL:GETSPIRALR2 PLT PLTST PST))
 (if (< L RFL:TOLFINE)
  (progn
   (setq PS PLT)
   (setq THETA 0.0)
  )
  (progn
   (setq THETA (* THETAMAX (expt (/ L LS) 2)))
   (if (< L RFL:TOLFINE)
    (progn
     (setq R 0.0)
     (setq X 0.0)
     (setq Y 0.0)
    )
    (progn
     (setq R (* RMAX (/ LS L)))
     (setq X (* R (RFL:SPIRALFXR THETA)))
     (setq Y (* DIR R (RFL:SPIRALFYR THETA)))
    )
   )
   (setq PS (list (+ (car PLT) (* X (cos ANG)) (* -1.0 Y (sin ANG)))
                  (+ (cadr PLT) (* X (sin ANG)) (* Y (cos ANG)))
            )
   )
  )
 )
 (setq ANG2 (+ ANG (* DIR THETA) (/ pi -2.0)))
 (setq PXY (list (+ (car PS) (* OFFSET (cos ANG2)))
                 (+ (cadr PS) (* OFFSET (sin ANG2)))
           )
 )

 PXY
)
;
;
;   Program written by Robert Livingston, 98/06/11
;
;   C:DALIGN draws the current alignment
;
;
(defun C:DALIGN (/ AL ANGBASE ANGDIR CMDECHO OSMODE REP SFLAG STEP STEPSTA)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq OSMODE (getvar "OSMODE"))
 (setvar "OSMODE" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 0)

 (command "._UNDO" "M")
 (command "._UCS" "W")

 (if (/= nil ALIGNLIST)
  (progn
   (setq AL ALIGNLIST)
   (setq SFLAG 0)
   (while (/= AL nil)
    (if (listp (last (car AL)))
     (progn
      (setq SFLAG 1)
     )
    )
    (setq AL (cdr AL))
   )
   (if (= SFLAG 0)
    (RFL:DRAWALIGN)
    (RFL:DRAWALIGN2)
   )
  )
  (princ "\n*** ALIGNMENT NOT SET ***\n")
 )

 (command "._UCS" "P")
 (setvar "CMDECHO" CMDECHO)
 (setvar "OSMODE" OSMODE)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
);
;
;   Program written by Robert Livingston, 98/06/11
;
;   C:DALIGNOS draws the current alignment at a specified offset
;
;
(defun C:DALIGNOS (/ AL ANGBASE ANGDIR CMDECHO OS OSMODE REP SFLAG)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq OSMODE (getvar "OSMODE"))
 (setvar "OSMODE" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 0)

 (command "._UNDO" "M")
 (command "._UCS" "W")

 (if (/= nil ALIGNLIST)
  (progn
   (setq OS (getreal "\nEnter offset (-ve = left, +ve = right) : "))
   (setq AL ALIGNLIST)
   (setq SFLAG 0)
   (while (/= AL nil)
    (if (listp (last (car AL)))
     (progn
      (setq SFLAG 1)
     )
    )
    (setq AL (cdr AL))
   )
   (if (= SFLAG 0)
    (RFL:DRAWALIGNOS OS)
    (RFL:DRAWALIGNOS2 OS)
   )
  )
  (princ "\n*** ALIGNMENT NOT SET ***\n")
 )

 (command "._UCS" "P")
 (setvar "CMDECHO" CMDECHO)
 (setvar "OSMODE" OSMODE)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
)
;
;
;    Program Written by Robert Livingston, 99/07/14
;    C:DSPIRAL draws a reverse engineered DCA spiral at the end of a selected line
;
;
(defun C:DSPIRAL (/ ANG CMDECHO DIR ENT ENTLIST FX FY LR P P1 P2 PLT PLTST PST THETA)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (setq ENT (entsel "\nSelect line : "))
 (if (/= ENT nil) 
  (progn
   (setq P (car (cdr ENT)))
   (setq P (list (car P) (cadr P)))
   (setq ENT (car ENT))
   (if (= (cdr (assoc 0 (setq ENTLIST (entget ENT)))) "LINE")
    (progn
     (setq P1 (cdr (assoc 10 ENTLIST)))
     (setq P1 (list (car P1) (cadr P1)))
     (setq P2 (cdr (assoc 11 ENTLIST)))
     (setq P2 (list (car P2) (cadr P2)))
     (if (< (distance P P1) (distance P P2))
      (progn
       (setq TMP P1)
       (setq P1 P2)
       (setq P2 TMP)
      )
     )
     (setq ANG (angle P1 P2))
     (if (/= (setq R (getreal "Radius : ")) nil)
      (if (/= (setq L (getreal "Length : ")) nil)
       (progn
        (initget 1 "Left Right")
        (setq LR (getkword "\n Left or Right : "))
        (if (= LR "Left")
         (setq DIR 1.0)
         (setq DIR -1.0)
        )
        (setq THETA (/ L (* 2.0 R)))
        (setq FX (* R (RFL:SPIRALFXR THETA)))
        (setq FY (* R (RFL:SPIRALFYR THETA)))
        (setq PLT P2)
        (setq PST (list (+ (car PLT) (* FX (cos ANG)) (* DIR -1.0 FY (sin ANG)))
                        (+ (cadr PLT) (* FX (sin ANG)) (* DIR FY (cos ANG)))))
        (setq PLTST (list (+ (car PLT) (* (- FX (/ FY (RFL:TAN THETA))) (cos ANG)))
                          (+ (cadr PLT) (* (- FX (/ FY (RFL:TAN THETA))) (sin ANG))))) 
        (RFL:DRAWSPIRAL PLT PLTST PST 0.0 0.0)
       )
      )
     )
    )
   )
  )
 )
 (setvar "CMDECHO" CMDECHO)
)
;
;
;    Program Written by Robert Livingston, 99/07/14
;    C:FITSPIRAL draws a reverse engineered DCA spiral between two selected objects (lines and arcs)
;
;
(defun C:FITSPIRAL (/ CMDECHO ENT1 ENT2 ENTLIST1 ENTLIST2 GETLS LS1 LS2 R)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (defun GETLS (R MSG / LS AL)
  (setq LS nil)
  (setq AL "L")
  (if (= R 0.0)
   (progn
    (princ "\n*** Zero length arc selected - only spiral length valid!")
    (setq LS (getreal (strcat MSG " length :")))
   )
   (progn
    (while (= LS nil)
     (if (= AL "L")
      (progn
       (setq LS (getreal (strcat MSG " length <return for A>:")))
       (if (= LS nil)
        (progn
         (setq AL "A")
        )
       )
      )
      (progn
       (setq LS (getreal (strcat MSG " A <return for length>:")))
       (if (= LS nil)
        (progn
         (setq AL "L")
        )
        (progn
         (setq LS (/ (* LS LS) R))
        )
       )
      )
     )
    )
   )
  )
  (eval LS)
 )
 
 (if (/= (setq ENT1 (car (entsel "\nSelect first entity : "))) nil)
  (if (/= (setq ENT2 (car (entsel "\nSelect second entity : "))) nil)
   (progn
    (setq ENTLIST1 (entget ENT1))
    (setq ENTLIST2 (entget ENT2))
    (if (and (= (cdr (assoc 0 ENTLIST1)) "LINE") (= (cdr (assoc 0 ENTLIST2)) "LINE"))
     (progn
      (if (/= (setq R (getreal "\nEnter radius (0 for Spiral/Spiral) : ")) nil)
       (if (/= (setq LS1 (GETLS R "\nSpiral IN")) nil)
        (if (/= (setq LS2 (GETLS R "\nSpiral OUT")) nil)
         (RFL:FITSPIRALLL ENT1 ENT2 LS1 R LS2)
        )
       )
      )
     )
     (if (and (= (cdr (assoc 0 ENTLIST1)) "LINE") (= (cdr (assoc 0 ENTLIST2)) "ARC"))
      (progn
       (RFL:FITSPIRALLA ENT1 ENT2)
      )
      (if (and (= (cdr (assoc 0 ENTLIST1)) "ARC") (= (cdr (assoc 0 ENTLIST2)) "LINE"))
       (progn
        (RFL:FITSPIRALLA ENT2 ENT1)
       )
       (if (and (= (cdr (assoc 0 ENTLIST1)) "ARC") (= (cdr (assoc 0 ENTLIST2)) "ARC"))
        (progn
;         (RFL:FITSPIRALAA ENT1 ENT2)
        )
       )
      )
     )
    )
   )
  )
 )

 (setvar "CMDECHO" CMDECHO)
)
;
;
;   Program written by Robert Livingston, 98/06/11
;
;   GALIGN extracts a horizontal alignment from the current drawing
;
;
(defun C:GALIGN (/ ALIGNENT ALIGNENTLIST ANGBASE ANGDIR CMDECHO PSTART STASTART)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 0)

 (command "._UNDO" "M")
 (command "._UCS" "W")

 (if (/= RFL:ALIGNDEF nil)
  (progn
   (setq ALIGNLIST nil)
   (setq PSTART (getpoint "\nStart point:"))
   (if (/= PSTART nil)
    (progn
     (setq STASTART (getreal "\nStart chainage:"))
     (if (/= STASTART nil)
      (progn
       (princ "\nSelect R14 polyline (<return> to select SoftDesk entities):")
       (setq ALIGNENT (car (entsel)))
       (if (= ALIGNENT nil)
        (progn
         (setq ALIGNENT (ssget))
         (setq ALIGNLIST (RFL:ALIGNDEF (list ALIGNENT) PSTART STASTART))
        )
        (progn
         (setq ALIGNENTLIST (entget ALIGNENT))
         (if (= (cdr (assoc 0 ALIGNENTLIST)) "POLYLINE")
          (progn
           (command "._CONVERT" "P" "S" ALIGNENT "")
           (setq ALIGNENTLIST (entget ALIGNENT))
          )
         )
         (if (= (cdr (assoc 0 ALIGNENTLIST)) "LWPOLYLINE")
          (progn
           (setq ALIGNLIST (RFL:ALIGNDEF ALIGNENT PSTART STASTART))
          )
          (princ "\n**** NOT A POLYLINE ****")
         )
        )
       )
      )
     )
    )
   )
  )
  (progn
   (princ "\n!!!!! ALIGNMENT UTILITIES NOT LOADED !!!!!\n")
  )
 )

 (command "._UCS" "P")
 (setvar "CMDECHO" CMDECHO)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
);
;
;   Program written by Robert Livingston, 98/06/11
;
;   RALIGN reads a horizontal alignment from file
;
;
(defun C:RALIGN (/ ANGBASE ANGDIR CMDECHO INFILENAME)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 0)

 (if (= (vl-registry-read "HKEY_CURRENT_USER\\rflAlignDirectory") nil)
  (vl-registry-write "HKEY_CURRENT_USER\\rflAlignDirectory" "" "")
 )
 (setq INFILENAME (getfiled "Select a Horizontal Alignment File" (vl-registry-read "HKEY_CURRENT_USER\\rflAlignDirectory") "hor" 2))
 (RFL:RALIGN INFILENAME)
 (setvar "CMDECHO" CMDECHO)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
)
;
;
;   Program written by Robert Livingston, 98/06/11
;
;   WALIGN writes a horizontal alignment to file
;
;
(defun C:WALIGN (/ CMDECHO OUTFILENAME)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (if (= (vl-registry-read "HKEY_CURRENT_USER\\rflAlignDirectory") nil)
  (vl-registry-write "HKEY_CURRENT_USER\\rflAlignDirectory" "" "")
 )
 (if (= ALIGNLIST nil)
  (princ "\n*** NO ALIGNMENT EXISTS - USE RALIGN OR GALIGN ***\n")
  (progn
   (setq OUTFILENAME (getfiled "Select a Horizontal Alignment File" (vl-registry-read "HKEY_CURRENT_USER\\rflAlignDirectory") "hor" 1))
   (RFL:WALIGN OUTFILENAME)
  )
 )
 (setvar "CMDECHO" CMDECHO)
)
