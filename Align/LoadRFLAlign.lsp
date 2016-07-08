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
;   RFL:DRAWALIGN2 draws the current alignmnet
;
;
;
(defun RFL:DRAWALIGN2 (/ ANG1 ANG2 ALLIST ALENT ENT ENTLIST PC PREVENT R)
 (setq ALLIST ALIGNLIST)
 (entmake)
 (while (/= ALLIST nil)
  (setq ALENT (car ALLIST))
  (setq ALLIST (cdr ALLIST))
  (if (listp (last ALENT))
   (progn
    (RFL:DRAWSPIRAL (nth 0 (last ALENT)) (nth 1 (last ALENT)) (nth 2 (last ALENT)) (nth 3 (last ALENT)) 0.0)
    (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
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
      (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
     )
     (progn
      (setq ENTLIST (list (cons 0 "LINE")
                          (list 10 (nth 0 (nth 1 ALENT)) (nth 1 (nth 1 ALENT)) 0.0)
                          (list 11 (nth 0 (nth 2 ALENT)) (nth 1 (nth 2 ALENT)) 0.0)
                    )
      )
      (entmake ENTLIST)
      (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
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
;   Program written by Robert Livingston, 98/06/11
;
;   RFL:RALIGN reads a horizontal alignment from the specified file
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
;   Program written by Robert Livingston, 98/06/12
;
;   RFL:STAOFF returns a list of (STA OFFSET) for a provided (X Y)
;
;
(defun RFL:STAOFF (P / ANG ANG1 ANG2 AL C D D1 D11 D2 D22 OFFSET
                       P1 P2 PLT PLTST PST LO
                       OFFSETBEST PC R STA STABEST TMP)
 (setq STABEST nil)
 (setq OFFSETBEST nil)
 (if (/= ALIGNLIST nil)
  (progn
   (setq C 0)
   (setq AL (nth C ALIGNLIST))
   (while (/= AL nil)
    (if (> (distance (cadr AL) (caddr AL)) RFL:TOLFINE)
     (progn
      (if (listp (cadddr AL))
       (progn
        (setq P1 (cadr AL))
        (setq P2 (caddr AL))
        (setq PLT (car (cadddr AL)))
        (setq PLTST (cadr (cadddr AL)))
        (setq PST (caddr (cadddr AL)))
        (setq LO (cadddr (cadddr AL)))
        (if (= (RFL:SPIRALPOINTON P PLT PLTST PST LO) 1)
         (progn
          (setq TMP (RFL:SPIRALSTAOFF2 P PLT PLTST PST LO))
          (if (< (distance P2 PST) (distance P1 PST))
           (progn
            (setq STA (- (+ (car AL) (car TMP)) LO))
            (setq OFFSET (cadr TMP))
           )
           (progn
            (setq STA (- (+ (car AL) (RFL:GETSPIRALLS2 PLT PLTST PST)) (car TMP)))
            (setq OFFSET (* -1.0 (cadr TMP)))
           )
          )
          (if (= STABEST nil)
           (progn
            (setq STABEST STA)
            (setq OFFSETBEST OFFSET)
           )
           (progn
            (if (< (abs OFFSET) (abs OFFSETBEST))
             (progn
              (setq STABEST STA)
              (setq OFFSETBEST OFFSET)
             )
            )
           )
          )
         )
        )
       )
       (progn
        (if (< (abs (cadddr AL)) RFL:TOLFINE)
         (progn
          (setq D (distance (cadr AL) (caddr AL)))
          (setq D1 (distance (cadr AL) P))
          (setq D2 (distance (caddr AL) P))
          (setq D11 (/ (+ (* D D)
                          (- (* D1 D1)
                             (* D2 D2)
                          )
                       )
                       (* 2.0 D)
                    )
          )
          (setq D22 (- D D11))
          (if (and (<= D11 (+ D RFL:TOLFINE)) (<= D22 (+ D RFL:TOLFINE)))
           (progn
            (setq STA (+ (car AL) D11))
            (setq OFFSET (sqrt (abs (- (* D1 D1) (* D11 D11)))))
            (setq ANG (- (angle (cadr AL) (caddr AL)) (angle (cadr AL) P)))
            (while (< ANG 0.0) (setq ANG (+ ANG (* 2.0 pi))))
            (if (> ANG (/ pi 2.0)) (setq OFFSET (* OFFSET -1.0)))
            (if (= STABEST nil)
             (progn
              (setq STABEST STA)
              (setq OFFSETBEST OFFSET)
             )
             (progn
              (if (< (abs OFFSET) (abs OFFSETBEST))
               (progn
                (setq STABEST STA)
                (setq OFFSETBEST OFFSET)
               )
              )
             )
            )
           )
          )
         )
         (progn
          (setq PC (RFL:CENTER (cadr AL) (caddr AL) (cadddr AL)))
          (if (< (cadddr AL) 0.0)
           (setq ANG1 (- (angle PC (cadr AL)) (angle PC P)))
           (setq ANG1 (- (angle PC P) (angle PC (cadr AL))))
          )
          (while (< ANG1 0.0) (setq ANG1 (+ ANG1 (* 2.0 pi))))
          (if (< (cadddr AL) 0.0)
           (setq ANG2 (- (angle PC (cadr AL)) (angle PC (caddr AL))))
           (setq ANG2 (- (angle PC (caddr AL)) (angle PC (cadr AL))))
          )
          (while (< ANG2 0.0) (setq ANG2 (+ ANG2 (* 2.0 pi))))
          (if (<= ANG1 (+ ANG2 RFL:TOLFINE))
           (progn
            (setq R (RFL:RADIUS (cadr AL) (caddr AL) (cadddr AL)))
            (setq STA (+ (car AL) (* R ANG1)))
            (setq OFFSET (- (distance PC P) R))
            (if (< (cadddr AL) 0.0) (setq OFFSET (* -1.0 OFFSET)))
            (if (= STABEST nil)
             (progn
              (setq STABEST STA)
              (setq OFFSETBEST OFFSET)
             )
             (progn
              (if (< (abs OFFSET) (abs OFFSETBEST))
               (progn
                (setq STABEST STA)
                (setq OFFSETBEST OFFSET)
               )
              )
             )
            )
           )
          )
         )
        )
       )
      )
     )
    )
    (setq C (+ C 1))
    (setq AL (nth C ALIGNLIST))
   )
  )
 )
 (if (= STABEST nil)
  (eval nil)
  (list STABEST OFFSETBEST)
 )
)
;
;
;   Program written by Robert Livingston, 98/06/12
;
;   RFL:XY returns a list of (X Y) for a provided (STA OFFSET)
;
;
(defun RFL:XY (P / ANG AL ALTMP C D DIST OFFSET P1 P2 PC POINT STA X Y TOL)
 (setq TOL 0.00000001)
 (defun POINT (P1 P2 BULGE L / A ATOTAL C CHORD LTOTAL P PC R SB X Y)
  (setq CHORD (distance P1 P2))
  (if (< (abs BULGE) TOL)
   (progn
    (list (+ (* (/ L CHORD) (- (car P2) (car P1))) (car P1))
          (+ (* (/ L CHORD) (- (cadr P2) (cadr P1))) (cadr P1)))
   )
   (progn
    (setq ATOTAL (* 4.0 (atan (abs BULGE))))
    (setq PC (RFL:CENTER P1 P2 BULGE))
    (setq R (RFL:RADIUS P1 P2 BULGE))
    (setq A (+ (angle PC P1) (* (RFL:SIGN BULGE) (/ L R))))
    (list (+ (car PC) (* R (cos A)))
          (+ (cadr PC) (* R (sin A))))
   )
  )
 )
 (defun DIST (P1 P2 BULGE / ATOTAL CHORD R)
  (if (listp BULGE)
   (progn
    (- (RFL:GETSPIRALLS2 (car BULGE) (cadr BULGE) (caddr BULGE)) (cadddr BULGE))
   )
   (progn
    (setq ATOTAL (* 4.0 (atan (abs BULGE))))
    (setq CHORD (distance P1 P2))
    (if (= 0.0 BULGE)
     (eval CHORD)
     (progn 
      (setq R (/ CHORD (* 2 (sin (/ ATOTAL 2)))))
      (* R ATOTAL)
     )
    )
   )
  )
 )
 (if (/= nil ALIGNLIST)
  (progn
   (setq STA (car P))
   (setq OFFSET (cadr P))
   (setq AL (last ALIGNLIST))
   (if (<= STA (+ (car AL) (DIST (cadr AL) (caddr AL) (cadddr AL))))
    (progn
     (setq AL (car ALIGNLIST))
     (setq ALTMP (cdr ALIGNLIST))
     (if (>= STA (car AL))
      (progn
       (while (> STA (+ (car AL) (DIST (cadr AL) (caddr AL) (cadddr AL))))
        (setq AL (car ALTMP))
        (setq ALTMP (cdr ALTMP))
       )
       (if (listp (cadddr AL))
        (progn
         (if (< (distance (caddr AL) (caddr (cadddr AL))) (distance (cadr AL) (caddr (cadddr AL))))
          (progn
           (setq P1 (RFL:SPIRALXY2 (list (+ (- STA
                                           (car AL)
                                        )
                                        (cadddr (cadddr AL))
                                     )
                                     OFFSET
                               )
                               (car (cadddr AL))
                               (cadr (cadddr AL))
                               (caddr (cadddr AL))
                    )
           )
          )
          (progn
           (setq P1 (RFL:SPIRALXY2 (list (- (RFL:GETSPIRALLS2 (car (cadddr AL))
                                                              (cadr (cadddr AL))
                                                              (caddr (cadddr AL))
                                            )
                                            (- STA
                                               (car AL)
                                            )
                                         )
                                         (* -1.0 OFFSET)
                                   )
                                   (car (cadddr AL))
                                   (cadr (cadddr AL))
                                   (caddr (cadddr AL))
                    )
           )
          )
         )
        )
        (progn
         (setq P2 (POINT (cadr AL) (caddr AL) (cadddr AL) (- STA (car AL))))
         (if (< (abs (cadddr AL)) TOL)
          (progn
           (setq ANG (angle (cadr AL) (caddr AL)))
           (setq D (distance (cadr AL) P2))
           (setq P1 (list (+ (+ (car (cadr AL)) (* D (cos ANG))) (* OFFSET (sin ANG)))
                          (- (+ (cadr (cadr AL)) (* D (sin ANG))) (* OFFSET (cos ANG)))
                    )
           )
          )
          (progn
           (setq PC (RFL:CENTER (cadr AL) (caddr AL) (cadddr AL)))
           (if (< (cadddr AL) 0.0)
            (setq ANG (angle P2 PC))
            (setq ANG (angle PC P2))
           )
           (setq P1 (list (+ (car P2) (* OFFSET (cos ANG)))
                          (+ (cadr P2) (* OFFSET (sin ANG)))
                    )
           )
          )
         )
        )
       )
      )
      (progn
       (princ "\n**** STATION OUT OF RANGE ****")
       nil
      )
     )
    )
    (progn
     (princ "\n**** STATION OUT OF RANGE ****")
     nil
    )
   )
  )
  (progn
   (princ "\n**** NO ALIGNMENT DEFINED ****")
   nil
  )
 )
)
;
;
;     Program written by Robert Livingston, 2016/07/05
;
;     RFL:LALIGNSTALBL is a utility for placing STALBL blocks along and alignment
;
;
(defun RFL:LALIGNSTALBL (STASTART STAEND INC OS R / HANDLE HANDLEPREV)
 (if ALIGNLIST
  (progn
  )
  (princ "\n*** No alignment defined! ***\n")
 )
 nil
)
;
;
;     Program written by Robert Livingston, 2016/07/07
;
;     RFL:GETALIGNLENGTH returns the length the alignment defined by ALIGNLIST
;
;
(defun RFL:GETALIGNLENGTH (/ DIST)
 (defun DIST (P1 P2 BULGE / ATOTAL CHORD R)
  (if (listp BULGE)
   (progn
    (- (RFL:GETSPIRALLS2 (car BULGE) (cadr BULGE) (caddr BULGE)) (cadddr BULGE))
   )
   (progn
    (setq ATOTAL (* 4.0 (atan (abs BULGE))))
    (setq CHORD (distance P1 P2))
    (if (= 0.0 BULGE)
     (eval CHORD)
     (progn 
      (setq R (/ CHORD (* 2 (sin (/ ATOTAL 2)))))
      (* R ATOTAL)
     )
    )
   )
  )
 )
 (if (= ALIGNLIST nil)
  (progn
   nil
  )
  (progn
   (- (+ (car (last ALIGNLIST))
         (DIST (cadr (last ALIGNLIST)) (caddr (last ALIGNLIST)) (cadddr (last ALIGNLIST)))
      )
      (car (car ALIGNLIST))
   )
  )
 )
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
;    RFL:SPIRALPR returns (R * spiral 'P') for a given deflection
;
;
(defun RFL:SPIRALPR (THETA)
 (- (RFL:SPIRALFYR THETA) (- 1.0 (cos THETA)))
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
;    RFL:FITSPIRALLA Fits a reverse engineered DCA spiral between a line and an arc
;
;
(defun RFL:FITSPIRALLA (ENT1 ENT2 / A ANG ANG1 ANG2 ANG3 B C D1 D2 D3 DIR ENTLIST1 ENTLIST2
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
;   Program written by Robert Livingston, 98/05/14
;
;   RFL:PROFDEF locates and defines a global variable (PROFDEF) with the profile base point, stationing and elevations
;
;
(defun RFL:PROFDEF (/ BPOINT DIRECTION ELEV ENT ENTLIST FNAME PLAYER PTLAYER SCALE STA STAH STAL TMP VEXAG)
 (setq PROFDEF nil
       BPOINT nil
       DIRECTION nil
       ELEV nil
       FNAME ""
       PLAYER (getvar "CLAYER")
       PTLAYER (getvar "CLAYER")
       SCALE 1.0
       STA nil
       VEXAG 1.0
 )
 (setq ENT (car (entsel "\nSelect profile grid or profile definition block : ")))
 (setq ENTLIST (entget ENT))
 (setq BPOINT (cdr (assoc 10 ENTLIST)))
 (if (and (= "INSERT" (cdr (assoc 0 ENTLIST)))
          (= 1 (cdr (assoc 66 ENTLIST)))
     )
  (progn
   (setq ENT (entnext ENT))
   (setq ENTLIST (entget ENT))
   (while (= "ATTRIB" (cdr (assoc 0 ENTLIST)))
    (cond ((= "DIRECTION" (strcase (cdr (assoc 2 ENTLIST))))
           (setq DIRECTION (atoi (cdr (assoc 1 ENTLIST))))
          )
          ((= "ELEV" (strcase (cdr (assoc 2 ENTLIST))))
           (setq ELEV (atof (cdr (assoc 1 ENTLIST))))
          )
          ((= "FNAME" (strcase (cdr (assoc 2 ENTLIST))))
           (setq FNAME (cdr (assoc 1 ENTLIST)))
          )
          ((= "PLAYER" (strcase (cdr (assoc 2 ENTLIST))))
           (setq PLAYER (cdr (assoc 1 ENTLIST)))
          )
          ((= "PTLAYER" (strcase (cdr (assoc 2 ENTLIST))))
           (setq PTLAYER (cdr (assoc 1 ENTLIST)))
          )
          ((= "SCALE" (strcase (cdr (assoc 2 ENTLIST))))
           (setq SCALE (atof (cdr (assoc 1 ENTLIST))))
          )
          ((= "STAH" (strcase (cdr (assoc 2 ENTLIST))))
           (setq STAH (cdr (assoc 1 ENTLIST)))
          )
          ((= "STAL" (strcase (cdr (assoc 2 ENTLIST))))
           (setq STAL (cdr (assoc 1 ENTLIST)))
          )
          ((= "VEXAG" (strcase (cdr (assoc 2 ENTLIST))))
           (setq VEXAG (atof (cdr (assoc 1 ENTLIST))))
          )
    )
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
   )
   (if (and STAH STAL) (setq STA (atof (strcat STAH STAL))))
  )
  (if (/= nil (setq ENTLIST (cdadr (assoc -3 (entget ENT (list "RFLTOOLS_XENT"))))))
   (if (= (cdar ENTLIST) "RFLTOOLS_DRAWGRID")
    (progn
     (setq ENTLIST (cdr ENTLIST)
           ENTLIST (cdr ENTLIST)
           ENTLIST (cdr ENTLIST)
           ENTLIST (cdr ENTLIST)
           STA (cdar ENTLIST)
           ENTLIST (cdr ENTLIST)
           ELEV (cdar ENTLIST)
           ENTLIST (cdr ENTLIST)
           ENTLIST (cdr ENTLIST)
           ENTLIST (cdr ENTLIST)
           VEXAG (cdar ENTLIST)
           ENTLIST (cdr ENTLIST)
           ENTLIST (cdr ENTLIST)
           ENTLIST (cdr ENTLIST)
           ENTLIST (cdr ENTLIST)
           ENTLIST (cdr ENTLIST)
           ENTLIST (cdr ENTLIST)
           ENTLIST (cdr ENTLIST)
           ENTLIST (cdr ENTLIST)
           ENTLIST (cdr ENTLIST)
           ENTLIST (cdr ENTLIST)
           ENTLIST (cdr ENTLIST)
           ENTLIST (cdr ENTLIST)
           ENTLIST (cdr ENTLIST)
           SCALE (cdar ENTLIST)
           ENTLIST (cdr ENTLIST)
           DIRECTION (cdar ENTLIST)
     )
    )
   )
  )
 )
 (if (and BPOINT DIRECTION ELEV FNAME PLAYER PTLAYER SCALE STA VEXAG)
  (setq PROFDEF (list (cons "BPOINT" BPOINT)
                      (cons "DIRECTION" DIRECTION)
                      (cons "ELEV" ELEV)
                      (cons "FNAME" FNAME)
                      (cons "PLAYER" PLAYER)
                      (cons "PTLAYER" PTLAYER)
                      (cons "SCALE" SCALE)
                      (cons "STA" STA)
                      (cons "VEXAG" VEXAG)
                )
  )
  nil
 )
);
;
;   Program written by Robert Livingston, 98/05/14
;
;   RFL:ELEVATION returns the elevation at a specified station for the curretnly defined profile (PVILIST)
;
;
(defun RFL:ELEVATION (STA / C CMDECHO ELEV ELEV1 ELEV2 ELEV3 G1 G2 L NODE P STA1 STA2 STA3)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (if (/= nil PVILIST)
  (progn
   (if (or (< STA (caar PVILIST)) (> STA (car (last PVILIST))))
    (progn
     (princ "\n*** STATION OUT OF RANGE ***\n")
     (setq ELEV nil)
    )
    (progn
     (setq C 1)
     (while (> STA (+ (car (setq NODE (nth C PVILIST)))
                      (/ (if (= nil (cadddr NODE)) 0.0 (cadddr NODE)) 2.0)
                   )
            )
      (setq C (1+ C))
     )
     (if (or (= "L" (caddr (nth C PVILIST))) (= nil (caddr (nth C PVILIST))))
      (progn
       (setq NODE (nth (1- C) PVILIST))
       (setq STA1 (car NODE))
       (setq ELEV1 (cadr NODE))
       (setq NODE (nth C PVILIST))
       (setq STA2 (car NODE))
       (setq ELEV2 (cadr NODE))
       (setq L (if (= nil (cadddr NODE)) 0.0 (cadddr NODE)))
       (setq G1 (/ (- ELEV2 ELEV1) (- STA2 STA1)))
       (setq ELEV (+ ELEV1 (* G1 (- STA STA1))))
       (setq D (- STA (- STA2 (/ L 2.0))))
       (if (> D 0.0)
        (progn
         (setq NODE (nth (1+ C) PVILIST))
         (setq STA3 (car NODE))
         (setq ELEV3 (cadr NODE))
         (setq G2 (/ (- ELEV3 ELEV2) (- STA3 STA2)))
         (setq ELEV (+ ELEV (/ (* D D (- G2 G1)) (* L 2.0))))
        )
       )        
      )
      (progn
       (princ "\n*** ONLY PARABILIC VERTICAL CURVES SUPPORTED ***\n")
       (setq ELEV nil)
      )
     )
    )
   )
  )
  (progn
   (princ "\n*** PROFILE NOT SET - RUN GPROF OR RPROF ***\n")
   (setq ELEV nil)
  )
 )

 (setvar "CMDECHO" CMDECHO)
 (eval ELEV)
)
;
;
;   Program written by Robert Livingston, 98/05/14
;
;   RFL:SLOPE returns the slope at a specified station for the curretnly defined profile (PVILIST)
;
;
(defun RFL:SLOPE (STA / C CMDECHO ELEV1 ELEV2 ELEV3 G G1 G2 L NODE P)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (if (/= nil PVILIST)
  (progn
   (if (or (< STA (caar PVILIST)) (> STA (car (last PVILIST))))
    (progn
     (princ "\n*** STATION OUT OF RANGE ***\n")
     (if (< STA (caar PVILIST))
      (setq G (/ (- (cadadr PVILIST) (cadar PVILIST)) (- (caadr PVILIST) (caar PVILIST))))
      (setq G (/ (- (cadadr (reverse PVILIST)) (cadar (reverse PVILIST))) (- (caadr (reverse PVILIST)) (caar (reverse PVILIST)))))
     )
    )
    (progn
     (setq C 0)
     (while (> STA (+ (car (setq NODE (nth C PVILIST)))
                      (/ (cadddr NODE) 2.0)
                   )
            )
      (setq C (1+ C))
     )
     (if (= "L" (caddr (nth C PVILIST)))
      (progn
       (setq NODE (nth (1- C) PVILIST))
       (setq STA1 (car NODE))
       (setq ELEV1 (cadr NODE))
       (setq NODE (nth C PVILIST))
       (setq STA2 (car NODE))
       (setq ELEV2 (cadr NODE))
       (setq L (cadddr NODE))
       (setq G1 (/ (- ELEV2 ELEV1) (- STA2 STA1)))
       (setq G G1)
       (setq D (- STA (- STA2 (/ L 2.0))))
       (if (> D 0.0)
        (progn
         (setq NODE (nth (1+ C) PVILIST))
         (setq STA3 (car NODE))
         (setq ELEV3 (cadr NODE))
         (setq G2 (/ (- ELEV3 ELEV2) (- STA3 STA2)))
         (setq G (+ G1 (* (/ D L) (- G2 G1))))
        )
       )        
      )
      (progn
       (princ "\n*** ONLY PARABILIC VERTICAL CURVES SUPPORTED ***\n")
       (setq ELEV nil)
      )
     )
    )
   )
  )
  (progn
   (princ "\n*** PROFILE NOT SET - RUN GPROF OR RPROF ***\n")
   (setq G nil)
  )
 )

 (setvar "CMDECHO" CMDECHO)
 G
)
;
;
;   Program written by Robert Livingston, 98/05/14
;
;   RFL:PROFPOINT returns the point at a specified station and elevation for the curretnly defined profile grid (PROFDEF)
;
;
(defun RFL:PROFPOINT (STA ELEV / D X Y)
 (if (/= nil PROFDEF)
  (progn
   (if (= (assoc "DIRECTION" PROFDEF) nil)
    (setq D 1)
    (setq D (cdr (assoc "DIRECTION" PROFDEF)))
   )
   (setq X (+ (* (- STA
                    (cdr (assoc "STA" PROFDEF))
                 )
                 D
              )
              (car (cdr (assoc "BPOINT" PROFDEF)))
           )
   )
   (setq Y (+ (* (- ELEV
                    (cdr (assoc "ELEV" PROFDEF))
                 )
                 (cdr (assoc "VEXAG" PROFDEF))
              )
              (cadr (cdr (assoc "BPOINT" PROFDEF)))
           )
   )
   (list X Y 0.0)
  )
  (progn
   (princ "\n*** PROFILE NOT SET - RUN GPROF OR RPROF ***\n")
   nil
  )
 )
)
;
;
;   Program written by Robert Livingston, 99/11/15
;
;   RFL:PROFHIGHLOW draws circles at the high and low points along a profile
;
;
(defun RFL:PROFHIGHLOW (R / CLAYER ENT OSMODE G1 G2 L P1 P2 P3 PREVENT PVI STA STA1 STA2)
 (setq OSMODE (getvar "OSMODE"))
 (setvar "OSMODE" 0)
 (setq CLAYER (getvar "CLAYER"))
 (setq PREVENT nil)
 (if (not (tblsearch "LAYER" (cdr (assoc "PTLAYER" PROFDEF))))
  (entmake (list (cons 0 "LAYER")
                 (cons 100 "AcDbSymbolTableRecord")
                 (cons 100 "AcDbLayerTableRecord")
                 (cons 2 (cdr (assoc "PTLAYER" PROFDEF)))
                 (cons 70 0)
           )
  )
 )
 (setvar "CLAYER" (cdr (assoc "PTLAYER" PROFDEF)))

 (setq PVI PVILIST)
 (setq P1 (car PVI))
 (setq PVI (cdr PVI))
 (setq P2 (car PVI))
 (setq PVI (cdr PVI))
 (setq P3 (car PVI))
 (setq PVI (cdr PVI))
 (while (/= nil P3)
  (setq G1 (/ (- (nth 1 P2) (nth 1 P1)) (- (nth 0 P2) (nth 0 P1))))
  (setq G2 (/ (- (nth 1 P3) (nth 1 P2)) (- (nth 0 P3) (nth 0 P2))))
  (setq L (nth 3 P2))
  (setq STA1 (- (nth 0 P2) (/ L 2.0)))
  (setq STA2 (+ (nth 0 P2) (/ L 2.0)))
  (if (< (* G1 G2) 0.0)
   (progn
    (setq STA (+ STA1 (/ (* L G1) (- G1 G2))))
    (entmake (list (cons 0 "CIRCLE")
                   (cons 10 (RFL:PROFPOINT STA (RFL:ELEVATION STA)))
                   (cons 40 R)
             )
    )
    (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
   )
  )
  (setq P1 P2)
  (setq P2 P3)
  (setq P3 (car PVI))
  (setq PVI (cdr PVI))
 )
 (setvar "OSMODE" OSMODE)
 (setvar "CLAYER" CLAYER)
);
;
;   Program written by Robert Livingston, 99/11/15
;
;   RFL:DRAWPROF draws the current profile as defined in PVILIST
;
;
(defun RFL:DRAWPROF ( PVILIST / ACTIVEDOC ACTIVESPACE ANG BULGE C C2 CLAYER ENT ENTLIST G G1 G2 K
                                L L1 L2 L3 L4 P1 P2 P3 PLINETYPE PREVENT SIGN STA STA1 STA2 TMP)
 (setq CLAYER (getvar "CLAYER"))
 
 (setq PREVENT nil)

 (vl-load-com)
 (setq ACTIVEDOC (vla-get-activedocument (vlax-get-acad-object)))
 (setq ACTIVESPC
       (vlax-get-property ACTIVEDOC
        (if (or (eq acmodelspace (vla-get-activespace ACTIVEDOC)) (eq :vlax-true (vla-get-mspace ACTIVEDOC)))
         'modelspace
         'paperspace
        )
       )
 )

 (if (= nil PROFDEF)
  (princ "\n*** Profile not set ***")
  (progn
   (if (= (tblsearch "BLOCK" "PVI2") nil)
    (progn
     (princ "\n*** Creating LDD PVI node ***")
     (RFL:MAKEENT "PVI2")
    )
   )
     (setq C 0)
     (if (not (tblsearch "LAYER" (cdr (assoc "PTLAYER" PROFDEF))))
      (entmake (list (cons 0 "LAYER")
                     (cons 100 "AcDbSymbolTableRecord")
                     (cons 100 "AcDbLayerTableRecord")
                     (cons 2 (cdr (assoc "PTLAYER" PROFDEF)))
                     (cons 70 0)
               )
      )
     )
     (setvar "CLAYER" (cdr (assoc "PTLAYER" PROFDEF)))
     (if (= nil (tblsearch "BLOCK" "PVI2")) (RFL:MAKEENT "PVI2"))
     (while (< C (length PVILIST))
      (vla-insertblock ACTIVESPC
                       (vlax-3D-point (RFL:PROFPOINT (nth 0 (nth C PVILIST)) (nth 1 (nth C PVILIST))))
                       "PVI2"
                       25.4
                       25.4
                       25.4
                       0.0
      )
      (setq ENT (entlast))
      (RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
      (setq ENTLIST (entget ENT))
      (if (= (cdr (assoc 66 ENTLIST)) 1)
       (progn
        (if (or (= C 0) (= C (- (length PVILIST) 1)))
         (progn
          (setq L 0.0 K 0.0)
         )
         (progn
          (setq L (nth 3 (nth C PVILIST)))
          (setq G1 (/ (- (nth 1 (nth C PVILIST))
                         (nth 1 (nth (- C 1) PVILIST))
                      )
                      (- (nth 0 (nth C PVILIST))
                         (nth 0 (nth (- C 1) PVILIST))
                      )
                   )
          )
          (setq G2 (/ (- (nth 1 (nth (+ C 1) PVILIST))
                         (nth 1 (nth C PVILIST))
                      )
                      (- (nth 0 (nth (+ C 1) PVILIST))
                         (nth 0 (nth C PVILIST))
                      )
                   )
          )
          (if (= G1 G2)
           (setq K 0.0)
           (setq K (abs (/ L (- G2 G1) 100.0)))
          )
         )
        )
        (while (/= (cdr (assoc 0 ENTLIST)) "SEQEND")
         (if (= (cdr (assoc 2 ENTLIST)) "LENGTH")
          (progn
           (setq ENTLIST (subst (cons 1 (rtos L 2 8)) (assoc 1 ENTLIST) ENTLIST))
           (entmod ENTLIST)
           (entupd ENT)
          )
         )
         (if (= (cdr (assoc 2 ENTLIST)) "K")
          (progn
           (setq ENTLIST (subst (cons 1 (rtos K 2 8)) (assoc 1 ENTLIST) ENTLIST))
           (entmod ENTLIST)
           (entupd ENT)
          )
         )
         (setq ENT (entnext ENT))
         (setq ENTLIST (entget ENT))
        )
       )
      )
      (setq C (+ C 1))
     )
   (setq C 1)
   (setq G1 (/ (- (nth 1 (nth C PVILIST))
                  (nth 1 (nth (- C 1) PVILIST))
               )
               (- (nth 0 (nth C PVILIST))
                  (nth 0 (nth (- C 1) PVILIST))
               )
            )
   )
   (if (= (+ C 1) (length PVILIST))
    (setq G2 0)
    (setq G2 (/ (- (nth 1 (nth (+ C 1) PVILIST))
                   (nth 1 (nth C PVILIST))
                )
                (- (nth 0 (nth (+ C 1) PVILIST))
                   (nth 0 (nth C PVILIST))
                )
             )
    )
   )
   (setq G (- G2 G1))
   (setq ANG (- (atan G2) (atan G1)))
   (if (> G 0) (setq SIGN 1.0) (setq SIGN -1.0))
   (if (= (nth 2 (nth C PVILIST)) "L")
    (progn
     (setq L3 (/ (nth 3 (nth C PVILIST)) 2.0))
     (setq L4 (/ (nth 3 (nth C PVILIST)) 2.0))
    )
    (progn
     (setq TMP (* (nth 3 (nth C PVILIST))
                (RFL:TAN (/ (abs ANG) 2.0))
             )
     )
     (setq L3 (* TMP (cos (atan (abs G1)))))
     (setq L4 (* TMP (cos (atan (abs G2)))))
    )
   )
   (setq STA1 (nth 0 (nth (- C 1) PVILIST)))
   (setq STA2 (- (nth 0 (nth C PVILIST)) L3))
   (command "._LINE"
            (RFL:PROFPOINT STA1 (RFL:ELEVATION STA1))
            (RFL:PROFPOINT STA2 (RFL:ELEVATION STA2))
            ""
   )
   (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
   (while (< C (- (length PVILIST) 1))
    (setq C (+ C 1))
    (setq G1 G2)
    (if (= (+ C 1) (length PVILIST))
     (setq G2 0)
     (setq G2 (/ (- (nth 1 (nth (+ C 1) PVILIST))
                    (nth 1 (nth C PVILIST))
                 )
                 (- (nth 0 (nth (+ C 1) PVILIST))
                    (nth 0 (nth C PVILIST))
                 )
              )
     )
    )
    (setq G (- G2 G1))
    (setq ANG (- (atan G2) (atan G1)))
    (if (> G 0) (setq SIGN 1.0) (setq SIGN -1.0))
    (if (= (nth 2 (nth C PVILIST)) "L")
     (progn
      (setq L1 L3)
      (setq L2 L4)
      (setq L3 (/ (nth 3 (nth C PVILIST)) 2.0))
      (setq L4 (/ (nth 3 (nth C PVILIST)) 2.0))
     )
     (progn
      (setq L1 L3)
      (setq L2 L4)
      (setq TMP (* (nth 3 (nth C PVILIST))
                 (RFL:TAN (/ (abs ANG) 2.0))
              )
      )
      (setq L3 (* TMP (cos (atan (abs G1)))))
      (setq L4 (* TMP (cos (atan (abs G2)))))
     )
    )
    (if (> (+ L1 L2) 0.0)
     (progn
      (entmake)
      (setq STA1 (- (nth 0 (nth (- C 1) PVILIST)) L1))
      (setq STA2 (+ (nth 0 (nth (- C 1) PVILIST)) L2))
      (RFL:DRAWPARABOLICVCURVE (RFL:PROFPOINT STA1 (RFL:ELEVATION STA1))
                               (RFL:PROFPOINT (nth 0 (nth (- C 1) PVILIST)) (nth 1 (nth (- C 1) PVILIST)))
                               (RFL:PROFPOINT STA2 (RFL:ELEVATION STA2))
      )
      (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
     )
    )
    (setq STA1 (+ (nth 0 (nth (- C 1) PVILIST)) L2))
    (setq STA2 (- (nth 0 (nth C PVILIST)) L3))
    (command "._LINE"
             (RFL:PROFPOINT STA1 (RFL:ELEVATION STA1))
             (RFL:PROFPOINT STA2 (RFL:ELEVATION STA2))
             ""
    )
    (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
   )
  )
 )
 (setvar "CLAYER" CLAYER)
);
;
;   Program written by Robert Livingston, 98/05/13
;
;   RPROF reads a vertical alignment from file INFILENAME and sets the global variable PVILIST
;
;
(defun RFL:RPROF (INFILENAME / INFILE INLINE PVIENT PVISET STA ELEV LR VAL)
 (if (/= INFILENAME nil) (setq INFILENAME (findfile INFILENAME)))
 (if (/= INFILENAME nil)
  (progn
   (vl-registry-write "HKEY_CURRENT_USER\\rflAlignDirectory" "" (strcat (vl-filename-directory INFILENAME) "\\"))
   (setq INFILE (open INFILENAME "r"))
   (setq PVILIST nil)
   (setq INLINE (read-line INFILE))
   (if (/= INLINE "#RFL VERTICAL ALIGNMENT FILE")
    (progn
     (princ "\n*** FILE NOT FORMATTED CORRECTLY ***\n")
    )
    (progn
     (setq INLINE (read-line INFILE))
     (while (and (/= nil INLINE) (/= INLINE "#END DEFINITION"))
      (setq STA (atof INLINE))
      (setq INLINE (read-line INFILE))
      (setq ELEV (atof INLINE))
      (setq INLINE (read-line INFILE))
      (setq LR INLINE)
      (setq INLINE (read-line INFILE))
      (setq VAL (atof INLINE))
      (setq INLINE (read-line INFILE))
      (setq PVILIST (append PVILIST (list (list STA ELEV LR VAL))))
     )
    )
   )
   (close INFILE)
  )
 )
)
;
;
;   Program written by Robert Livingston, 98/05/13
;
;   RFL:RPROFOG reads a vertical alignment from file INFILENAME and sets the global variable OGLIST
;
;
(defun RFL:RPROFOG (INFILENAME / INFILE INLINE PVIENT PVISET STA ELEV LR VAL)
 (if (/= INFILENAME nil) (setq INFILENAME (findfile INFILENAME)))
 (if (/= INFILENAME nil)
  (progn
   (setq INFILE (open INFILENAME "r"))
   (setq OGLIST nil)
   (setq INLINE (read-line INFILE))
   (if (/= INLINE "#RFL VERTICAL ALIGNMENT FILE")
    (progn
     (princ "\n*** FILE NOT FORMATTED CORRECTLY ***\n")
    )
    (progn
     (setq INLINE (read-line INFILE))
     (while (and (/= nil INLINE) (/= INLINE "#END DEFINITION"))
      (setq STA (atof INLINE))
      (setq INLINE (read-line INFILE))
      (setq ELEV (atof INLINE))
      (setq INLINE (read-line INFILE))
      (setq LR INLINE)
      (setq INLINE (read-line INFILE))
      (setq VAL (atof INLINE))
      (setq INLINE (read-line INFILE))
      (setq OGLIST (append OGLIST (list (list STA ELEV))))
     )
    )
   )
   (close INFILE)
  )
 )
);
;
;   Program written by Robert Livingston, 98/05/13
;
;   RFL:WPROF writes a vertical alignment to file
;
;
(defun RFL:WPROF (OUTFILENAME / C OUTFILE)
 (if (/= OUTFILENAME nil)
  (progn
   (if (/= ".VRT" (strcase (substr OUTFILENAME (- (strlen OUTFILENAME) 3))))
    (setq OUTFILENAME (strcat OUTFILENAME ".VRT"))
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
     (princ "#RFL VERTICAL ALIGNMENT FILE\n" OUTFILE)
     (setq C 0)
     (while (< C (length PVILIST))
      (princ (rtos (nth 0 (nth C PVILIST)) 2 16) OUTFILE)
      (princ "\n" OUTFILE)
      (princ (rtos (nth 1 (nth C PVILIST)) 2 16) OUTFILE)
      (princ "\n" OUTFILE)
      (princ (nth 2 (nth C PVILIST)) OUTFILE)
      (princ "\n" OUTFILE)
      (princ (rtos (nth 3 (nth C PVILIST)) 2 16) OUTFILE)
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
;   Program written by Robert Livingston, 98/05/13
;
;   RFL:WPROFOG writes a vertical alignment to file
;
;
(defun RFL:WPROFOG (OUTFILENAME / C OUTFILE)
 (if (/= OUTFILENAME nil)
  (progn
   (if (/= ".VRT" (strcase (substr OUTFILENAME (- (strlen OUTFILENAME) 3))))
    (setq OUTFILENAME (strcat OUTFILENAME ".VRT"))
   )
   (setq C 0)
   (while (and (= nil (setq OUTFILE (open OUTFILENAME "w"))) (< C 5))
    (setq C (+ C 1))
    (princ (strcat "\nProblem openning file for writing : " (itoa C)))
   )
   (if (= nil OUTFILE)
    (alert (strcat "Error openning file for writing : " OUTFILENAME))
    (progn
     (princ "#RFL VERTICAL ALIGNMENT FILE\n" OUTFILE)
     (setq C 0)
     (while (< C (length OGLIST))
      (princ (rtos (nth 0 (nth C OGLIST)) 2 16) OUTFILE)
      (princ "\n" OUTFILE)
      (princ (rtos (nth 1 (nth C OGLIST)) 2 16) OUTFILE)
      (princ "\n" OUTFILE)
      (princ "L\n" OUTFILE)
      (princ "0.0\n" OUTFILE)
      (setq C (+ C 1))
     )
     (princ "#END DEFINITION\n" OUTFILE)
     (close OUTFILE)
    )
   )
  )
 )
);
;
;     Program written by Robert Livingston, 2016/07/06
;
;     RFL:DRAWPARABOLICVCURVE draws a parabolic vertical curve through three input points.
;         Note that P2 must be precisely between P1 and P3 for this to be a valid alignment curve
;
;
(defun RFL:DRAWPARABOLICVCURVE (P1 P2 P3 / ENT ENTOB SPLINESEGS SPLINETYPE)
 (setq SPLINESEGS (getvar "SPLINESEGS"))
 (setq SPLINETYPE (getvar "SPLINETYPE"))
 
 (setq P1 (list (car P1) (cadr P1) 0.0)
       P2 (list (car P2) (cadr P2) 0.0)
       P3 (list (car P3) (cadr P3) 0.0)
 )

 (entmake (list (cons 0 "POLYLINE")
                (list 10 0.0 0.0 0.0)
                (cons 66 1)
          )
 )
 (entmake (list (cons 0 "VERTEX")
                (cons 10 P1)
          )
 )
 (entmake (list (cons 0 "VERTEX")
                (cons 10 P2)
          )
 )
 (entmake (list (cons 0 "VERTEX")
                (cons 10 P3)
          )
 )
 (setq ENT (entmake (list (cons 0 "SEQEND")
                    )
           )
 )
 (if ENT
  (progn
   (setvar "SPLINESEGS" 65)
   (setvar "SPLINETYPE" 5)
   (setq ENTOB (vlax-ename->vla-object (entlast)))
   (vlax-put-property ENTOB "Type" 2)
  )
 )
 
 (setvar "SPLINESEGS" SPLINESEGS)
 (setvar "SPLINETYPE" SPLINETYPE)
);
;
;     Program written by Robert Livingston, 2016/07/06
;
;     RFL:xxxENT is a collection of routines for adding extended data for linking entities
;
;     (RFL:PUTENT E1 E2 E3)   :  Adds handle of E2 as the next entity, E3 as the previous entity to E1
;     (RFL:PUTNEXTENT E1 E2)  :  Adds handle of E2 as the next entity to E1
;     (RFL:PUTPREVENT E1 E2)  :  Adds handle of E2 as the previous entity to E1
;     (RFL:GETNEXTENT E1)     :  Returns the next entity of E1
;     (RFL:GETPREVENT E1)     :  Returns the previous entity of E1
;     (RFL:GETALLNEXTENT E1)  :  Returns all the next entities of E1
;     (RFL:GETALLPREVENT E1)  :  Returns all the previous entities of E1
;     (RFL:GETALLENT E1)      :  Returns all the entities linked to E1 (including E1)
;     (RFL:BREAKENT E1)       :  Removes all the links to E1 and relinks the previous and next to eachother
;
(defun RFL:PUTENT (ENT NEXTENT PREVENT / ENTLIST)
 (vl-load-com)
 (if (not (tblsearch "APPID" "RFLTOOLS_XENT"))
  (regapp "RFLTOOLS_XENT")
 )
 (setq ENTLIST nil)
 (if (= (type ENT) 'ENAME)
  (progn
   (cond ((and (= (type NEXTENT) 'ENAME)
               (= (type PREVENT) 'ENAME)
          )
          (setq ENTLIST (append (entget ENT)
                                (list
                                      (list -3 
                                            (list "RFLTOOLS_XENT"
                                                  (cons 1000 "RFLTOOLS_NEXTENT")
                                                  (cons 1005 (cdr (assoc 5 (entget NEXTENT))))
                                                  (cons 1000 "RFLTOOLS_PREVENT")
                                                  (cons 1005 (cdr (assoc 5 (entget PREVENT))))
                                            )
                                      )
                                )
                        )
          )
         )
         ((and (= (type NEXTENT) 'ENAME)
               (= PREVENT nil)
          )
          (setq ENTLIST (append (entget ENT)
                                (list
                                      (list -3 
                                            (list "RFLTOOLS_XENT"
                                                  (cons 1000 "RFLTOOLS_NEXTENT")
                                                  (cons 1005 (cdr (assoc 5 (entget NEXTENT))))
                                            )
                                      )
                                )
                        )
          )
         )
         ((and (= NEXTENT nil)
               (= (type PREVENT) 'ENAME)
          )
          (setq ENTLIST (append (entget ENT)
                                (list
                                      (list -3 
                                            (list "RFLTOOLS_XENT"
                                                  (cons 1000 "RFLTOOLS_PREVENT")
                                                  (cons 1005 (cdr (assoc 5 (entget PREVENT))))
                                            )
                                      )
                                )
                        )
          )
         )
         ((and (= NEXTENT nil)
               (= PREVENT nil)
          )
          (setq ENTLIST (list (cons -1 ENT) (list -3 (list "RFLTOOLS_XENT"))))
         )
   )
  )
 )
 (if ENTLIST
  (progn
   (entmod ENTLIST)
   ENT
  )
  nil
 )
)
(defun RFL:PUTNEXTENT (ENT NEXTENT / ENTLIST PREVENT)
 (if (= (type ENT) 'ENAME)
  (progn
   (setq PREVENT (RFL:GETPREVENT ENT))
   (RFL:PUTENT ENT NEXTENT PREVENT)
   ENT
  )
  nil
 )
)
(defun RFL:PUTPREVENT (ENT PREVENT / ENTLIST NEXTENT)
 (if (= (type ENT) 'ENAME)
  (progn
   (setq NEXTENT (RFL:GETNEXTENT ENT))
   (RFL:PUTENT ENT NEXTENT PREVENT)
   ENT
  )
  nil
 )
)
(defun RFL:GETNEXTENT (ENT / ENTLIST)
 (if (= (type ENT) 'ENAME)
  (progn
   (if (/= nil (setq ENTLIST (cdadr (assoc -3 (entget ENT (list "RFLTOOLS_XENT"))))))
    (while (and ENTLIST (/= (cdar ENTLIST) "RFLTOOLS_NEXTENT"))
     (setq ENTLIST (cdr ENTLIST))
    )
   )
   (setq ENTLIST (cdr ENTLIST))
   (if ENTLIST
    (handent (cdar ENTLIST))
    nil
   )
  )
  nil
 )
)
(defun RFL:GETPREVENT (ENT / ENTLIST)
 (if (= (type ENT) 'ENAME)
  (progn
   (if (/= nil (setq ENTLIST (cdadr (assoc -3 (entget ENT (list "RFLTOOLS_XENT"))))))
    (while (and ENTLIST (/= (cdar ENTLIST) "RFLTOOLS_PREVENT"))
     (setq ENTLIST (cdr ENTLIST))
    )
   )
   (setq ENTLIST (cdr ENTLIST))
   (if ENTLIST
    (handent (cdar ENTLIST))
    nil
   )
  )
  nil
 )
)
(defun RFL:GETALLNEXTENT (ENT / ENT2 ENTSET)
 (if (= (type ENT) 'ENAME)
  (progn
   (setq ENTSET (ssadd)
         ENT2 ENT
   )
   (while (setq ENT2 (RFL:GETNEXTENT ENT2))
    (ssadd ENT2 ENTSET)
   )
   ENTSET
  )
  nil
 )
)
(defun RFL:GETALLPREVENT (ENT / ENT2 ENTSET)
 (if (= (type ENT) 'ENAME)
  (progn
   (setq ENTSET (ssadd)
         ENT2 ENT
   )
   (while (setq ENT2 (RFL:GETPREVENT ENT2))
    (ssadd ENT2 ENTSET)
   )
   ENTSET
  )
  nil
 )
)
(defun RFL:GETALLENT (ENT / ENT2 ENTSET)
 (if (= (type ENT) 'ENAME)
  (progn
   (setq ENTSET (ssadd ENT)
         ENT2 ENT
   )
   (while (setq ENT2 (RFL:GETNEXTENT ENT2))
    (ssadd ENT2 ENTSET)
   )
   (setq ENT2 ENT)
   (while (setq ENT2 (RFL:GETPREVENT ENT2))
    (ssadd ENT2 ENTSET)
   )
   ENTSET
  )
 )
)
(defun RFL:BREAKENT (ENT / NEXTENT PREVENT)
 (if (= (type ENT) 'ENAME)
  (progn
   (setq NEXTENT (RFL:GETNEXTENT ENT))
   (setq PREVENT (RFL:GETPREVENT ENT))
   (RFL:PUTNEXTENT PREVENT NEXTENT)
   (RFL:PUTPREVENT NEXTENT PREVENT)
   (entmod (list (cons -1 ENT) (list -3 (list "RFLTOOLS_XENT"))))
   ENT
  )
  nil
 )
);
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
;
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
;   Program written by Robert Livingston, 99/11/15
;
;   DPROF draws the current profile
;
;
(defun C:DPROF (/ ANGBASE ANGDIR CMDECHO OSMODE REP STEP STEPSTA)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq OSMODE (getvar "OSMODE"))
 (setvar "OSMODE" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 0)

 (if (/= nil PVILIST)
  (progn
   (RFL:PROFDEF)
   (initget "Yes No")
   (setq REP (getkword "\nDraw profile (<Yes>/No) :"))
   (if (= REP nil) (setq REP "Yes"))
   (if (= REP "Yes")
    (progn
     (initget "Yes No")
     (setq REP (getkword "\nErase current profile entities (Yes/<No>) :"))
     (if (= REP nil) (setq REP "No"))
     (if (= REP "Yes") (command "._ERASE" (ssget "X" (list (cons 8 (cdr (assoc "PLAYER" PROFDEF))))) ""))
     (RFL:DRAWPROF PVILIST)
    )
   )
   (initget "Yes No")
   (setq REP (getkword "\nCircle high/low points (Yes/<No>) :"))
   (if (= REP nil) (setq REP "No"))
   (if (= REP "Yes") (RFL:PROFHIGHLOW 1.0))
  )
  (progn
   (princ "\n*** PROFILE NOT SET - RUN GPROF OR RPROF ***\n")
  )
 )

 (setvar "CMDECHO" CMDECHO)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
)
(defun C:VCURVE (/ A ACTIVEDOC ACTIVESPACE B C CMDECHO D ENT ENTLIST G G1 G2 P P1 P2 P3 P4 PP
                   OSMODE TMP VEXAG X Y Z)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq OSMODE (getvar "OSMODE"))

 (vl-load-com)
 (setq ACTIVEDOC (vla-get-activedocument (vlax-get-acad-object)))
 (setq ACTIVESPC
       (vlax-get-property ACTIVEDOC
        (if (or (eq acmodelspace (vla-get-activespace ACTIVEDOC)) (eq :vlax-true (vla-get-mspace ACTIVEDOC)))
         'modelspace
         'paperspace
        )
       )
 )
 
 (setq ENT (car (entsel "\nSelect first tangent :")))
 (if (/= nil ENT)
  (progn
   (setq ENTLIST (entget ENT))
   (if (= (cdr (assoc 0 ENTLIST)) "LINE")
    (progn
     (command "._LIST" ENT)
     (setq P1 (cdr (assoc 10 ENTLIST)))
     (setq P2 (cdr (assoc 11 ENTLIST)))
     (setq ENT (car (entsel "\nSelect second tangent :")))
     (if (/= nil ENT)
      (progn
       (setq ENTLIST (entget ENT))
       (if (= (cdr (assoc 0 ENTLIST)) "LINE")
        (progn
         (command ENT)
         (setq P3 (cdr (assoc 10 ENTLIST)))
         (setq P4 (cdr (assoc 11 ENTLIST)))
         (setq P (inters P1 P2 P3 P4 nil))
         (if (/= nil P)
          (progn
           (setq VEXAG (getreal (strcat "\nEnter vertical exageration <" (rtos (if PROFDEF (cdr (assoc "VEXAG" PROFDEF)) 10.0)) "> :")))
           (if (= VEXAG nil) (setq VEXAG (if PROFDEF (cdr (assoc "VEXAG" PROFDEF)) 10.0)))
           (if (/= VEXAG nil)
            (progn
             (if (> VEXAG 0.0)
              (progn
               (if (> (distance P2 P) (distance P1 P))
                (setq P1 P2)
               )
               (if (> (distance P3 P) (distance P4 P))
                (setq P4 P3)
               )
               (setq K nil)
               (setq L nil)
               (setq PP nil)
               (setq G1 (/ (- (nth 1 P) (nth 1 P1))
                           (- (nth 0 P) (nth 0 P1))
                           VEXAG
                        )
               )
               (setq G2 (/ (- (nth 1 P4) (nth 1 P))
                           (- (nth 0 P4) (nth 0 P))
                           VEXAG
                        )
               )
               (while (or (= K nil) (= L nil) (= PP nil))
                (if (= L nil)
                 (progn
                  (setq L (getdist "\nEnter vertical curve 'L' (<return> for 'K') :"))
                  (if (= L nil)
                   (progn
                    (setq L -1.0)
                    (setq K nil)
                    (setq PP -1.0)
                   )
                   (progn
                    (setq K (abs (/ L (- G2 G1) 100.0)))
                    (setq PP -1.0)
                   )
                  )
                 )
                 (progn
                  (if (= K nil)
                   (progn
                    (setq K (getreal "\nEnter vertical curve 'K' (<return> for 'P') :"))
                    (if (= K nil)
                     (progn
                      (setq L -1.0)
                      (setq K -1.0)
                      (setq PP nil)
                     )
                     (progn
                      (setq L (abs (* K (- G2 G1) 100.0)))
                      (setq REP (getreal (strcat "\nL = " (rtos L) ", enter new value or <return> to accept :")))
                      (if (/= nil REP)
                       (progn
                        (setq L REP)
                        (setq K (abs (/ L (- G2 G1) 100.0)))
                       )
                      )
                      (setq PP -1.0)
                     )
                    )
                   )
                   (progn
                    (setq PP (getpoint "\nEnter through point (<return> for 'L') :"))
                    (if (= PP nil)
                     (progn
                      (setq L nil)
                      (setq K -1.0)
                      (setq PP -1.0)
                     )
                     (progn
                      (setq D (- (nth 0 P) (nth 0 PP)))
                      (setq G (- G2 G1))
                      (setq Z (- (/ (nth 1 PP) VEXAG)
                                 (- (/ (nth 1 P) VEXAG)
                                    (* G1 D)
                                 )
                              )
                      )
                      (setq A 0.25)
                      (setq B (* -1.0
                                 (+ D
                                    (/ (* 2.0 Z)
                                       G
                                    )
                                 )
                              )
                      )
                      (setq C (* D D))
                      (setq TMP (- (* B B) (* 4.0 A C)))
                      (if (< TMP 0.0)
                       (progn
                        (princ "\n*** No solution ***")
                        (setq L -1.0)
                        (setq K -1.0)
                        (setq PP nil)
                       )
                       (progn
                        (setq L (/ (- (sqrt TMP) B) (* 2.0 A)))
                        (if (< L 0.0)
                         (progn
                          (princ "\n*** No solution ***")
                          (setq L -1.0)
                          (setq K -1.0)
                          (setq PP nil)
                         )
                         (progn
                          (setq REP (getreal (strcat "\nL = " (rtos L) ", enter new value or <return> to accept :")))
                          (if (/= nil REP)
                           (progn
                            (setq L REP)
                           )
                          )
                          (setq K (abs (/ L (- G2 G1) 100.0)))
                         )
                        )
                       )
                      )
                     )
                    )
                   )
                  )
                 )
                )
               )
               (command)
               (setvar "OSMODE" 0)
               (setvar "ATTREQ" 0)
               (if (= nil (tblsearch "BLOCK" "PVI2")) (RFL:MAKEENT "PVI2"))
               (vla-insertblock ACTIVESPC
                                (vlax-3D-point P)
                                "PVI2"
                                25.4
                                25.4
                                25.4
                                0.0
               )
               (setq ENT (entlast))
               (setq ENTLIST (entget ENT))
               (if (= (cdr (assoc 66 ENTLIST)) 1)
                (progn
                 (setq ENT (entnext ENT))
                 (setq ENTLIST (entget ENT))
                 (while (/= (cdr (assoc 0 ENTLIST)) "SEQEND")
                  (if (= (cdr (assoc 2 ENTLIST)) "LENGTH")
                   (progn
                    (setq ENTLIST (subst (cons 1 (rtos L 2 8)) (assoc 1 ENTLIST) ENTLIST))
                    (entmod ENTLIST)
                    (entupd ENT)
                   )
                  )
                  (if (= (cdr (assoc 2 ENTLIST)) "K")
                   (progn
                    (setq ENTLIST (subst (cons 1 (rtos K 2 8)) (assoc 1 ENTLIST) ENTLIST))
                    (entmod ENTLIST)
                    (entupd ENT)
                   )
                  )
                  (setq ENT (entnext ENT))
                  (setq ENTLIST (entget ENT))
                 )
                )
               )
               (setvar "ATTREQ" 1)
               (setq P2 (list (- (nth 0 P) (/ L 2.0))
                              (- (nth 1 P) (* (/ L 2.0) G1 VEXAG))
                        )
               )
               (setq P3 (list (+ (nth 0 P) (/ L 2.0))
                              (+ (nth 1 P) (* (/ L 2.0) G2 VEXAG))
                        )
               )
               (RFL:DRAWPARABOLICVCURVE P2 P P3)
               (setvar "OSMODE" OSMODE)
              )
              (princ "\n**** NOT VALID ****")
             )
            )
           )
          )
          (princ "\n**** NO INTERSECTION FOUND ****")
         )
        )
        (princ "\n**** NOT A LINE ****")
       )
      )
     )
    )
    (princ "\n**** NOT A LINE ****")
   )
  )
 )
)
;
;
;   Program written by Robert Livingston, 99/11/15
;
;   DPROFOG draws on the current layer the current OG profile defined in OGLIST
;
;
(defun C:DPROFOG (/ ANGBASE ANGDIR CMDECHO OSMODE C)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq OSMODE (getvar "OSMODE"))
 (setvar "OSMODE" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 0)

 (if (/= nil OGLIST)
  (progn
   (RFL:PROFDEF)
   (setq C 0)
   (command "._PLINE")
   (while (< C (length OGLIST))
    (command (RFL:PROFPOINT (nth 0 (nth C OGLIST)) (nth 1 (nth C OGLIST))))
    (setq C (+ C 1))
   )
   (command "")
  )
  (progn
   (princ "\n*** OG PROFILE NOT SET ***\n")
  )
 )

 (setvar "CMDECHO" CMDECHO)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
)
;
;
;   Program written by Robert Livingston, 98/05/13
;
;   C:GPROF extracts a vertical alignment from the current drawing
;
;
(defun C:GPROF (/ ANGBASE ANGDIR C CMDECHO ENT ENTSET PVIENT PVISET STA ELEV LR VAL)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 0)

 (RFL:PROFDEF)

 (setq PVILIST nil)
 (if (/= nil PROFDEF)
  (progn
   (princ "Select PVI blocks (or <return> to find all based on profile definition block) : ")
   (setq ENTSET (ssget))
   (if (= nil ENTSET)
    (progn
     (setq PVISET (ssget "X" (list (cons 0 "INSERT")
                                   (cons -4 "<OR")
                                   (cons 2 "RFLPVI")
                                   (cons 2 "PVI2")
                                   (cons -4 "OR>")
                                   (cons 8 (cdr (assoc "PLAYER" PROFDEF))))))
    )
    (progn
     (setq C 0)
     (setq PVISET nil)
     (while (< C (sslength ENTSET))
      (setq ENT (ssname ENTSET C))
      (if (and (/= nil (cdr (assoc 2 (entget ENT))))
               (or (= "RFLPVI" (strcase (cdr (assoc 2 (entget ENT)))))
                   (= "PVI2" (strcase (cdr (assoc 2 (entget ENT)))))
               )
          )
       (if (= nil PVISET)
        (setq PVISET (ssadd ENT))
        (ssadd ENT PVISET)
       )
      )
      (setq C (+ C 1))
     )
    )
   )

   (if (= PVISET nil)
    (princ "\n*** NO PVI's EXIST ***\n")
    (if (= (sslength PVISET) 1)
     (princ "\n*** ONLY ONE PVI EXISTS ***\n")
     (progn
      (while (> (sslength PVISET) 0)
       (setq C 1)
       (setq PVIENT (ssname PVISET 0))
       (while (< C (sslength PVISET))
        (if (or (= (cdr (assoc "DIRECTION" PROFDEF)) 1) (= (assoc "DIRECTION" PROFDEF) nil))
         (if (< (nth 0 (cdr (assoc 10 (entget PVIENT))))
                (nth 0 (cdr (assoc 10 (entget (ssname PVISET C))))))
          (setq PVIENT (ssname PVISET C))
         )
         (if (> (nth 0 (cdr (assoc 10 (entget PVIENT))))
                (nth 0 (cdr (assoc 10 (entget (ssname PVISET C))))))
          (setq PVIENT (ssname PVISET C))
         )
        )
        (setq C (+ C 1))
       )
       (setq PVISET (ssdel PVIENT PVISET))
       (setq STA (+ (* (- (nth 0 (cdr (assoc 10 (entget PVIENT))))
                          (nth 0 (cdr (assoc "BPOINT" PROFDEF))))
                       (if (or (= (cdr (assoc "DIRECTION" PROFDEF)) 1) (= (assoc "DIRECTION" PROFDEF) nil)) 1.0 -1.0)
                    )
                    (cdr (assoc "STA" PROFDEF))
                 )
       )
       (setq ELEV (+ (/ (- (nth 1 (cdr (assoc 10 (entget PVIENT))))
                           (nth 1 (cdr (assoc "BPOINT" PROFDEF))))
                        (cdr (assoc "VEXAG" PROFDEF)))
                     (cdr (assoc "ELEV" PROFDEF))))
       (setq PVIENT (entnext PVIENT))
       (while (/= "SEQEND" (cdr (assoc 0 (entget PVIENT))))
        (if (= "R" (cdr (assoc 2 (entget PVIENT))))
         (if (/= "" (cdr (assoc 1 (entget PVIENT))))
          (progn
           (setq LR "R")
           (setq VAL (atof (cdr (assoc 1 (entget PVIENT)))))
          )
         )
        )
        (if (= "L" (cdr (assoc 2 (entget PVIENT))))
         (if (/= "" (cdr (assoc 1 (entget PVIENT))))
          (progn
           (setq LR "L")
           (setq VAL (atof (cdr (assoc 1 (entget PVIENT)))))
          )
         )
        )
        (if (= "LENGTH" (cdr (assoc 2 (entget PVIENT))))
         (progn
          (setq LR "L")
          (setq VAL (atof (cdr (assoc 1 (entget PVIENT)))))
         )
        )
        (setq PVIENT (entnext PVIENT))
       )
       (setq PVILIST (append (list (list STA ELEV LR VAL)) PVILIST))
      )
     )
    )
   )
  )
 )

 (setvar "CMDECHO" CMDECHO)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
)
;
;
;   Program written by Robert Livingston, 98/05/13
;
;   C:GPROFOG extracts an OG vertical alignment from the current drawing
;
;
(defun C:GPROFOG (/ ANGBASE ANGDIR CMDECHO ENT ENTLIST ELEV LR NODE NODEPREV P TOL)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 0)

 (setq TOL 0.0001)
 (setq NODEPREV nil)
 
 (RFL:PROFDEF)

 (setq OGLIST nil)
 (if (/= nil PROFDEF)
  (progn
   (princ "\nSelect OG polyline:")
   (setq ENT (car (entsel)))
   (if (= ENT nil)
    (setq ENTLIST nil)
    (setq ENTLIST (entget ENT))
   )
   (if (= nil ENT)
    (princ "\n*** NO ENTITY SELECTED ***\n")
    (if (/= (cdr (assoc 0 ENTLIST)) "LWPOLYLINE")
     (princ "\n*** NOT A R14 POLYLINE ***\n")
     (progn
      (while (/= ENTLIST nil)
       (setq NODE (car ENTLIST))
       (setq ENTLIST (cdr ENTLIST))
       (if (= (car NODE) 10)
        (if (or (= NODEPREV nil) (> (distance (cdr NODEPREV) (cdr NODE)) TOL))
         (progn
          (setq STA (+ (* (- (nth 0 (cdr NODE))
                             (nth 0 (cdr (assoc "BPOINT" PROFDEF))))
                          (if (or (= (cdr (assoc "DIRECTION" PROFDEF)) 1) (= (assoc "DIRECTION" PROFDEF) nil)) 1.0 -1.0)
                       )
                       (cdr (assoc "STA" PROFDEF))
                    )
          )
          (setq ELEV (+ (/ (- (nth 1 (cdr NODE))
                              (nth 1 (cdr (assoc "BPOINT" PROFDEF))))
                           (cdr (assoc "VEXAG" PROFDEF)))
                        (cdr (assoc "ELEV" PROFDEF))))
          (setq OGLIST (append (list (list STA ELEV)) OGLIST))
          (setq NODEPREV NODE)
         )
        )
       )
      )
      (if (> (nth 0 (car OGLIST)) (nth 0 (last OGLIST)))
       (setq OGLIST (reverse OGLIST))
      )
     )
    )
   )
  )
 )

 (setvar "CMDECHO" CMDECHO)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
)
;
;
;   Program written by Robert Livingston, 98/05/13
;
;   C:GPROFP extracts an vertical alignment from a selected polyline
;
;
(defun C:GPROFP (/ ANGBASE ANGDIR CMDECHO ENT ENTLIST ELEV LR NODE NODEPREV OGLIST P TOL)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 0)

 (setq TOL 0.0001)
 (setq NODEPREV nil)
 
 (RFL:PROFDEF)

 (setq OGLIST nil)
 (if (/= nil PROFDEF)
  (progn
   (princ "\nSelect polyline:")
   (setq ENT (car (entsel)))
   (if (= ENT nil)
    (setq ENTLIST nil)
    (setq ENTLIST (entget ENT))
   )
   (if (= nil ENT)
    (princ "\n*** NO ENTITY SELECTED ***\n")
    (if (/= (cdr (assoc 0 ENTLIST)) "LWPOLYLINE")
     (princ "\n*** NOT A R14 POLYLINE ***\n")
     (progn
      (while (/= ENTLIST nil)
       (setq NODE (car ENTLIST))
       (setq ENTLIST (cdr ENTLIST))
       (if (= (car NODE) 10)
        (if (or (= NODEPREV nil) (> (distance (cdr NODEPREV) (cdr NODE)) TOL))
         (progn
          (setq STA (+ (* (- (nth 0 (cdr NODE))
                             (nth 0 (cdr (assoc "BPOINT" PROFDEF))))
                          (if (or (= (cdr (assoc "DIRECTION" PROFDEF)) 1) (= (assoc "DIRECTION" PROFDEF) nil)) 1.0 -1.0)
                       )
                       (cdr (assoc "STA" PROFDEF))
                    )
          )
          (setq ELEV (+ (/ (- (nth 1 (cdr NODE))
                              (nth 1 (cdr (assoc "BPOINT" PROFDEF))))
                           (cdr (assoc "VEXAG" PROFDEF)))
                        (cdr (assoc "ELEV" PROFDEF))))
          (setq OGLIST (append (list (list STA ELEV)) OGLIST))
          (setq NODEPREV NODE)
         )
        )
       )
      )
      (if (> (nth 0 (car OGLIST)) (nth 0 (last OGLIST)))
       (setq OGLIST (reverse OGLIST))
      )
     )
    )
   )
  )
 )
 (if (/= nil OGLIST)
  (progn
   (setq PVILIST nil)
   (foreach NODE OGLIST
    (progn
     (setq PVILIST (append PVILIST (list (list (car NODE) (cadr NODE) "L" 0.0))))
    )
   )
  )
 )

 (setvar "CMDECHO" CMDECHO)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
);
;
;   Program written by Robert Livingston, 98/05/13
;
;   C:RPROF reads a vertical alignment from file
;
;
(defun C:RPROF (/ CMDECHO INFILENAME)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (if (= (vl-registry-read "HKEY_CURRENT_USER\\rflAlignDirectory") nil)
  (vl-registry-write "HKEY_CURRENT_USER\\rflAlignDirectory" "" "")
 )
 (setq INFILENAME (getfiled "Select a Vertical Alignment File" (vl-registry-read "HKEY_CURRENT_USER\\rflAlignDirectory") "vrt" 2))
 (RFL:RPROF INFILENAME)
 (setvar "CMDECHO" CMDECHO)
)
;
;
;   Program written by Robert Livingston, 98/05/13
;
;   C:RPROFOG reads an OG vertical alignment from file and sets the global variable OGLIST
;
;
(defun C:RPROFOG (/ CMDECHO INFILENAME)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (setq INFILENAME (getfiled "Select an OG Vertical Alignment File" "" "vrt" 2))
 (RFL:RPROFOG INFILENAME)
 (setvar "CMDECHO" CMDECHO)
)
;
;
;   Program written by Robert Livingston, 98/05/13
;
;   C:WPROF writes a vertical alignment to file
;
;
(defun C:WPROF (/ CMDECHO OUTFILENAME)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (if (= (vl-registry-read "HKEY_CURRENT_USER\\rflAlignDirectory") nil)
  (vl-registry-write "HKEY_CURRENT_USER\\rflAlignDirectory" "" "")
 )
 (if (= PVILIST nil)
  (princ "\n*** NO VERTICAL EXISTS - USE RPROF OR GPROF ***\n")
  (progn
   (setq OUTFILENAME (getfiled "Select a Vertical Alignment File" (vl-registry-read "HKEY_CURRENT_USER\\rflAlignDirectory") "vrt" 1))
   (RFL:WPROF OUTFILENAME)
  )
 )
 (setvar "CMDECHO" CMDECHO)
)
;
;
;   Program written by Robert Livingston, 98/05/13
;
;   C:WPROFOG writes a vertical alignment to file
;
;
(defun C:WPROFOG (/ C CMDECHO OUTFILE OUTFILENAME)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (if (= OGLIST nil)
  (princ "\n*** NO OG EXISTS - USE GPROFOG ***\n")
  (progn
   (setq OUTFILENAME (getfiled "Select a Vertical OG File" "" "vrt" 1))
   (RFL:WPROFOG OUTFILENAME)
  )
 )

 (setvar "CMDECHO" CMDECHO)
)
;
;
;     Program written by Robert Livingston, 2016/07/07
;
;     C:LALIGN is a utility for labelling alignments
;
;
;     NODEMODE = 0  :  LEFT
;     NODEMODE = 1  :  RIGHT
;     NODEMODE = 2  :  INSIDE
;     NODEMODE = 3  :  OUTSIDE
;
;     xxxLAYER  :  '*' concatinates current layer
;
(setq RFL:LALIGNLIST (list (cons "LABELBLOCK" "STALBL")
                           (cons "LABEL" 1)
                           (cons "LABELLAYER" "*-LBL")
                           (cons "LABELINC" 100.0)
                           (cons "LABELSCALE" 1.0)
                           (cons "LABELOFFSET" 4.0)
                           (cons "LABELROTATE" 0.0)
                           (cons "TICKBLOCK" "STATICK")
                           (cons "TICK" 1)
                           (cons "TICKLAYER" "*-LBL")
                           (cons "TICKINC" 20.0)
                           (cons "TICKSCALE" 1.0)
                           (cons "TICKOFFSET" 0.0)
                           (cons "TICKROTATE" 0.0)
                           (cons "NODELEFTBLOCK" "STANODELEFT")
                           (cons "NODERIGHTBLOCK" "STANODERIGHT")
                           (cons "NODE" 1)
                           (cons "NODELAYER" "*-LBL")
                           (cons "NODEMODE" 3)
                           (cons "NODESCALE" 1.0)
                           (cons "NODEOFFSET" 0.0)
                           (cons "NODEROTATE" 0.0)
                     )
)
(defun C:LALIGN (/ ACTIVEDOC ACTIVESPC ENT ENTLIST LLABEL LTICK LNODE P P1 PREVENT)
 (command "._UNDO" "M")
 (vl-load-com)
 (setq ACTIVEDOC (vla-get-activedocument (vlax-get-acad-object)))
 (setq ACTIVESPC
       (vlax-get-property ACTIVEDOC
        (if (or (eq acmodelspace (vla-get-activespace ACTIVEDOC)) (eq :vlax-true (vla-get-mspace ACTIVEDOC)))
         'modelspace
         'paperspace
        )
       )
 )
 (defun LLABEL (/ ANGBASE ANGDIR CLAYER INC NLAYER P STA STAH STAL STAMAX)
  (setq ANGBASE (getvar "ANGBASE"))
  (setvar "ANGBASE" 0.0)
  (setq ANGDIR (getvar "ANGDIR"))
  (setvar "ANGDIR" 0)
  (setq CLAYER (getvar "CLAYER"))
  (setq NLAYER (cdr (assoc "LABELLAYER" RFL:LALIGNLIST)))
  (if (= "*" (substr NLAYER 1 1)) (setq NLAYER (strcat CLAYER (substr NLAYER 2))))
  (if (not (tblsearch "LAYER" NLAYER))
   (entmake (list (cons 0 "LAYER")
                  (cons 100 "AcDbSymbolTableRecord")
                  (cons 100 "AcDbLayerTableRecord")
                  (cons 2 NLAYER)
                  (cons 70 0)
            )
   )
  )
  (setvar "CLAYER" NLAYER)
  (if (not (tblsearch "BLOCK" (cdr (assoc "LABELBLOCK" RFL:LALIGNLIST))))
   (RFL:MAKEENT (cdr (assoc "LABELBLOCK" RFL:LALIGNLIST)))
  )
  (if (tblsearch "BLOCK" (cdr (assoc "LABELBLOCK" RFL:LALIGNLIST)))
   (progn
    (setq STA (float (* (fix (/ (caar ALIGNLIST)
                                (cdr (assoc "LABELINC" RFL:LALIGNLIST))
                             )
                        )
                        (cdr (assoc "LABELINC" RFL:LALIGNLIST))
                     )
              )
    )
    (setq STAEND (+ (caar ALIGNLIST) (RFL:GETALIGNLENGTH)))
    (setq INC (cdr (assoc "LABELINC" RFL:LALIGNLIST)))
    (while (<= STA STAEND)
     (if (setq P (RFL:XY (list STA (cdr (assoc "LABELOFFSET" RFL:LALIGNLIST)))))
      (progn
       (setq P1 (RFL:XY (list STA (- (cdr (assoc "LABELOFFSET" RFL:LALIGNLIST)) 1))))
       (vla-insertblock ACTIVESPC
                        (vlax-3D-point P)
                        (cdr (assoc "LABELBLOCK" RFL:LALIGNLIST))
                        (cdr (assoc "LABELSCALE" RFL:LALIGNLIST))
                        (cdr (assoc "LABELSCALE" RFL:LALIGNLIST))
                        (cdr (assoc "LABELSCALE" RFL:LALIGNLIST))
                        (+ (/ pi 2.0) (angle P1 P) (cdr (assoc "LABELROTATE" RFL:LALIGNLIST)))
       )
       (setq ENT (entlast))
       (RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
       (if (= 1 (cdr (assoc 66 (setq ENTLIST (entget ENT)))))
        (progn
         (setq STAH (RFL:STATXT STA))
         (setq STAL (substr STAH (+ 2 (vl-string-search "+" STAH))))
         (setq STAH (substr STAH 1 (vl-string-search "+" STAH)))
         (setq ENT (entnext ENT))
         (setq ENTLIST (entget ENT))
         (while (= "ATTRIB" (cdr (assoc 0 ENTLIST)))
          (cond ((= "STAH" (cdr (assoc 2 ENTLIST)))
                 (entmod (subst (cons 1 STAH) (assoc 1 ENTLIST) ENTLIST))
                )
                ((= "STAL" (cdr (assoc 2 ENTLIST)))
                 (entmod (subst (cons 1 STAL) (assoc 1 ENTLIST) ENTLIST))
                )
           )
          (setq ENT (entnext ENT))
          (setq ENTLIST (entget ENT))
         )
        )
       )
      )
     )
     (setq STA (+ STA INC))
    )
   )
   (princ "\n!!! Unable to locate or create Lable Block !!!")
  )
  (setvar "CLAYER" CLAYER)
  (setvar "ANGBASE" ANGBASE)
  (setvar "ANGDIR" ANGDIR)
  1
 )
 (defun LTICK (/ ANGBASE ANGDIR CLAYER INC NLAYER P STA STAMAX)
  (setq ANGBASE (getvar "ANGBASE"))
  (setvar "ANGBASE" 0.0)
  (setq ANGDIR (getvar "ANGDIR"))
  (setvar "ANGDIR" 0)
  (setq CLAYER (getvar "CLAYER"))
  (setq NLAYER (cdr (assoc "TICKLAYER" RFL:LALIGNLIST)))
  (if (= "*" (substr NLAYER 1 1)) (setq NLAYER (strcat CLAYER (substr NLAYER 2))))
  (if (not (tblsearch "LAYER" NLAYER))
   (entmake (list (cons 0 "LAYER")
                  (cons 100 "AcDbSymbolTableRecord")
                  (cons 100 "AcDbLayerTableRecord")
                  (cons 2 NLAYER)
                  (cons 70 0)
            )
   )
  )
  (setvar "CLAYER" NLAYER)
  (if (not (tblsearch "BLOCK" (cdr (assoc "TICKBLOCK" RFL:LALIGNLIST))))
   (RFL:MAKEENT (cdr (assoc "TICKBLOCK" RFL:LALIGNLIST)))
  )
  (if (tblsearch "BLOCK" (cdr (assoc "LABELBLOCK" RFL:LALIGNLIST)))
   (progn
    (setq STA (float (* (fix (/ (caar ALIGNLIST)
                                (cdr (assoc "TICKINC" RFL:LALIGNLIST))
                             )
                        )
                        (cdr (assoc "TICKINC" RFL:LALIGNLIST))
                     )
              )
    )
    (setq STAEND (+ (caar ALIGNLIST) (RFL:GETALIGNLENGTH)))
    (setq INC (cdr (assoc "TICKINC" RFL:LALIGNLIST)))
    (while (<= STA STAEND)
     (if (setq P (RFL:XY (list STA (cdr (assoc "TICKOFFSET" RFL:LALIGNLIST)))))
      (progn
       (setq P1 (RFL:XY (list STA (- (cdr (assoc "TICKOFFSET" RFL:LALIGNLIST)) 1))))
       (vla-insertblock ACTIVESPC
                        (vlax-3D-point P)
                        (cdr (assoc "TICKBLOCK" RFL:LALIGNLIST))
                        (cdr (assoc "TICKSCALE" RFL:LALIGNLIST))
                        (cdr (assoc "TICKSCALE" RFL:LALIGNLIST))
                        (cdr (assoc "TICKSCALE" RFL:LALIGNLIST))
                        (+ (/ pi 2.0) (angle P1 P) (cdr (assoc "TICKROTATE" RFL:LALIGNLIST)))
       )
       (setq ENT (entlast))
       (RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
      )
     )
     (setq STA (+ STA INC))
    )
   )
   (princ "\n!!! Unable to locate or create Tick Block !!!")
  )
  (setvar "CLAYER" CLAYER)
  (setvar "ANGBASE" ANGBASE)
  (setvar "ANGDIR" ANGDIR)
  1
 )
 (defun LNODE (/ ANGBASE ANGDIR CLAYER NODEDIR NODELABEL NODETYPE NLAYER NODE NODEBLOCK NODEPREV P STA STAH STAMAX)
  (defun NODETYPE (NODE / TOL)
   ; 0 = Tangent
   ; 1 = Arc
   ; 2 = Spiral
   (setq TOL 0.00000001)
   (if NODE
    (if (listp (last NODE))
     2
     (if (< (abs (last NODE)) TOL)
      0
      1
     )
    )
    nil
   )
  )
  (defun NODEDIR (NODE / DIR TOL)
   ; 0 = Tangent
   ; -1 = Right
   ; 1 = Left
   (setq TOL 0.00000001)
   (if NODE
    (if (listp (last NODE))
     (progn
      (setq DIR (RFL:SIGN (sin (- (angle (nth 1 (last NODE))  (nth 2 (last NODE)))
                                  (angle (nth 0 (last NODE))  (nth 1 (last NODE)))
                               )
                          )
                )
      )
      (if (> (distance (nth 2 NODE) (nth 2 (last NODE))) TOL)
       (setq DIR (* -1 DIR))
      )
      (fix DIR)
     )
     (if (< (abs (last NODE)) TOL)
      0
      (if (< (last NODE) 0.0)
       -1
       1
      )
     )
    )
    nil
   )
  )
  (defun NODELABEL (NODE NODEPREV)
   (cond ((and (= (NODETYPE NODE) 0)
               (= (NODETYPE NODEPREV) nil)
          )
          "POT "
         )
         ((and (= (NODETYPE NODE) 0)
               (= (NODETYPE NODEPREV) 0)
          )
          "PI "
         )
         ((and (= (NODETYPE NODE) 0)
               (= (NODETYPE NODEPREV) 1)
          )
          "EC "
         )
         ((and (= (NODETYPE NODE) 0)
               (= (NODETYPE NODEPREV) 2)
          )
          "ST "
         )
         ((and (= (NODETYPE NODE) 1)
               (= (NODETYPE NODEPREV) nil)
          )
          "POC "
         )
         ((and (= (NODETYPE NODE) 1)
               (= (NODETYPE NODEPREV) 0)
          )
          "BC "
         )
         ((and (= (NODETYPE NODE) 1)
               (= (NODETYPE NODEPREV) 1)
          )
          (if (= (NODEDIR NODE) (NODEDIR NODEPREV))
           "PCC "
           "EC/BC "
          )
         )
         ((and (= (NODETYPE NODE) 1)
               (= (NODETYPE NODEPREV) 2)
          )
          "SC "
         )
         ((and (= (NODETYPE NODE) 2)
               (= (NODETYPE NODEPREV) nil)
          )
          "POS "
         )
         ((and (= (NODETYPE NODE) 2)
               (= (NODETYPE NODEPREV) 0)
          )
          "TS "
         )
         ((and (= (NODETYPE NODE) 2)
               (= (NODETYPE NODEPREV) 1)
          )
          "CS "
         )
         ((and (= (NODETYPE NODE) 2)
               (= (NODETYPE NODEPREV) 2)
          )
          "S/S "
         )
         (T
          ""
         )
   )
  )
  (defun NODEINSERT (STA NODESTR)
   (cond ((= (cdr (assoc "NODEMODE" RFL:LALIGNLIST)) 0)
          (setq NODEBLOCK (cdr (assoc "NODELEFTBLOCK" RFL:LALIGNLIST)))
         )
         ((= (cdr (assoc "NODEMODE" RFL:LALIGNLIST)) 1)
          (setq NODEBLOCK (cdr (assoc "NODERIGHTBLOCK" RFL:LALIGNLIST)))
         )
         ((= (cdr (assoc "NODEMODE" RFL:LALIGNLIST)) 2)
          (cond ((= (NODEDIR NODE) nil)
                 (cond ((= (NODEDIR NODEPREV) nil)
                        (setq NODEBLOCK (cdr (assoc "NODERIGHTBLOCK" RFL:LALIGNLIST)))
                       )
                       ((= (NODEDIR NODEPREV) -1)
                        (setq NODEBLOCK (cdr (assoc "NODERIGHTBLOCK" RFL:LALIGNLIST)))
                       )
                       ((= (NODEDIR NODEPREV) 0)
                        (setq NODEBLOCK (cdr (assoc "NODERIGHTBLOCK" RFL:LALIGNLIST)))
                       )
                       ((= (NODEDIR NODEPREV) 1)
                        (setq NODEBLOCK (cdr (assoc "NODELEFTBLOCK" RFL:LALIGNLIST)))
                       )
                       (T
                        (setq NODEBLOCK (cdr (assoc "NODERIGHTBLOCK" RFL:LALIGNLIST)))
                       )
                 )
                )
                ((= (NODEDIR NODE) -1)
                 (setq NODEBLOCK (cdr (assoc "NODERIGHTBLOCK" RFL:LALIGNLIST)))
                )
                ((= (NODEDIR NODE) 0)
                 (cond ((= (NODEDIR NODEPREV) nil)
                        (setq NODEBLOCK (cdr (assoc "NODERIGHTBLOCK" RFL:LALIGNLIST)))
                       )
                       ((= (NODEDIR NODEPREV) -1)
                        (setq NODEBLOCK (cdr (assoc "NODERIGHTBLOCK" RFL:LALIGNLIST)))
                       )
                       ((= (NODEDIR NODEPREV) 0)
                        (setq NODEBLOCK (cdr (assoc "NODERIGHTBLOCK" RFL:LALIGNLIST)))
                       )
                       ((= (NODEDIR NODEPREV) 1)
                        (setq NODEBLOCK (cdr (assoc "NODELEFTBLOCK" RFL:LALIGNLIST)))
                       )
                       (T
                        (setq NODEBLOCK (cdr (assoc "NODERIGHTBLOCK" RFL:LALIGNLIST)))
                       )
                 )
                )
                ((= (NODEDIR NODE) 1)
                 (setq NODEBLOCK (cdr (assoc "NODELEFTBLOCK" RFL:LALIGNLIST)))
                )
                (T
                 (setq NODEBLOCK (cdr (assoc "NODERIGHTBLOCK" RFL:LALIGNLIST)))
                )
          )
         )
         ((= (cdr (assoc "NODEMODE" RFL:LALIGNLIST)) 3)
          (cond ((= (NODEDIR NODE) nil)
                 (cond ((= (NODEDIR NODEPREV) nil)
                        (setq NODEBLOCK (cdr (assoc "NODELEFTBLOCK" RFL:LALIGNLIST)))
                       )
                       ((= (NODEDIR NODEPREV) -1)
                        (setq NODEBLOCK (cdr (assoc "NODELEFTBLOCK" RFL:LALIGNLIST)))
                       )
                       ((= (NODEDIR NODEPREV) 0)
                        (setq NODEBLOCK (cdr (assoc "NODELEFTBLOCK" RFL:LALIGNLIST)))
                       )
                       ((= (NODEDIR NODEPREV) 1)
                        (setq NODEBLOCK (cdr (assoc "NODERIGHTBLOCK" RFL:LALIGNLIST)))
                       )
                       (T
                        (setq NODEBLOCK (cdr (assoc "NODELEFTBLOCK" RFL:LALIGNLIST)))
                       )
                 )
                )
                ((= (NODEDIR NODE) -1)
                 (setq NODEBLOCK (cdr (assoc "NODELEFTBLOCK" RFL:LALIGNLIST)))
                )
                ((= (NODEDIR NODE) 0)
                 (cond ((= (NODEDIR NODEPREV) nil)
                        (setq NODEBLOCK (cdr (assoc "NODELEFTBLOCK" RFL:LALIGNLIST)))
                       )
                       ((= (NODEDIR NODEPREV) -1)
                        (setq NODEBLOCK (cdr (assoc "NODELEFTBLOCK" RFL:LALIGNLIST)))
                       )
                       ((= (NODEDIR NODEPREV) 0)
                        (setq NODEBLOCK (cdr (assoc "NODELEFTBLOCK" RFL:LALIGNLIST)))
                       )
                       ((= (NODEDIR NODEPREV) 1)
                        (setq NODEBLOCK (cdr (assoc "NODERIGHTBLOCK" RFL:LALIGNLIST)))
                       )
                       (T
                        (setq NODEBLOCK (cdr (assoc "NODELEFTBLOCK" RFL:LALIGNLIST)))
                       )
                 )
                )
                ((= (NODEDIR NODE) 1)
                 (setq NODEBLOCK (cdr (assoc "NODERIGHTBLOCK" RFL:LALIGNLIST)))
                )
                (T
                 (setq NODEBLOCK (cdr (assoc "NODELEFTBLOCK" RFL:LALIGNLIST)))
                )
          )
         )
         (T
          (setq NODEBLOCK (cdr (assoc "NODELEFTBLOCK" RFL:LALIGNLIST)))
         )
   )
   (if (setq P (RFL:XY (list STA (cdr (assoc "NODEOFFSET" RFL:LALIGNLIST)))))
    (progn
     (setq P1 (RFL:XY (list STA (- (cdr (assoc "NODEOFFSET" RFL:LALIGNLIST)) 1))))
     (vla-insertblock ACTIVESPC
                      (vlax-3D-point P)
                      NODEBLOCK
                      (cdr (assoc "NODESCALE" RFL:LALIGNLIST))
                      (cdr (assoc "NODESCALE" RFL:LALIGNLIST))
                      (cdr (assoc "NODESCALE" RFL:LALIGNLIST))
                      (+ (/ pi 2.0) (angle P1 P) (cdr (assoc "NODEROTATE" RFL:LALIGNLIST)))
     )
     (setq ENT (entlast))
     (RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
     (if (= 1 (cdr (assoc 66 (setq ENTLIST (entget ENT)))))
      (progn
       (setq ENT (entnext ENT))
       (setq ENTLIST (entget ENT))
       (while (= "ATTRIB" (cdr (assoc 0 ENTLIST)))
        (cond ((= "NODE" (cdr (assoc 2 ENTLIST)))
               (entmod (subst (cons 1 NODESTR) (assoc 1 ENTLIST) ENTLIST))
              )
         )
        (setq ENT (entnext ENT))
        (setq ENTLIST (entget ENT))
       )
      )
     )
    )
   )
  )
  (setq ANGBASE (getvar "ANGBASE"))
  (setvar "ANGBASE" 0.0)
  (setq ANGDIR (getvar "ANGDIR"))
  (setvar "ANGDIR" 0)
  (setq CLAYER (getvar "CLAYER"))
  (setq NLAYER (cdr (assoc "NODELAYER" RFL:LALIGNLIST)))
  (if (= "*" (substr NLAYER 1 1)) (setq NLAYER (strcat CLAYER (substr NLAYER 2))))
  (if (not (tblsearch "LAYER" NLAYER))
   (entmake (list (cons 0 "LAYER")
                  (cons 100 "AcDbSymbolTableRecord")
                  (cons 100 "AcDbLayerTableRecord")
                  (cons 2 NLAYER)
                  (cons 70 0)
            )
   )
  )
  (setvar "CLAYER" NLAYER)
  (if (not (tblsearch "BLOCK" (cdr (assoc "NODELEFTBLOCK" RFL:LALIGNLIST))))
   (RFL:MAKEENT (cdr (assoc "NODELEFTBLOCK" RFL:LALIGNLIST)))
  )
  (if (not (tblsearch "BLOCK" (cdr (assoc "NODERIGHTBLOCK" RFL:LALIGNLIST))))
   (RFL:MAKEENT (cdr (assoc "NODERIGHTBLOCK" RFL:LALIGNLIST)))
  )
  (if (and (tblsearch "BLOCK" (cdr (assoc "NODELEFTBLOCK" RFL:LALIGNLIST)))
           (tblsearch "BLOCK" (cdr (assoc "NODERIGHTBLOCK" RFL:LALIGNLIST)))
      )
   (progn
    (setq NODEPREV nil)
    (foreach NODE ALIGNLIST
     (setq STA (car NODE))
     (setq STAH (RFL:STATXT STA))
     (setq STAH (strcat (NODELABEL NODE NODEPREV) STAH))
     (NODEINSERT STA STAH)
     (setq NODEPREV NODE)
    )
    (setq STA (+ (caar ALIGNLIST) (RFL:GETALIGNLENGTH)))
    (setq STAH (RFL:STATXT STA))
    (setq STAH (strcat (NODELABEL (last ALIGNLIST) nil) STAH))
    (NODEINSERT STA STAH)
   )
   (princ "\n!!! Unable to locate or create Lable Block !!!")
  )
  (setvar "CLAYER" CLAYER)
  (setvar "ANGBASE" ANGBASE)
  (setvar "ANGDIR" ANGDIR)
  1
 )
 (if ALIGNLIST
  (progn
   (setq PREVENT nil)
   (if (= 1 (cdr (assoc "LABEL" RFL:LALIGNLIST))) (LLABEL))
   (if (= 1 (cdr (assoc "TICK" RFL:LALIGNLIST))) (LTICK))
   (if (= 1 (cdr (assoc "NODE" RFL:LALIGNLIST))) (LNODE))
   T
  )
  (progn
   (princ "\n!!! No alignment defined !!!")
   nil
  )
 )
)
