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
(defun RFL:ALIGNDEF (ALIGNENT PSTART STASTART / ALIGNENTLIST ALIGNENTSET AL BULGE FINDENT P P1 P2 R RFLAG STA)
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
     (setq ALSAVE RFL:ALIGNLIST)
     (setq RFL:ALIGNLIST AL)
     (if (= nil RFL:ALIGNLIST)
      (progn
       (setq RFL:ALIGNLIST ALSAVE)
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
           (setq RFL:ALIGNLIST ALSAVE)
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
           (setq RFL:ALIGNLIST ALSAVE)
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
 (if (/= nil RFL:ALIGNLIST)
  (progn
   (setq AL (last RFL:ALIGNLIST))
   (if (<= STA (+ (car AL) (RFL:ARCLENGTH (cadr AL) (caddr AL) (cadddr AL))))
    (progn
     (setq C 0)
     (setq AL (nth C RFL:ALIGNLIST))
     (if (>= STA (car AL))
      (progn
       (while (> STA (+ (car AL) (RFL:ARCLENGTH (cadr AL) (caddr AL) (cadddr AL))))
        (setq C (+ C 1))
        (setq AL (nth C RFL:ALIGNLIST))
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
 (setq ALLIST RFL:ALIGNLIST)
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
 (setq ALLIST RFL:ALIGNLIST)
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
 (setq ALLIST RFL:ALIGNLIST)
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
   (setq RFL:ALIGNLIST nil)
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
      (setq RFL:ALIGNLIST (append RFL:ALIGNLIST (list (list STA (list P1X P1Y) (list P2X P2Y) BULGE))))
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
     (while (< C (length RFL:ALIGNLIST))
      (princ (rtos (nth 0 (nth C RFL:ALIGNLIST)) 2 16) OUTFILE)
      (princ "\n" OUTFILE)
      (princ (rtos (nth 0 (nth 1 (nth C RFL:ALIGNLIST))) 2 16) OUTFILE)
      (princ "\n" OUTFILE)
      (princ (rtos (nth 1 (nth 1 (nth C RFL:ALIGNLIST))) 2 16) OUTFILE)
      (princ "\n" OUTFILE)
      (princ (rtos (nth 0 (nth 2 (nth C RFL:ALIGNLIST))) 2 16) OUTFILE)
      (princ "\n" OUTFILE)
      (princ (rtos (nth 1 (nth 2 (nth C RFL:ALIGNLIST))) 2 16) OUTFILE)
      (princ "\n" OUTFILE)
      (if (listp (nth 3 (nth C RFL:ALIGNLIST)))
       (progn
        (princ "SPIRAL\n" OUTFILE)
        (princ (rtos (nth 0 (nth 0 (nth 3 (nth C RFL:ALIGNLIST)))) 2 16) OUTFILE)
        (princ "\n" OUTFILE)
        (princ (rtos (nth 1 (nth 0 (nth 3 (nth C RFL:ALIGNLIST)))) 2 16) OUTFILE)
        (princ "\n" OUTFILE)
        (princ (rtos (nth 0 (nth 1 (nth 3 (nth C RFL:ALIGNLIST)))) 2 16) OUTFILE)
        (princ "\n" OUTFILE)
        (princ (rtos (nth 1 (nth 1 (nth 3 (nth C RFL:ALIGNLIST)))) 2 16) OUTFILE)
        (princ "\n" OUTFILE)
        (princ (rtos (nth 0 (nth 2 (nth 3 (nth C RFL:ALIGNLIST)))) 2 16) OUTFILE)
        (princ "\n" OUTFILE)
        (princ (rtos (nth 1 (nth 2 (nth 3 (nth C RFL:ALIGNLIST)))) 2 16) OUTFILE)
        (princ "\n" OUTFILE)
        (princ (rtos (nth 3 (nth 3 (nth C RFL:ALIGNLIST))) 2 16) OUTFILE)
       )
       (progn
        (princ (rtos (nth 3 (nth C RFL:ALIGNLIST)) 2 16) OUTFILE)
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
(if RFL:STAOFF (princ "\nRFL:STAOFF already loaded...")
(defun RFL:STAOFF (P / ANG ANG1 ANG2 AL C D D1 D11 D2 D22 OFFSET
                       P1 P2 PLT PLTST PST LO
                       OFFSETBEST PC R STA STABEST TMP)
 (setq STABEST nil)
 (setq OFFSETBEST nil)
 (if (/= RFL:ALIGNLIST nil)
  (progn
   (setq C 0)
   (setq AL (nth C RFL:ALIGNLIST))
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
    (setq AL (nth C RFL:ALIGNLIST))
   )
  )
 )
 (if (= STABEST nil)
  (eval nil)
  (list STABEST OFFSETBEST)
 )
)
)
;
;
;   Program written by Robert Livingston, 98/06/12
;
;   RFL:XY returns a list of (X Y) for a provided (STA OFFSET)
;
;
(if RFL:XY (princ "\nRFL:XY already loaded...")
(defun RFL:XY (P / ANG AL ALTMP C D OFFSET P1 P2 PC POINT STA X Y TOL)
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
 (if (/= nil RFL:ALIGNLIST)
  (progn
   (setq STA (car P))
   (setq OFFSET (cadr P))
   (setq AL (last RFL:ALIGNLIST))
   (if (<= STA (+ (car AL) (RFL:DIST (cadr AL) (caddr AL) (cadddr AL))))
    (progn
     (setq AL (car RFL:ALIGNLIST))
     (setq ALTMP (cdr RFL:ALIGNLIST))
     (if (>= STA (car AL))
      (progn
       (while (> STA (+ (car AL) (RFL:DIST (cadr AL) (caddr AL) (cadddr AL))))
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
)
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
;
;
;     Program written by Robert Livingston, 2016/07/05
;
;     RFL:LALIGNSTALBL is a utility for placing STALBL blocks along and alignment
;
;
(defun RFL:LALIGNSTALBL (STASTART STAEND INC OS R / HANDLE HANDLEPREV)
 (if RFL:ALIGNLIST
  (progn
  )
  (princ "\n*** No alignment defined! ***\n")
 )
 nil
)
;
;
;   Program written by Robert Livingston, 2008/11/04
;
;   RFL:RALIGNB reads a horizontal alignment from a RFLAlign Block
;
;
(defun RFL:RALIGNB (BLKENT / ENT ENTLIST INLINE LO P1X P1Y P2X P2Y
                             PLTX PLTY PLTSTX PLTSTY PSTX PSTY BULGE)
 (setq RFL:ALIGNLIST nil)
 (setq ENT (entnext BLKENT))
 (setq ENTLIST (entget ENT))
 (while (/= "HOR" (cdr (assoc 2 ENTLIST)))
  (setq ENT (entnext ENT))
  (setq ENTLIST (entget ENT))
 )
 (setq INLINE (cdr (assoc 1 ENTLIST)))
 (setq ENT (entnext ENT))
 (setq ENTLIST (entget ENT))
 (if (/= INLINE "#RFL HORIZONTAL ALIGNMENT FILE")
  (progn
   (princ "\n*** FILE NOT FORMATTED CORRECTLY ***\n")
  )
  (progn
   (setq INLINE (cdr (assoc 1 ENTLIST)))
   (setq ENT (entnext ENT))
   (setq ENTLIST (entget ENT))
   (while (and (/= nil INLINE) (/= INLINE "#END DEFINITION"))
    (setq STA (atof INLINE))
    (setq INLINE (cdr (assoc 1 ENTLIST)))
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
    (setq P1X (atof INLINE))
    (setq INLINE (cdr (assoc 1 ENTLIST)))
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
    (setq P1Y (atof INLINE))
    (setq INLINE (cdr (assoc 1 ENTLIST)))
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
    (setq P2X (atof INLINE))
    (setq INLINE (cdr (assoc 1 ENTLIST)))
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
    (setq P2Y (atof INLINE))
    (setq INLINE (cdr (assoc 1 ENTLIST)))
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
    (if (= INLINE "SPIRAL")
     (progn
      (setq INLINE (cdr (assoc 1 ENTLIST)))
      (setq ENT (entnext ENT))
      (setq ENTLIST (entget ENT))
      (setq PLTX (atof INLINE))
      (setq INLINE (cdr (assoc 1 ENTLIST)))
      (setq ENT (entnext ENT))
      (setq ENTLIST (entget ENT))
      (setq PLTY (atof INLINE))
      (setq INLINE (cdr (assoc 1 ENTLIST)))
      (setq ENT (entnext ENT))
      (setq ENTLIST (entget ENT))
      (setq PLTSTX (atof INLINE))
      (setq INLINE (cdr (assoc 1 ENTLIST)))
      (setq ENT (entnext ENT))
      (setq ENTLIST (entget ENT))
      (setq PLTSTY (atof INLINE))
      (setq INLINE (cdr (assoc 1 ENTLIST)))
      (setq ENT (entnext ENT))
      (setq ENTLIST (entget ENT))
      (setq PSTX (atof INLINE))
      (setq INLINE (cdr (assoc 1 ENTLIST)))
      (setq ENT (entnext ENT))
      (setq ENTLIST (entget ENT))
      (setq PSTY (atof INLINE))
      (setq INLINE (cdr (assoc 1 ENTLIST)))
      (setq ENT (entnext ENT))
      (setq ENTLIST (entget ENT))
      (setq LO (atof INLINE))
      (setq BULGE (list (list PLTX PLTY) (list PLTSTX PLTSTY) (list PSTX PSTY) LO))
     )
     (progn
      (setq BULGE (atof INLINE))
     )
    )
    (setq INLINE (cdr (assoc 1 ENTLIST)))
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
    (setq RFL:ALIGNLIST (append RFL:ALIGNLIST (list (list STA (list P1X P1Y) (list P2X P2Y) BULGE))))
   )
  )
 )
)
;
;
;   Program written by Robert Livingston, 99/12/03
;
;   (RFL:XYP) returns the X,Y point for a station and offset of the currently defined alignment
;
;
(defun RFL:XYP (/ ACCEPTXYP AL ANG CANCEL CANCELXYP DCL_ID FIXE FIXN FIXSTA FIXOS
                  FIXFROMSTA FIXFROMOS FIXTOSTA FIXTOOS
                  FIXSTEP FIXXFROMSTA FIXXTOSTA FIXXSWATH FIXXINC INC INFILE INLINE
                  NODE P P1 P2 RERUN STA STA1 STA2 STAMIN STAMAX
                  TMP UPDATENE UPDATESTAOFF)

 (defun ACCEPTXYP (TMP)
  (setq CANCEL TMP)
  (setq XYPSTA (atof (get_tile "STATION")))
  (setq XYPOS (atof (get_tile "OFFSET")))
  (setq XYPFROMSTA (atof (get_tile "FROMSTATION")))
  (setq XYPFROMOS (atof (get_tile "FROMOFFSET")))
  (setq XYPTOSTA (atof (get_tile "TOSTATION")))
  (setq XYPTOOS (atof (get_tile "TOOFFSET")))
  (setq XYPSTEP (atoi (get_tile "STEP")))
  (setq XYPXFROMSTA (atof (get_tile "XFROMSTATION")))
  (setq XYPXTOSTA (atof (get_tile "XTOSTATION")))
  (setq XYPXROUND (get_tile "XROUND"))
  (setq XYPXSWATH (atof (get_tile "XSWATH")))
  (setq XYPXINC (atof (get_tile "XINC")))
  (setq XYPXSPECIAL (get_tile "XSPECIAL"))
  (done_dialog)
 )

 (defun CANCELXYP ()
  (setq CANCEL 1)
  (done_dialog)
 )

 (defun FIXSTA (/ TMP)
  (setq TMP (atof (get_tile "STATION")))
  (if (< TMP STAMIN) (setq TMP STAMIN))
  (if (> TMP STAMAX) (setq TMP STAMAX))
  (set_tile "STATION" (rtos TMP))
  (UPDATENE)
 )

 (defun FIXOS ()
  (set_tile "OFFSET" (rtos (atof (get_tile "OFFSET"))))
  (UPDATENE)
 )

 (defun FIXN ()
  (set_tile "NORTHING" (rtos (atof (get_tile "NORTHING"))))
  (UPDATESTAOFF)
 )

 (defun FIXE ()
  (set_tile "EASTING" (rtos (atof (get_tile "EASTING"))))
  (UPDATESTAOFF)
 )

 (defun FIXFROMSTA (/ TMP)
  (setq TMP (atof (get_tile "FROMSTATION")))
  (if (< TMP STAMIN) (setq TMP STAMIN))
  (if (> TMP STAMAX) (setq TMP STAMAX))
  (set_tile "FROMSTATION" (rtos TMP))
 )

 (defun FIXFROMOS ()
  (set_tile "FROMOFFSET" (rtos (atof (get_tile "FROMOFFSET"))))
 )

 (defun FIXTOSTA (/ TMP)
  (setq TMP (atof (get_tile "TOSTATION")))
  (if (< TMP STAMIN) (setq TMP STAMIN))
  (if (> TMP STAMAX) (setq TMP STAMAX))
  (set_tile "TOSTATION" (rtos TMP))
 )

 (defun FIXTOOS ()
  (set_tile "TOOFFSET" (rtos (atof (get_tile "TOOFFSET"))))
 )

 (defun FIXSTEP (/ TMP)
  (setq TMP (atoi (get_tile "STEP")))
  (if (< TMP 1) (setq TMP 1))
  (set_tile "STEP" (itoa TMP))
 )

 (defun FIXXFROMSTA (/ TMP)
  (setq TMP (atof (get_tile "XFROMSTATION")))
  (if (< TMP STAMIN) (setq TMP STAMIN))
  (if (> TMP STAMAX) (setq TMP STAMAX))
  (set_tile "XFROMSTATION" (rtos TMP))
 )

 (defun FIXXTOSTA (/ TMP)
  (setq TMP (atof (get_tile "XTOSTATION")))
  (if (< TMP STAMIN) (setq TMP STAMIN))
  (if (> TMP STAMAX) (setq TMP STAMAX))
  (set_tile "XTOSTATION" (rtos TMP))
 )

 (defun FIXXSWATH (/ TMP)
  (setq TMP (atof (get_tile "XSWATH")))
  (if (<= TMP 0.0) (setq TMP 0.0))
  (set_tile "XSWATH" (rtos TMP))
 )

 (defun FIXXINC (/ TMP)
  (setq TMP (atof (get_tile "XINC")))
  (if (< TMP 0.0) (setq TMP 10.0))
  (set_tile "XINC" (rtos TMP))
 )

 (defun UPDATENE (/ P)
  (setq P (list (atof (get_tile "STATION")) (atof (get_tile "OFFSET"))))
  (setq P (RFL:XY P))
  (if (/= P nil)
   (progn
    (set_tile "NORTHING" (rtos (nth 1 P)))
    (set_tile "EASTING" (rtos (nth 0 P)))
   )
  )
 )

 (defun UPDATESTAOFF (/ P)
  (setq P (list (atof (get_tile "EASTING")) (atof (get_tile "NORTHING"))))
  (setq P (RFL:STAOFF P))
  (if (/= P nil)
   (progn
    (set_tile "STATION" (rtos (nth 0 P)))
    (set_tile "OFFSET" (rtos (nth 1 P)))
   )
   (progn
    (UPDATENE)
   )
  )
 )

 (if (or (= RFL:ALIGNLIST nil) (= RFL:XY nil))
  (princ "\n*** No alignment defined or utilities not loaded ***")
  (progn
   (setq NODE (car RFL:ALIGNLIST))
   (setq STAMIN (car NODE))
   (setq NODE (last RFL:ALIGNLIST))
   (setq STAMAX (+ (car NODE) (RFL:DIST (nth 1 NODE) (nth 2 NODE) (nth 3 node))))

   (if (= XYPDCLNAME nil)
    (progn
     (setq XYPDCLNAME (vl-filename-mktemp "rfl.dcl"))
     (RFL:MAKEDCL XYPDCLNAME "XYP")
    )
    (if (= nil (findfile XYPDCLNAME))
     (progn
      (setq XYPDCLNAME (vl-filename-mktemp "rfl.dcl"))
      (RFL:MAKEDCL XYPDCLNAME "XYP")
     )
    )
   )
   
   (setq DCL_ID (load_dialog XYPDCLNAME))
   (if (not (new_dialog "XYP" DCL_ID)) (exit))

   (if (= nil XYPSTA) (setq XYPSTA STAMIN))
   (if (= nil XYPOS) (setq XYPOS 0.0))

   (if (= nil XYPFROMSTA) (setq XYPFROMSTA STAMIN))
   (if (= nil XYPFROMOS) (setq XYPFROMOS 0.0))
   (if (= nil XYPTOSTA) (setq XYPTOSTA STAMAX))
   (if (= nil XYPTOOS) (setq XYPTOOS 0.0))
   (if (= nil XYPSTEP) (setq XYPSTEP 1))
   (if (= nil XYPXFROMSTA) (setq XYPXFROMSTA STAMIN))
   (if (= nil XYPXTOSTA) (setq XYPXTOSTA STAMAX))
   (if (= nil XYPXROUND) (setq XYPXROUND "1"))
   (if (= nil XYPXSWATH) (setq XYPXSWATH 25.0))
   (if (= nil XYPXINC) (setq XYPXINC 10.0))
   (if (= nil XYPXSPECIAL) (setq XYPXSPECIAL "0"))

   (set_tile "STAMINMAX" (strcat (rtos STAMIN 2 3) " < STA < " (rtos STAMAX 2 3)))
   (set_tile "STATION" (rtos XYPSTA))
   (set_tile "OFFSET" (rtos XYPOS))

   (set_tile "FROMSTATION" (rtos XYPFROMSTA))
   (set_tile "FROMOFFSET" (rtos XYPFROMOS))
   (set_tile "TOSTATION" (rtos XYPTOSTA))
   (set_tile "TOOFFSET" (rtos XYPTOOS))
   (set_tile "STEP" (itoa XYPSTEP))

   (set_tile "XFROMSTATION" (rtos XYPXFROMSTA))
   (set_tile "XTOSTATION" (rtos XYPXTOSTA))
   (set_tile "XROUND" XYPXROUND)
   (set_tile "XSWATH" (rtos XYPXSWATH))
   (set_tile "XINC" (rtos XYPXINC))
   (set_tile "XSPECIAL" XYPXSPECIAL)

   (FIXSTA)
   (FIXOS)
   (FIXFROMSTA)
   (FIXFROMOS)
   (FIXTOSTA)
   (FIXTOOS)
   (FIXSTEP)
   (FIXXFROMSTA)
   (FIXXTOSTA)
   (FIXXSWATH)
   (FIXXINC)

   (action_tile "STATION" "(FIXSTA)")
   (action_tile "OFFSET" "(FIXOS)")
   (action_tile "NORTHING" "(FIXN)")
   (action_tile "EASTING" "(FIXE)")
   (action_tile "FROMSTATION" "(FIXFROMSTA)")
   (action_tile "FROMOFFSET" "(FIXFROMOS)")
   (action_tile "TOSTATION" "(FIXTOSTA)")
   (action_tile "TOOFFSET" "(FIXTOOS)")
   (action_tile "STEP" "(FIXSTEP)")
   (action_tile "XFROMSTATION" "(FIXXFROMSTA)")
   (action_tile "XTOSTATION" "(FIXXTOSTA)")
   (action_tile "XSWATH" "(FIXXSWATH)")
   (action_tile "XINC" "(FIXXINC)")
   (action_tile "OK" "(ACCEPTXYP 0)")
   (action_tile "DRAW" "(ACCEPTXYP -1)")
   (action_tile "XDRAW" "(ACCEPTXYP -2)")
   (action_tile "FROMFILE" "(ACCEPTXYP -3)")
   (action_tile "PICK" "(ACCEPTXYP \"RERUN1\")")
   (action_tile "MPICK" "(ACCEPTXYP \"RERUN2\")")
   (action_tile "XPICK" "(ACCEPTXYP \"RERUN3\")")
   (action_tile "CANCEL" "(CANCELXYP)")

   (start_dialog)

   (if (= CANCEL 0)
    (progn
     (unload_dialog DCL_ID)
     (setq P (RFL:XY (list XYPSTA XYPOS)))
     (if (/= P nil)
      (command "_NON" P)
     )
    )
   )
   (if (= CANCEL -1)
    (progn
     (unload_dialog DCL_ID)
     (RFL:MPOINT2 XYPFROMSTA XYPFROMOS XYPTOSTA XYPTOOS XYPSTEP)
    )
   )
   (if (= CANCEL -2)
    (progn
     (unload_dialog DCL_ID)
     (command)
     (command)
     (setq INC XYPXINC)
     (if (> INC 0.0)
      (progn
       (if (< XYPXFOMSTA XYPXTOSTA)
        (setq STA1 XYPXFROMSTA STA2 XYPXTOSTA)
        (setq STA1 XYPXTOSTA STA2 XYPXFROMSTA)
       )
       (if (= XYPXROUND "1")
        (setq STA (float (* INC (fix (+ 0.99999999 (/ STA1 INC))))))
        (setq STA STA1)
       )
       (while (<= STA STA2)
        (setq P1 (RFL:XY (list STA (* -0.5 XYPXSWATH))))
        (setq P2 (RFL:XY (list STA (* 0.5 XYPXSWATH))))
        (if (/= P1 nil)
         (progn
          ;(command "._LINE" "_NON" P1 "_NON" P2 "")
          (entmake)
          (if (= XYPXSWATH 0.0)
           (progn
            (setq P2 (RFL:XY (list STA 1.0)))
            (setq ANG (angle P1 P2))
            (entmake (list (cons 0 "XLINE")
                           (cons 100 "AcDbEntity")
                           (cons 100 "AcDbXline")
                           (append (list 10) P1 (list 0.0))
                           (list 11 (cos ANG) (sin ANG) 0.0)
                     )
            )
           )
           (entmake (list (cons 0 "LINE")
                          (append (list 10) P1 (list 0.0))
                          (append (list 11) P2 (list 0.0))
                    )
           )
          )
         )
        )
        (setq STA (+ STA INC))
       )
      )
     )
     (if (= XYPXSPECIAL "1")
      (progn
       (setq AL RFL:ALIGNLIST)
       (setq NODE (car AL))
       (setq AL (cdr AL))
       (while (/= NODE nil)
        (setq P1 (RFL:XY (list (car NODE) (* -0.5 XYPXSWATH))))
        (setq P2 (RFL:XY (list (car NODE) (* 0.5 XYPXSWATH))))
        (if (/= P1 nil)
         (progn
          (command "._LINE" "_NON" P1 "_NON" P2 "")
         )
        )
        (setq NODE (car AL))
        (setq AL (cdr AL))
       )
       (setq NODE (last RFL:ALIGNLIST))
       (setq P1 (RFL:XY (list (+ (car NODE) (RFL:DIST (nth 1 NODE) (nth 2 NODE) (nth 3 NODE))) (* -0.5 XYPXSWATH))))
       (setq P2 (RFL:XY (list (+ (car NODE) (RFL:DIST (nth 1 NODE) (nth 2 NODE) (nth 3 NODE))) (* 0.5 XYPXSWATH))))
       (if (/= P1 nil)
        (progn
         (command "._LINE" "_NON" P1 "_NON" P2 "")
        )
       )
      )
     )
    )
   )
   (if (= CANCEL -3)
    (progn
     (unload_dialog DCL_ID)
     (setq INFILE (getfiled "Select a Sta,O/S comma delimited file" "" "" 2))
     (if (/= INFILE nil)
      (progn
       (setq INFILE (open INFILE "r"))
       (setq INLINE (read-line INFILE))
       (while (/= INLINE nil)
        (if (= INLINE "")
         (command "")
         (progn
          (setq P (RFL:XY (list (atof (RFL:COLUMN INLINE 1 ","))
                            (atof (RFL:COLUMN INLINE 2 ",")))))
          (if (/= P nil) (command "_NON" P))
         )
        )
        (setq INLINE (read-line INFILE))
       )
       (close INFILE)
      )
     )
    )
   )
   (if (= CANCEL "RERUN1")
    (progn
     (unload_dialog DCL_ID)
     (setq TMP (RFL:STAOFF (getpoint "\n\n\nPoint :")))
     (if (/= TMP nil)
      (progn
       (setq XYPSTA (nth 0 TMP))
       (setq XYPOS (nth 1 TMP))
      )
     )
     (RFL:XYP)
    )
   )
   (if (= CANCEL "RERUN2")
    (progn
     (unload_dialog DCL_ID)
     (setq TMP (RFL:STAOFF (getpoint "\n\n\nFirst point :")))
     (if (/= TMP nil)
      (progn
       (setq XYPFROMSTA (nth 0 TMP))
       (setq XYPFROMOS (nth 1 TMP))
      )
     )
     (setq TMP (RFL:STAOFF (getpoint "\nSecond point :")))
     (if (/= TMP nil)
      (progn
       (setq XYPTOSTA (nth 0 TMP))
       (setq XYPTOOS (nth 1 TMP))
      )
     )
     (if (> XYPFROMSTA XYPTOSTA)
      (progn
       (setq TMP XYPFROMSTA)
       (setq XYPFROMSTA XYPTOSTA)
       (setq XYPTOSTA TMP)
       (setq TMP XYPFROMOS)
       (setq XYPFROMOS XYPTOOS)
       (setq XYPTOOS TMP)
      )
     )
     (RFL:XYP)
    )
   )
   (if (= CANCEL "RERUN3")
    (progn
     (unload_dialog DCL_ID)
     (setq TMP (RFL:STAOFF (getpoint "\n\n\nFirst point :")))
     (if (/= TMP nil) (setq XYPXFROMSTA (car TMP)))
     (setq TMP (RFL:STAOFF (getpoint "\nSecond point :")))
     (if (/= TMP nil) (setq XYPXTOSTA (car TMP)))
     (if (> XYPXFROMSTA XYPXTOSTA)
      (progn
       (setq TMP XYPXFROMSTA)
       (setq XYPXFROMSTA XYPXTOSTA)
       (setq XYPXTOSTA TMP)
      )
     )
     (RFL:XYP)
    )
   )
  )
 )
);
;
;   Program written by Robert Livingston, 99/12/03
;
;   RFL:DIST returns the length of an alignment entity
;
;
(defun RFL:DIST (P1 P2 BULGE / ATOTAL CHORD R)
 (if (listp BULGE)
  (progn
   (- (RFL:GETSPIRALLS2 (nth 0 BULGE) (nth 1 BULGE) (nth 2 BULGE)) (nth 3 BULGE))
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
;
;
;   Program written by Robert Livingston, 99/09/10
;
;   RFL:MPOINT is a routine that returns a set of points from one station/offset to another station/offset
;          (note - ALIGN must be loaded and an alignment must be set.  Also recommended to turn off your osnaps)
;
;
(defun RFL:MPOINT2 (STATION1 OFFSET1 STATION2 OFFSET2 CMAX / C OFFSETINC P STATIONINC)
 (if (and (/= RFL:ALIGNLIST nil) (/= RFL:XY nil))
  (progn
   (setq C 0)
   (while (< C (+ CMAX 1))
    (setq P (RFL:XY (list (+ STATION1 (* (- STATION2 STATION1) (/ (float C) (float CMAX))))
                          (+ OFFSET1 (* (- OFFSET2 OFFSET1) (/ (float C) (float CMAX))))
                    )
            )
    )
    (print P)
    (command "_NON" P)
    (setq C (+ C 1))
   )
  )
 )
)
(defun RFL:MPOINT (/ C CMAX CMDECHO OFFSET1 OFFSET2 STATION1 STATION2)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (if (and (/= RFL:ALIGNLIST nil) (/= RFL:XY nil))
  (progn
   (setq STATION1 (getreal "\nEnter start station : "))
   (setq OFFSET1 (getreal "\nEnter start offset : "))
   (setq STATION2 (getreal "\nEnter end station : "))
   (setq OFFSET2 (getreal "\nEnter end offset : "))
   (setq CMAX (getint "\nEnter number of steps : "))
   (RFL:MPOINT2 STATION1 OFFSET1 STATION2 OFFSET2 CMAX)
  )
 )
 (setvar "CMDECHO" CMDECHO)
)
;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:SPIRALFYR returns (R *  Spiral 'Y') for a given deflection
;
;
(if RFL:SPIRALFYR (princ "\nRFL:SPIRALFYR already loaded...")
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
)
;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:SPIRALFXR returns (R *  Spiral 'X') for a given deflection
;
;
(if RFL:SPIRALFXR (princ "\nRFL:SPIRALFXR already loaded...")
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
)

;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:SPIRALP returns the spiral 'P' offset for a given length and radius
;
;
(if RFL:SPIRALP (princ "\nRFL:SPIRALP already loaded...")
(defun RFL:SPIRALP (R LS / THETA)
 (setq THETA (/ LS R 2.0))
 (* R (- (RFL:SPIRALFYR THETA) (- 1.0 (cos THETA))))
)
)
;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:SPIRALPR returns (R * spiral 'P') for a given deflection
;
;
(if RFL:SPIRALPR (princ "\nRFL:SPIRALPR already loaded...")
(defun RFL:SPIRALPR (THETA)
 (- (RFL:SPIRALFYR THETA) (- 1.0 (cos THETA)))
)
)
;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:SPIRALK returns the spiral 'K' value for a given radius and length
;
;
(if RFL:SPIRALK (princ "\nRFL:SPIRALK already loaded...")
(defun RFL:SPIRALK (R LS / THETA)
 (setq THETA (/ LS R 2.0))
 (* R (- (SPIRALFXR THETA) (sin THETA)))
)
)
;
;
;    Program Written by Robert Livingston, 99/07/14
;    RFL:SPIRALKR returns the spiral 'K' value for a given deflection
;
;
(if RFL:SPIRALKR (princ "\nRFL:SPIRALKR already loaded...")
(defun RFL:SPIRALKR (THETA)
 (- (RFL:SPIRALFXR THETA) (sin THETA))
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
;   RFL:PROFDEF locates and defines a global variable RFL:PROFDEFLIST with the profile base point, stationing and elevations
;
;
(defun RFL:PROFDEF (/ BPOINT DIRECTION ELEV ELEVMAX ENT ENTLIST FNAME OBPROFILE PLAYER PTLAYER
                      SCALE STA STAH STAL STAMAX TMP VEXAG X1 X2 Y1 Y2)
 (setq RFL:PROFDEFLIST nil
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
  (if (= "AECC_PROFILE_VIEW" (cdr (assoc 0 ENTLIST)))
   (progn
    (setq OBPROFILE (vlax-ename->vla-object ENT))
    (setq ELEV (vlax-get OBPROFILE "ElevationMin"))
    (setq ELEVMAX (vlax-get OBPROFILE "ElevationMax"))
    (setq STA (vlax-get OBPROFILE "StationStart"))
    (setq STAMAX (vlax-get OBPROFILE "StationEnd"))
    (vlax-invoke-method OBPROFILE 'FindXYAtStationAndElevation STA ELEV 'X1 'Y1 'inside)
    (vlax-invoke-method OBPROFILE 'FindXYAtStationAndElevation STAMAX ELEVMAX 'X2 'Y2 'inside)
    (if (< X1 X2)
     (setq BPOINT (list X1 Y1)
           DIRECTION 1
     )
     (setq BPOINT (list X2 Y1)
           DIRECTION -1
           STA (vlax-get OBPROFILE "StationEnd")
           STAMAX (vlax-get OBPROFILE "StationStart")
     )
    )
    (setq VEXAG (/ (- Y2 Y1) (- ELEVMAX ELEV)))
   )
   (if (= "AECC_PROFILE" (cdr (assoc 0 ENTLIST)))
    (progn
     (setq OBPROFILE (vlax-ename->vla-object ENT))
     (setq ELEV (vlax-get OBPROFILE "ElevationMin"))
     (setq ELEVMAX (vlax-get OBPROFILE "ElevationMax"))
     (setq STA (vlax-get OBPROFILE "StartingStation"))
     (setq STAMAX (vlax-get OBPROFILE "StationEnd"))
     (vlax-invoke-method OBPROFILE 'FindXYAtStationAndElevation STA ELEV 'X1 'Y1 'inside)
     (vlax-invoke-method OBPROFILE 'FindXYAtStationAndElevation STAMAX ELEVMAX 'X2 'Y2 'inside)
     (if (< X1 X2)
      (setq BPOINT (list X1 Y1)
            DIRECTION 1
      )
      (setq BPOINT (list X2 Y1)
            DIRECTION -1
            STA (vlax-get OBPROFILE "StationEnd")
            STAMAX (vlax-get OBPROFILE "StationStart")
      )
     )
     (setq VEXAG (/ (- Y2 Y1) (- ELEVMAX ELEV)))
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
  )
 )
 (if (and BPOINT DIRECTION ELEV FNAME PLAYER PTLAYER SCALE STA VEXAG)
  (setq RFL:PROFDEFLIST (list (cons "BPOINT" BPOINT)
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
;   RFL:ELEVATION returns the elevation at a specified station for the curretnly defined profile (RFL:PVILIST)
;
;
(if RFL:ELEVATION (princ "\nRFL:ELEVATION already loaded...")
(defun RFL:ELEVATION (STA / C CMDECHO ELEV ELEV1 ELEV2 ELEV3 G1 G2 L NODE P STA1 STA2 STA3)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (if (/= nil RFL:PVILIST)
  (progn
   (if (or (< STA (caar RFL:PVILIST)) (> STA (car (last RFL:PVILIST))))
    (progn
     (princ "\n*** STATION OUT OF RANGE ***\n")
     (setq ELEV nil)
    )
    (progn
     (setq C 1)
     (while (> STA (+ (car (setq NODE (nth C RFL:PVILIST)))
                      (/ (if (= nil (cadddr NODE)) 0.0 (cadddr NODE)) 2.0)
                   )
            )
      (setq C (1+ C))
     )
     (if (or (= "L" (caddr (nth C RFL:PVILIST))) (= nil (caddr (nth C RFL:PVILIST))))
      (progn
       (setq NODE (nth (1- C) RFL:PVILIST))
       (setq STA1 (car NODE))
       (setq ELEV1 (cadr NODE))
       (setq NODE (nth C RFL:PVILIST))
       (setq STA2 (car NODE))
       (setq ELEV2 (cadr NODE))
       (setq L (if (= nil (cadddr NODE)) 0.0 (cadddr NODE)))
       (setq G1 (/ (- ELEV2 ELEV1) (- STA2 STA1)))
       (setq ELEV (+ ELEV1 (* G1 (- STA STA1))))
       (setq D (- STA (- STA2 (/ L 2.0))))
       (if (> D 0.0)
        (progn
         (setq NODE (nth (1+ C) RFL:PVILIST))
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
)
;
;
;   Program written by Robert Livingston, 98/05/14
;
;   RFL:SLOPE returns the slope at a specified station for the curretnly defined profile (RFL:PVILIST)
;
;
(if RFL:SLOPE (princ "\nRFL:SLOPE already loaded...")
(defun RFL:SLOPE (STA / C CMDECHO ELEV1 ELEV2 ELEV3 G G1 G2 L NODE P)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (if (/= nil RFL:PVILIST)
  (progn
   (if (or (< STA (caar RFL:PVILIST)) (> STA (car (last RFL:PVILIST))))
    (progn
     (princ "\n*** STATION OUT OF RANGE ***\n")
     (if (< STA (caar RFL:PVILIST))
      (setq G (/ (- (cadadr RFL:PVILIST) (cadar RFL:PVILIST)) (- (caadr RFL:PVILIST) (caar RFL:PVILIST))))
      (setq G (/ (- (cadadr (reverse RFL:PVILIST)) (cadar (reverse RFL:PVILIST))) (- (caadr (reverse RFL:PVILIST)) (caar (reverse RFL:PVILIST)))))
     )
    )
    (progn
     (setq C 0)
     (while (> STA (+ (car (setq NODE (nth C RFL:PVILIST)))
                      (/ (cadddr NODE) 2.0)
                   )
            )
      (setq C (1+ C))
     )
     (if (= "L" (caddr (nth C RFL:PVILIST)))
      (progn
       (setq NODE (nth (1- C) RFL:PVILIST))
       (setq STA1 (car NODE))
       (setq ELEV1 (cadr NODE))
       (setq NODE (nth C RFL:PVILIST))
       (setq STA2 (car NODE))
       (setq ELEV2 (cadr NODE))
       (setq L (cadddr NODE))
       (setq G1 (/ (- ELEV2 ELEV1) (- STA2 STA1)))
       (setq G G1)
       (setq D (- STA (- STA2 (/ L 2.0))))
       (if (> D 0.0)
        (progn
         (setq NODE (nth (1+ C) RFL:PVILIST))
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
)
;
;
;   Program written by Robert Livingston, 98/05/14
;
;   RFL:PROFPOINT returns the point at a specified station and elevation for the curretnly defined profile grid RFL:PROFDEFLIST
;
;
(defun RFL:PROFPOINT (STA ELEV / D X Y)
 (if (/= nil RFL:PROFDEFLIST)
  (progn
   (if (= (assoc "DIRECTION" RFL:PROFDEFLIST) nil)
    (setq D 1)
    (setq D (cdr (assoc "DIRECTION" RFL:PROFDEFLIST)))
   )
   (setq X (+ (* (- STA
                    (cdr (assoc "STA" RFL:PROFDEFLIST))
                 )
                 D
              )
              (car (cdr (assoc "BPOINT" RFL:PROFDEFLIST)))
           )
   )
   (setq Y (+ (* (- ELEV
                    (cdr (assoc "ELEV" RFL:PROFDEFLIST))
                 )
                 (cdr (assoc "VEXAG" RFL:PROFDEFLIST))
              )
              (cadr (cdr (assoc "BPOINT" RFL:PROFDEFLIST)))
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
 (if (not (tblsearch "LAYER" (cdr (assoc "PTLAYER" RFL:PROFDEFLIST))))
  (entmake (list (cons 0 "LAYER")
                 (cons 100 "AcDbSymbolTableRecord")
                 (cons 100 "AcDbLayerTableRecord")
                 (cons 2 (cdr (assoc "PTLAYER" RFL:PROFDEFLIST)))
                 (cons 70 0)
           )
  )
 )
 (setvar "CLAYER" (cdr (assoc "PTLAYER" RFL:PROFDEFLIST)))

 (setq PVI RFL:PVILIST)
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
;   RFL:DRAWPROF draws the current profile as defined in RFL:PVILIST
;
;
(defun RFL:DRAWPROF ( RFL:PVILIST / ACTIVEDOC ACTIVESPACE ANG BULGE C C2 CLAYER ENT ENTLIST G G1 G2 K
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

 (if (= nil RFL:PROFDEFLIST)
  (princ "\n*** Profile not set ***")
  (progn
   (if (= (tblsearch "BLOCK" "PVI2") nil)
    (progn
     (princ "\n*** Creating LDD PVI node ***")
     (RFL:MAKEENT "PVI2")
    )
   )
     (setq C 0)
     (if (not (tblsearch "LAYER" (cdr (assoc "PLAYER" RFL:PROFDEFLIST))))
      (entmake (list (cons 0 "LAYER")
                     (cons 100 "AcDbSymbolTableRecord")
                     (cons 100 "AcDbLayerTableRecord")
                     (cons 2 (cdr (assoc "PLAYER" RFL:PROFDEFLIST)))
                     (cons 70 0)
               )
      )
     )
     (setvar "CLAYER" (cdr (assoc "PLAYER" RFL:PROFDEFLIST)))
     (if (= nil (tblsearch "BLOCK" "PVI2")) (RFL:MAKEENT "PVI2"))
     (while (< C (length RFL:PVILIST))
      (vla-insertblock ACTIVESPC
                       (vlax-3D-point (RFL:PROFPOINT (nth 0 (nth C RFL:PVILIST)) (nth 1 (nth C RFL:PVILIST))))
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
        (if (or (= C 0) (= C (- (length RFL:PVILIST) 1)))
         (progn
          (setq L 0.0 K 0.0)
         )
         (progn
          (setq L (nth 3 (nth C RFL:PVILIST)))
          (setq G1 (/ (- (nth 1 (nth C RFL:PVILIST))
                         (nth 1 (nth (- C 1) RFL:PVILIST))
                      )
                      (- (nth 0 (nth C RFL:PVILIST))
                         (nth 0 (nth (- C 1) RFL:PVILIST))
                      )
                   )
          )
          (setq G2 (/ (- (nth 1 (nth (+ C 1) RFL:PVILIST))
                         (nth 1 (nth C RFL:PVILIST))
                      )
                      (- (nth 0 (nth (+ C 1) RFL:PVILIST))
                         (nth 0 (nth C RFL:PVILIST))
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
   (setq G1 (/ (- (nth 1 (nth C RFL:PVILIST))
                  (nth 1 (nth (- C 1) RFL:PVILIST))
               )
               (- (nth 0 (nth C RFL:PVILIST))
                  (nth 0 (nth (- C 1) RFL:PVILIST))
               )
            )
   )
   (if (= (+ C 1) (length RFL:PVILIST))
    (setq G2 0)
    (setq G2 (/ (- (nth 1 (nth (+ C 1) RFL:PVILIST))
                   (nth 1 (nth C RFL:PVILIST))
                )
                (- (nth 0 (nth (+ C 1) RFL:PVILIST))
                   (nth 0 (nth C RFL:PVILIST))
                )
             )
    )
   )
   (setq G (- G2 G1))
   (setq ANG (- (atan G2) (atan G1)))
   (if (> G 0) (setq SIGN 1.0) (setq SIGN -1.0))
   (if (= (nth 2 (nth C RFL:PVILIST)) "L")
    (progn
     (setq L3 (/ (nth 3 (nth C RFL:PVILIST)) 2.0))
     (setq L4 (/ (nth 3 (nth C RFL:PVILIST)) 2.0))
    )
    (progn
     (setq TMP (* (nth 3 (nth C RFL:PVILIST))
                (RFL:TAN (/ (abs ANG) 2.0))
             )
     )
     (setq L3 (* TMP (cos (atan (abs G1)))))
     (setq L4 (* TMP (cos (atan (abs G2)))))
    )
   )
   (setq STA1 (nth 0 (nth (- C 1) RFL:PVILIST)))
   (setq STA2 (- (nth 0 (nth C RFL:PVILIST)) L3))
   (command "._LINE"
            (RFL:PROFPOINT STA1 (RFL:ELEVATION STA1))
            (RFL:PROFPOINT STA2 (RFL:ELEVATION STA2))
            ""
   )
   (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
   (while (< C (- (length RFL:PVILIST) 1))
    (setq C (+ C 1))
    (setq G1 G2)
    (if (= (+ C 1) (length RFL:PVILIST))
     (setq G2 0)
     (setq G2 (/ (- (nth 1 (nth (+ C 1) RFL:PVILIST))
                    (nth 1 (nth C RFL:PVILIST))
                 )
                 (- (nth 0 (nth (+ C 1) RFL:PVILIST))
                    (nth 0 (nth C RFL:PVILIST))
                 )
              )
     )
    )
    (setq G (- G2 G1))
    (setq ANG (- (atan G2) (atan G1)))
    (if (> G 0) (setq SIGN 1.0) (setq SIGN -1.0))
    (if (= (nth 2 (nth C RFL:PVILIST)) "L")
     (progn
      (setq L1 L3)
      (setq L2 L4)
      (setq L3 (/ (nth 3 (nth C RFL:PVILIST)) 2.0))
      (setq L4 (/ (nth 3 (nth C RFL:PVILIST)) 2.0))
     )
     (progn
      (setq L1 L3)
      (setq L2 L4)
      (setq TMP (* (nth 3 (nth C RFL:PVILIST))
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
      (setq STA1 (- (nth 0 (nth (- C 1) RFL:PVILIST)) L1))
      (setq STA2 (+ (nth 0 (nth (- C 1) RFL:PVILIST)) L2))
      (RFL:DRAWPARABOLICVCURVE (RFL:PROFPOINT STA1 (RFL:ELEVATION STA1))
                               (RFL:PROFPOINT (nth 0 (nth (- C 1) RFL:PVILIST)) (nth 1 (nth (- C 1) RFL:PVILIST)))
                               (RFL:PROFPOINT STA2 (RFL:ELEVATION STA2))
      )
      (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
     )
    )
    (setq STA1 (+ (nth 0 (nth (- C 1) RFL:PVILIST)) L2))
    (setq STA2 (- (nth 0 (nth C RFL:PVILIST)) L3))
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
)
;
;
;   Program written by Robert Livingston, 98/05/13
;
;   RPROF reads a vertical alignment from file INFILENAME and sets the global variable RFL:PVILIST
;
;
(defun RFL:RPROF (INFILENAME / INFILE INLINE PVIENT PVISET STA ELEV LR VAL)
 (if (/= INFILENAME nil) (setq INFILENAME (findfile INFILENAME)))
 (if (/= INFILENAME nil)
  (progn
   (vl-registry-write "HKEY_CURRENT_USER\\rflAlignDirectory" "" (strcat (vl-filename-directory INFILENAME) "\\"))
   (setq INFILE (open INFILENAME "r"))
   (setq RFL:PVILIST nil)
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
      (setq RFL:PVILIST (append RFL:PVILIST (list (list STA ELEV LR VAL))))
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
;   RFL:RPROFOG reads a vertical alignment from file INFILENAME and sets the global variable RFL:OGLIST
;
;
(defun RFL:RPROFOG (INFILENAME / INFILE INLINE PVIENT PVISET STA ELEV LR VAL)
 (if (/= INFILENAME nil) (setq INFILENAME (findfile INFILENAME)))
 (if (/= INFILENAME nil)
  (progn
   (setq INFILE (open INFILENAME "r"))
   (setq RFL:OGLIST nil)
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
      (setq RFL:OGLIST (append RFL:OGLIST (list (list STA ELEV))))
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
     (while (< C (length RFL:PVILIST))
      (princ (rtos (nth 0 (nth C RFL:PVILIST)) 2 16) OUTFILE)
      (princ "\n" OUTFILE)
      (princ (rtos (nth 1 (nth C RFL:PVILIST)) 2 16) OUTFILE)
      (princ "\n" OUTFILE)
      (princ (nth 2 (nth C RFL:PVILIST)) OUTFILE)
      (princ "\n" OUTFILE)
      (princ (rtos (nth 3 (nth C RFL:PVILIST)) 2 16) OUTFILE)
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
     (while (< C (length RFL:OGLIST))
      (princ (rtos (nth 0 (nth C RFL:OGLIST)) 2 16) OUTFILE)
      (princ "\n" OUTFILE)
      (princ (rtos (nth 1 (nth C RFL:OGLIST)) 2 16) OUTFILE)
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
;   Program written by Robert Livingston, 2008-11-04
;
;   RFL:RPROFB reads a vertical profile from a RFLAlign Block
;
;
(defun RFL:RPROFB (BLKENT / ELEV ENT ENTLIST INLINE LR STA VAL)
 (setq RFL:PVILIST nil)
 (setq ENT (entnext BLKENT))
 (setq ENTLIST (entget ENT))
 (while (/= "VRT" (cdr (assoc 2 ENTLIST)))
  (setq ENT (entnext ENT))
  (setq ENTLIST (entget ENT))
 )
 (setq INLINE (cdr (assoc 1 ENTLIST)))
 (setq ENT (entnext ENT))
 (setq ENTLIST (entget ENT))
 (if (/= INLINE "#RFL VERTICAL ALIGNMENT FILE")
  (progn
   (princ "\n*** FILE NOT FORMATTED CORRECTLY ***\n")
  )
  (progn
   (setq INLINE (cdr (assoc 1 ENTLIST)))
   (setq ENT (entnext ENT))
   (setq ENTLIST (entget ENT))
   (while (and (/= nil INLINE) (/= INLINE "#END DEFINITION"))
    (setq STA (atof INLINE))
    (setq INLINE (cdr (assoc 1 ENTLIST)))
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
    (setq ELEV (atof INLINE))
    (setq INLINE (cdr (assoc 1 ENTLIST)))
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
    (setq LR INLINE)
    (setq INLINE (cdr (assoc 1 ENTLIST)))
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
    (setq VAL (atof INLINE))
    (setq INLINE (cdr (assoc 1 ENTLIST)))
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
    (setq RFL:PVILIST (append RFL:PVILIST (list (list STA ELEV LR VAL))))
   )
  )
 )
)
;
;
;   Program written by Robert Livingston, 2008-11-04
;
;   RFL:RPROFOGB reads a vertical OG profile from a RFLAlign Block
;
;
(defun RFL:RPROFOGB (BLKENT / ELEV ENT ENTLIST INLINE LR STA VAL)
 (setq RFL:OGLIST nil)
 (setq ENT (entnext BLKENT))
 (setq ENTLIST (entget ENT))
 (while (/= "OG" (cdr (assoc 2 ENTLIST)))
  (setq ENT (entnext ENT))
  (setq ENTLIST (entget ENT))
 )
 (setq INLINE (cdr (assoc 1 ENTLIST)))
 (setq ENT (entnext ENT))
 (setq ENTLIST (entget ENT))
 (if (/= INLINE "#RFL VERTICAL ALIGNMENT FILE")
  (progn
   (princ "\n*** FILE NOT FORMATTED CORRECTLY ***\n")
  )
  (progn
   (setq INLINE (cdr (assoc 1 ENTLIST)))
   (setq ENT (entnext ENT))
   (setq ENTLIST (entget ENT))
   (while (and (/= nil INLINE) (/= INLINE "#END DEFINITION"))
    (setq STA (atof INLINE))
    (setq INLINE (cdr (assoc 1 ENTLIST)))
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
    (setq ELEV (atof INLINE))
    (setq INLINE (cdr (assoc 1 ENTLIST)))
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
    (setq LR INLINE)
    (setq INLINE (cdr (assoc 1 ENTLIST)))
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
    (setq VAL (atof INLINE))
    (setq INLINE (cdr (assoc 1 ENTLIST)))
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
    (setq RFL:OGLIST (append RFL:OGLIST (list (list STA ELEV))))
   )
  )
 )
)
(defun RFL:WALIGNB (BLKENT / BLKENTNEW BLKENTLIST C ENT ENTLIST ENTN)
 (entmake)
 (setq BLKENTLIST (entget BLKENT))
 (setq BLKENTNEW (entmake BLKENTLIST))
 (setq ENT (entnext BLKENT))
 (setq ENTLIST (entget ENT))
 (while (/= "SEQEND" (cdr (assoc 0 ENTLIST)))
  (if (= "HOR" (cdr (assoc 2 ENTLIST)))
   (progn
    (setq ENTLIST (subst (cons 1 "#RFL HORIZONTAL ALIGNMENT FILE") (assoc 1 ENTLIST) ENTLIST))
    (entmake ENTLIST)
    (setq C 0)
    (while (< C (length RFL:ALIGNLIST))
     (setq ENTLIST (subst (cons 70 1) (assoc 70 ENTLIST) ENTLIST))
     (setq ENTLIST (subst (cons 1 (rtos (nth 0 (nth C RFL:ALIGNLIST)) 2 16)) (assoc 1 ENTLIST) ENTLIST))
     (entmake ENTLIST)
     (setq ENTLIST (subst (cons 1 (rtos (nth 0 (nth 1 (nth C RFL:ALIGNLIST))) 2 16)) (assoc 1 ENTLIST) ENTLIST))
     (entmake ENTLIST)
     (setq ENTLIST (subst (cons 1 (rtos (nth 1 (nth 1 (nth C RFL:ALIGNLIST))) 2 16)) (assoc 1 ENTLIST) ENTLIST))
     (entmake ENTLIST)
     (setq ENTLIST (subst (cons 1 (rtos (nth 0 (nth 2 (nth C RFL:ALIGNLIST))) 2 16)) (assoc 1 ENTLIST) ENTLIST))
     (entmake ENTLIST)
     (setq ENTLIST (subst (cons 1 (rtos (nth 1 (nth 2 (nth C RFL:ALIGNLIST))) 2 16)) (assoc 1 ENTLIST) ENTLIST))
     (entmake ENTLIST)
     (if (listp (nth 3 (nth C RFL:ALIGNLIST)))
      (progn
       (setq ENTLIST (subst (cons 1 "SPIRAL") (assoc 1 ENTLIST) ENTLIST))
       (entmake ENTLIST)
       (setq ENTLIST (subst (cons 1 (rtos (nth 0 (nth 0 (nth 3 (nth C RFL:ALIGNLIST)))) 2 16)) (assoc 1 ENTLIST) ENTLIST))
       (entmake ENTLIST)
       (setq ENTLIST (subst (cons 1 (rtos (nth 1 (nth 0 (nth 3 (nth C RFL:ALIGNLIST)))) 2 16)) (assoc 1 ENTLIST) ENTLIST))
       (entmake ENTLIST)
       (setq ENTLIST (subst (cons 1 (rtos (nth 0 (nth 1 (nth 3 (nth C RFL:ALIGNLIST)))) 2 16)) (assoc 1 ENTLIST) ENTLIST))
       (entmake ENTLIST)
       (setq ENTLIST (subst (cons 1 (rtos (nth 1 (nth 1 (nth 3 (nth C RFL:ALIGNLIST)))) 2 16)) (assoc 1 ENTLIST) ENTLIST))
       (entmake ENTLIST)
       (setq ENTLIST (subst (cons 1 (rtos (nth 0 (nth 2 (nth 3 (nth C RFL:ALIGNLIST)))) 2 16)) (assoc 1 ENTLIST) ENTLIST))
       (entmake ENTLIST)
       (setq ENTLIST (subst (cons 1 (rtos (nth 1 (nth 2 (nth 3 (nth C RFL:ALIGNLIST)))) 2 16)) (assoc 1 ENTLIST) ENTLIST))
       (entmake ENTLIST)
       (setq ENTLIST (subst (cons 1 (rtos (nth 3 (nth 3 (nth C RFL:ALIGNLIST))) 2 16)) (assoc 1 ENTLIST) ENTLIST))
       (entmake ENTLIST)
      )
      (progn
       (setq ENTLIST (subst (cons 1 (rtos (nth 3 (nth C RFL:ALIGNLIST)) 2 16)) (assoc 1 ENTLIST) ENTLIST))
       (entmake ENTLIST)
      )
     )
     (setq C (+ C 1))
    )
    (setq ENTLIST (subst (cons 1 "#END DEFINITION") (assoc 1 ENTLIST) ENTLIST))
    (entmake ENTLIST)
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
    (while (= "HOR" (cdr (assoc 2 ENTLIST)))
     (setq ENT (entnext ENT))
     (setq ENTLIST (entget ENT))
    )
   )
   (progn
    (entmake ENTLIST)
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
   )
  )
 )
 (entmake ENTLIST)
 (entdel BLKENT)
 (setq BLKENTNEW (entlast))
)
;
;
;   Program written by Robert Livingston, 2008/11/04
;
;   RFL:WPROFB writes a vertical alinment to a RFLALIGN Block
;
;
(defun RFL:WPROFB (BLKENT / BLKENTNEW BLKENTLIST C ENT ENTLIST ENTN)
 (entmake)
 (setq BLKENTLIST (entget BLKENT))
 (setq BLKENTNEW (entmake BLKENTLIST))
 (setq ENT (entnext BLKENT))
 (setq ENTLIST (entget ENT))
 (while (/= "SEQEND" (cdr (assoc 0 ENTLIST)))
  (if (= "VRT" (cdr (assoc 2 ENTLIST)))
   (progn
    (setq ENTLIST (subst (cons 1 "#RFL VERTICAL ALIGNMENT FILE") (assoc 1 ENTLIST) ENTLIST))
    (entmake ENTLIST)
    (setq C 0)
    (while (< C (length RFL:PVILIST))
     (setq ENTLIST (subst (cons 70 1) (assoc 70 ENTLIST) ENTLIST))
     (setq ENTLIST (subst (cons 1 (rtos (nth 0 (nth C RFL:PVILIST)) 2 16)) (assoc 1 ENTLIST) ENTLIST))
     (entmake ENTLIST)
     (setq ENTLIST (subst (cons 1 (rtos (nth 1 (nth C RFL:PVILIST)) 2 16)) (assoc 1 ENTLIST) ENTLIST))
     (entmake ENTLIST)
     (setq ENTLIST (subst (cons 1 (nth 2 (nth C RFL:PVILIST))) (assoc 1 ENTLIST) ENTLIST))
     (entmake ENTLIST)
     (setq ENTLIST (subst (cons 1 (rtos (nth 3 (nth C RFL:PVILIST)) 2 16)) (assoc 1 ENTLIST) ENTLIST))
     (entmake ENTLIST)
     (setq C (+ C 1))
    )
    (setq ENTLIST (subst (cons 1 "#END DEFINITION") (assoc 1 ENTLIST) ENTLIST))
    (entmake ENTLIST)
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
    (while (= "VRT" (cdr (assoc 2 ENTLIST)))
     (setq ENT (entnext ENT))
     (setq ENTLIST (entget ENT))
    )
   )
   (progn
    (entmake ENTLIST)
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
   )
  )
 )
 (entmake ENTLIST)
 (entdel BLKENT)
 (setq BLKENTNEW (entlast))
)
;
;
;   Program written by Robert Livingston, 2008/11/04
;
;   RFL:WPROFOGB writes a OG vertical alinment to a RFLALIGN Block
;
;
(defun RFL:WPROFOGB (BLKENT / BLKENTNEW BLKENTLIST C ENT ENTLIST ENTN)
 (entmake)
 (setq BLKENTLIST (entget BLKENT))
 (setq BLKENTNEW (entmake BLKENTLIST))
 (setq ENT (entnext BLKENT))
 (setq ENTLIST (entget ENT))
 (while (/= "SEQEND" (cdr (assoc 0 ENTLIST)))
  (if (= "OG" (cdr (assoc 2 ENTLIST)))
   (progn
    (setq ENTLIST (subst (cons 1 "#RFL VERTICAL ALIGNMENT FILE") (assoc 1 ENTLIST) ENTLIST))
    (entmake ENTLIST)
    (setq C 0)
    (while (< C (length RFL:OGLIST))
     (setq ENTLIST (subst (cons 70 1) (assoc 70 ENTLIST) ENTLIST))
     (setq ENTLIST (subst (cons 1 (rtos (nth 0 (nth C RFL:OGLIST)) 2 16)) (assoc 1 ENTLIST) ENTLIST))
     (entmake ENTLIST)
     (setq ENTLIST (subst (cons 1 (rtos (nth 1 (nth C RFL:OGLIST)) 2 16)) (assoc 1 ENTLIST) ENTLIST))
     (entmake ENTLIST)
     (setq ENTLIST (subst (cons 1 "L") (assoc 1 ENTLIST) ENTLIST))
     (entmake ENTLIST)
     (setq ENTLIST (subst (cons 1 "0.0") (assoc 1 ENTLIST) ENTLIST))
     (entmake ENTLIST)
     (setq C (+ C 1))
    )
    (setq ENTLIST (subst (cons 1 "#END DEFINITION") (assoc 1 ENTLIST) ENTLIST))
    (entmake ENTLIST)
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
    (while (= "OG" (cdr (assoc 2 ENTLIST)))
     (setq ENT (entnext ENT))
     (setq ENTLIST (entget ENT))
    )
   )
   (progn
    (entmake ENTLIST)
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
   )
  )
 )
 (entmake ENTLIST)
 (entdel BLKENT)
 (setq BLKENTNEW (entlast))
)
;
;
;     Program written by Robert Livingston, 2016-07-19
;
;     RFL:RPROFC3D is a utility for reading a C3D profile and setting RFL:PVILIST
;     NOTE - Must be using C3D, will not work in straight AutoCAD
;     NOTE - Works for type 1 and type 3 vertical curves
;
;
(defun RFL:RPROFC3D (ENT / C CMAX CMDECHO ENDELEVATION ENDSTATION ENTITY ENTITYNEXT ENTLIST OBPROFILE OBENTITIES
                           PVISTATION PVIELEVATION PVILENGTH STARTELEVATION STARTSTATION TYPE)
 (if (= nil vlax-create-object) (vl-load-com))
 
 (defun GETPVISTATION ()
  (setq PVISTATION (vlax-get-property ENTITY "PVIStation"))
 )
 
 (setq ENTLIST (entget ENT))
 
 (if (/= "AECC_PROFILE" (cdr (assoc 0 ENTLIST)))
  (princ "\n*** Not a C3D Profile ***")
  (progn
   (setq OBPROFILE (vlax-ename->vla-object ENT))
   (setq STARTSTATION (vlax-get-property OBPROFILE "StartingStation"))
   (setq STARTELEVATION (vlax-invoke-method OBPROFILE "ElevationAt" STARTSTATION))
   (setq RFL:PVILIST (list (list STARTSTATION STARTELEVATION "L" 0.0)))
   (setq ENDSTATION (vlax-get-property OBPROFILE "EndingStation"))
   (setq ENDELEVATION (vlax-invoke-method OBPROFILE "ElevationAt" ENDSTATION))
   (setq OBENTITIES (vlax-get-property OBPROFILE "Entities"))
   (setq CMAX (vlax-get-property OBENTITIES "Count"))
   (setq C 0)
   (while (< C CMAX)
    (setq ENTITY (vlax-invoke-method OBENTITIES "Item" C))
    (if (= (+ C 1) CMAX) (setq ENTITYNEXT nil) (setq ENTITYNEXT (vlax-invoke-method OBENTITIES "Item" (+ C 1))))
    (cond
     ((= 1 (vlax-get-property ENTITY "Type"))
      (progn
       (if (/= ENTITYNEXT nil)
        (if (= (vlax-get-property ENTITYNEXT "Type") 1)
         (progn
          (setq PVISTATION (vlax-get-property ENTITY "EndStation"))
          (setq PVIELEVATION (vlax-get-property ENTITY "EndElevation"))
          (setq PVILENGTH 0.0)
          (setq RFL:PVILIST (append RFL:PVILIST (list (list PVISTATION PVIELEVATION "L" PVILENGTH))))
         )
        )
       )
      )
     )
     ((= 3 (vlax-get-property ENTITY "Type"))
      (progn
       (setq PVISTATION (vlax-get-property ENTITY "PVIStation"))
       (setq PVIELEVATION (vlax-get-property ENTITY "PVIElevation"))
       (setq PVILENGTH (vlax-get-property ENTITY "Length"))
       (setq RFL:PVILIST (append RFL:PVILIST (list (list PVISTATION PVIELEVATION "L" PVILENGTH))))
      )
     )
    )
    (setq C (1+ C))
   )
   (setq RFL:PVILIST (append RFL:PVILIST (list (list ENDSTATION ENDELEVATION "L" 0.0))))
  )
 )
 
);
;
;   Program written by Robert Livingston, 99/10/08
;
;   RFL:SUPERDEF calculating superelevations from supplied entity set and setting RFL:SUPERLIST
;
;
;(defun RFL:SUPERDEF (ENTSET / C ENT ENTLIST P PT SORTSUPER SUPERLEFT SUPERRIGHT SUPERLIST2)
(defun RFL:SUPERDEF (ENTSET)
 (defun SORTSUPER (SL / A B)
  (vl-sort SL '(lambda (A B) (< (car A) (car B))))
 )
 (setq RFL:SUPERLIST nil)
 (setq SUPERLIST2 nil)
 (if (and (/= RFL:ALIGNLIST nil) (/= RFL:STAOFF nil))
  (progn
   (setq C 0)
   (while (< C (sslength ENTSET))
    (setq ENT (ssname ENTSET C))
    (setq ENTLIST (entget ENT))
    (if (and (= (cdr (assoc 0 ENTLIST)) "INSERT")
             (= (strcase (cdr (assoc 2 ENTLIST))) "SUPER")
             (= (cdr (assoc 66 ENTLIST)) 1))
     (progn
      (setq PT (cdr (assoc 10 ENTLIST)))
      (setq P (RFL:STAOFF PT))
      ;
      ; The following are to 'nudge' the point - sometimes RFL:STAOFF returns nil when the point is an an entity endpoint
      ;
      (if (= nil P) (setq P (RFL:STAOFF (list (+ (car PT) 0.00000001) (+ (cadr PT) 0.00000001)))))
      (if (= nil P) (setq P (RFL:STAOFF (list (- (car PT) 0.00000001) (- (cadr PT) 0.00000001)))))
      (if (= nil P) (setq P (RFL:STAOFF (list (+ (car PT) 0.00000001) (- (cadr PT) 0.00000001)))))
      (if (= nil P) (setq P (RFL:STAOFF (list (- (car PT) 0.00000001) (+ (cadr PT) 0.00000001)))))
      (if (/= P nil)
       (progn
        (setq SUPERLEFT nil)
        (setq SUPERRIGHT nil)
        (setq ENT (entnext ENT))
        (setq ENTLIST (entget ENT))
        (while (/= (cdr (assoc 0 ENTLIST)) "SEQEND")
         (if (= (cdr (assoc 2 ENTLIST)) "LEFT") (setq SUPERLEFT (atof (cdr (assoc 1 ENTLIST)))))
         (if (= (cdr (assoc 2 ENTLIST)) "RIGHT") (setq SUPERRIGHT (atof (cdr (assoc 1 ENTLIST)))))
         (setq ENT (entnext ENT))
         (setq ENTLIST (entget ENT))
        )
        (setq SUPERLIST2 (append SUPERLIST2 (list (list (car P) SUPERLEFT SUPERRIGHT))))
       )
      )
     )
    )
    (setq C (+ C 1))
   )
  )
 )
 (setq RFL:SUPERLIST (SORTSUPER SUPERLIST2))
 (princ (strcat "\n" (itoa (length RFL:SUPERLIST)) " nodes found."))
 T
)
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
;
;
;   Program written by Robert Livingston, 99/10/08
;
;   RFL:DSUPER inserts SUPER blocks along the current alignment
;
;
(defun RFL:DSUPER (/ ACTIVEDOC ACTIVESPACE ANGBASE ANGDIR ATTREQ DIMZIN ENT ENTLIST NODE OSMODE P1 P2 PREVENT SL)
 (setq OSMODE (getvar "OSMODE"))
 (setvar "OSMODE" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 0)
 (setq DIMZIN (getvar "DIMZIN"))
 (setvar "DIMZIN" 8)
 (setq ATTREQ (getvar "ATTREQ"))

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

 (command "._UNDO" "M")

 (if (and (/= nil RFL:ALIGNLIST) (/= RFL:XY nil) (/= RFL:SUPERLIST nil))
  (progn
   (setq SL RFL:SUPERLIST)
   (while (/= SL nil)
    (setq NODE (car SL))
    (setq SL (cdr SL))
    (setq P1 (RFL:XY (list (nth 0 NODE) 0.0)))
    (if (/= P1 nil)
     (progn
      (setq P2 (RFL:XY (list (nth 0 NODE) 10.0)))
      (if (= nil (tblsearch "BLOCK" "SUPER")) (RFL:MAKEENT "SUPER"))
      (vla-insertblock ACTIVESPC
                       (vlax-3D-point P1)
                       "SUPER"
                       1.0
                       1.0
                       1.0
                       (- (angle P1 P2) (/ pi 2.0))
      )
      (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
      (setq ENT (entnext ENT))
      (setq ENTLIST (entget ENT))
      (while (/= (cdr (assoc 0 ENTLIST)) "SEQEND")
       (if (= (cdr (assoc 2 ENTLIST)) "LEFT")
        (progn
         (setq ENTLIST (subst (cons 1 (rtos (nth 1 NODE) 2 8)) (assoc 1 ENTLIST) ENTLIST))
         (entmod ENTLIST)
         (entupd ENT)
        )
       )
       (if (= (cdr (assoc 2 ENTLIST)) "RIGHT")
        (progn
         (setq ENTLIST (subst (cons 1 (rtos (nth 2 NODE) 2 8)) (assoc 1 ENTLIST) ENTLIST))
         (entmod ENTLIST)
         (entupd ENT)
        )
       )
       (setq ENT (entnext ENT))
       (setq ENTLIST (entget ENT))
      )
     )
    )
   )
  )
  (princ "\n*** SUPERELEVATION NOT SET ***\n")
 )

 (setvar "OSMODE" OSMODE)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
 (setvar "DIMZIN" DIMZIN)
 (setvar "ANGDIR" ATTREQ)
)
;
;
;   Program written by Robert Livingston, 99/10/08
;
;   RFL:WSUPER writes the superelevation to file
;
;
(defun RFL:WSUPER (OUTFILENAME / C OUTFILE)
 (if (/= OUTFILENAME nil)
  (progn
   (if (/= ".E" (strcase (substr OUTFILENAME (- (strlen OUTFILENAME) 1))))
    (setq OUTFILENAME (strcat OUTFILENAME ".e"))
   )
   (vl-registry-write "HKEY_CURRENT_USER\\rflAlignDirectory" "" (strcat (vl-filename-directory OUTFILENAME) "\\"))
   (setq OUTFILE (open OUTFILENAME "w"))
   (princ "#RFL SUPERELEVATION FILE\n" OUTFILE)
   (setq C 0)
   (while (< C (length SUPERLIST))
    (princ (rtos (nth 0 (nth C SUPERLIST)) 2 16) OUTFILE)
    (princ "\n" OUTFILE)
    (princ (rtos (nth 1 (nth C SUPERLIST)) 2 16) OUTFILE)
    (princ "\n" OUTFILE)
    (princ (rtos (nth 2 (nth C SUPERLIST)) 2 16) OUTFILE)
    (princ "\n" OUTFILE)
    (setq C (+ C 1))
   )
   (princ "#END DEFINITION\n" OUTFILE)
   (close OUTFILE)
   T
  )
  nil
 )
)
;
;
;   Program written by Robert Livingston, 99/10/08
;
;   RFL:RSUPER reads the Superelevation from file
;
;
(defun RFL:RSUPER (INFILENAME / INFILE INLINE STA SUPERLEFT SUPERRIGHT)
 (if (/= INFILENAME nil) (setq INFILENAME (findfile INFILENAME)))
 (if (/= INFILENAME nil)
  (progn
   (vl-registry-write "HKEY_CURRENT_USER\\rflAlignDirectory" "" (strcat (vl-filename-directory INFILENAME) "\\"))
   (setq INFILE (open INFILENAME "r"))
   (setq RFL:SUPERLIST nil)
   (setq INLINE (read-line INFILE))
   (if (/= INLINE "#RFL SUPERELEVATION FILE")
    (progn
     (princ "\n*** FILE NOT FORMATTED CORRECTLY ***\n")
    )
    (progn
     (setq INLINE (read-line INFILE))
     (while (and (/= nil INLINE) (/= INLINE "#END DEFINITION"))
      (setq STA (atof INLINE))
      (setq INLINE (read-line INFILE))
      (setq SUPERLEFT (atof INLINE))
      (setq INLINE (read-line INFILE))
      (setq SUPERRIGHT (atof INLINE))
      (setq INLINE (read-line INFILE))
      (setq RFL:SUPERLIST (append RFL:SUPERLIST (list (list STA SUPERLEFT SUPERRIGHT))))
     )
    )
   )
   (close INFILE)
   T
  )
  nil
 )
)
;
;
;   Program written by Robert Livingston, 2008-11-04
;
;   RFL:RSUPERB reads the Superelevation from a RFLAlign Block
;
;
(defun RFL:RSUPERB (BLKENT / ENT ENTLIST INLINE STA SUPERLEFT SUPERRIGHT)
 (setq RFL:SUPERLIST nil)
 (setq ENT (entnext BLKENT))
 (setq ENTLIST (entget ENT))
 (while (/= "E" (cdr (assoc 2 ENTLIST)))
  (setq ENT (entnext ENT))
  (setq ENTLIST (entget ENT))
 )
 (setq INLINE (cdr (assoc 1 ENTLIST)))
 (setq ENT (entnext ENT))
 (setq ENTLIST (entget ENT))
 (if (/= INLINE "#RFL SUPERELEVATION FILE")
  (progn
   (princ "\n*** FILE NOT FORMATTED CORRECTLY ***\n")
  )
  (progn
   (setq INLINE (cdr (assoc 1 ENTLIST)))
   (setq ENT (entnext ENT))
   (setq ENTLIST (entget ENT))
   (while (and (/= nil INLINE) (/= INLINE "#END DEFINITION"))
    (setq STA (atof INLINE))
    (setq INLINE (cdr (assoc 1 ENTLIST)))
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
    (setq SUPERLEFT (atof INLINE))
    (setq INLINE (cdr (assoc 1 ENTLIST)))
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
    (setq SUPERRIGHT (atof INLINE))
    (setq INLINE (cdr (assoc 1 ENTLIST)))
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
    (setq RFL:SUPERLIST (append RFL:SUPERLIST (list (list STA SUPERLEFT SUPERRIGHT))))
   )
  )
 )
)
;
;
;   Program written by Robert Livingston, 2008/11/04
;
;   RFL:WSUPERB writes the superelevation to a RFLALIGN Block
;
;
(defun RFL:WSUPERB (BLKENT / BLKENTNEW BLKENTLIST C ENT ENTLIST ENTN)
 (entmake)
 (setq BLKENTLIST (entget BLKENT))
 (setq BLKENTNEW (entmake BLKENTLIST))
 (setq ENT (entnext BLKENT))
 (setq ENTLIST (entget ENT))
 (while (/= "SEQEND" (cdr (assoc 0 ENTLIST)))
  (if (= "E" (cdr (assoc 2 ENTLIST)))
   (progn
    (setq ENTLIST (subst (cons 1 "#RFL SUPERELEVATION FILE") (assoc 1 ENTLIST) ENTLIST))
    (entmake ENTLIST)
    (setq C 0)
    (while (< C (length RFL:SUPERLIST))
     (setq ENTLIST (subst (cons 70 1) (assoc 70 ENTLIST) ENTLIST))
     (setq ENTLIST (subst (cons 1 (rtos (nth 0 (nth C RFL:SUPERLIST)) 2 16)) (assoc 1 ENTLIST) ENTLIST))
     (entmake ENTLIST)
     (setq ENTLIST (subst (cons 1 (rtos (nth 1 (nth C RFL:SUPERLIST)) 2 16)) (assoc 1 ENTLIST) ENTLIST))
     (entmake ENTLIST)
     (setq ENTLIST (subst (cons 1 (rtos (nth 2 (nth C RFL:SUPERLIST)) 2 16)) (assoc 1 ENTLIST) ENTLIST))
     (entmake ENTLIST)
     (setq C (+ C 1))
    )
    (setq ENTLIST (subst (cons 1 "#END DEFINITION") (assoc 1 ENTLIST) ENTLIST))
    (entmake ENTLIST)
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
    (while (= "E" (cdr (assoc 2 ENTLIST)))
     (setq ENT (entnext ENT))
     (setq ENTLIST (entget ENT))
    )
   )
   (progn
    (entmake ENTLIST)
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
   )
  )
 )
 (entmake ENTLIST)
 (entdel BLKENT)
 (setq BLKENTNEW (entlast))
)
;
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
;     Program written by Robert Livingston, 2008-11-04
;
;     RFL:RABKILL removes alignment definition lists from RFLALIGN blocks
;
;
(defun RFL:RABKILL (BLKENT NODE / BLKENTNEW BLKENTLIST ENT ENTLIST ENTN)
 (entmake)
 (setq BLKENTLIST (entget BLKENT))
 (setq BLKENTNEW (entmake BLKENTLIST))
 (setq ENT (entnext BLKENT))
 (setq ENTLIST (entget ENT))
 (while (/= "SEQEND" (cdr (assoc 0 ENTLIST)))
  (if (= NODE (cdr (assoc 2 ENTLIST)))
   (progn
    (setq ENTLIST (subst (cons 1 "N/A") (assoc 1 ENTLIST) ENTLIST))
    (entmake ENTLIST)
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
    (while (= NODE (cdr (assoc 2 ENTLIST)))
     (setq ENT (entnext ENT))
     (setq ENTLIST (entget ENT))
    )
   )
   (progn
    (entmake ENTLIST)
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
   )
  )
 )
 (entmake ENTLIST)
 (entdel BLKENT)
 (setq BLKENTNEW (entlast))
)
;
;
;     Program written by Robert Livingston, 2014-11-20
;
;     RFL:STATXT converts a real to a station string
;
;
(setq RFL:STAPOS nil)
(defun RFL:STATXT (STA / C DIMZIN S STAH STAL)
 (if (= nil RFL:STAPOS) (if (= nil (setq RFL:STAPOS (getint "\nStation label '+' location <3> : "))) (setq RFL:STAPOS 3)))
 (setq DIMZIN (getvar "DIMZIN"))
 (setvar "DIMZIN" 8)
 (if (< RFL:STAPOS 1)
  (rtos STA)
  (progn
   (if (< STA 0.0)
    (setq S "-")
    (setq S "")
   )
   (setq STAH (fix (/ (abs STA) (expt 10 RFL:STAPOS))))
   (setq STAL (- (abs STA) (* STAH (expt 10 RFL:STAPOS))))
   (if (= (substr (rtos STAL) 1 (+ RFL:STAPOS 1)) (itoa (expt 10 RFL:STAPOS)))
    (progn
     (setq STAL 0.0)
     (setq STAH (+ STAH (RFL:SIGN STAH)))
    )
   )
   (setq STAH (itoa STAH))
   (setq C (- RFL:STAPOS (strlen (itoa (fix STAL)))))
   (setq STAL (rtos STAL 2 (getvar "LUPREC")))
   (while (> C 0)
    (setq STAL (strcat "0" STAL))
    (setq C (- C 1))
   )
   (setvar "DIMZIN" DIMZIN)
   (setq RFLSTAHTXT (strcat S STAH) RFLSTALTXT STAL)
   (strcat S STAH "+" STAL)
  )
 )
)
;
;
;     Program written by Robert Livingston, 2015-03-13
;
;     RFL:FIX+ modifies a text entity to adjust it's '+' to align with its insertion point.
;
;
(defun RFL:FIX+ (ENT / CODE ENTLIST P P0 STR TB TB1 TB2 W WL WR)
 (setq ENTLIST (entget ENT))
 (if (= "TEXT" (cdr (assoc 0 ENTLIST)))
  (if (/= nil (vl-string-search "+" (setq STR (cdr (assoc 1 ENTLIST)))))
   (progn
    (if (or (/= 0 (cdr (assoc 72 ENTLIST))) (/= 0 (cdr (assoc 73 ENTLIST))))
     (setq CODE 11)
     (setq CODE 10)
    )
    (setq P (cdr (assoc CODE ENTLIST)))
    (setq P0 (cdr (assoc 10 ENTLIST)))
    (setq TB (textbox ENTLIST))
    (setq W (- (caadr TB) (caar TB)))
    (setq TBL (textbox (subst (cons 1 (substr STR 1 (+ (vl-string-search "+" STR) 1))) (assoc 1 ENTLIST) ENTLIST)))
    (setq WL (- (caadr TBL) (caar TBL)))
    (setq TBR (textbox (subst (cons 1 (substr STR (+ (vl-string-search "+" STR) 1))) (assoc 1 ENTLIST) ENTLIST)))
    (setq WR (- (caadr TBR) (caar TBR)))
    (setq W+ (- (+ WR WL) W))
    (setq ENTLIST (subst (list CODE
                               (- (+ (car P) (- (car P) (car P0))) (- WL (/ W+ 2.0)) (caar TBL))
                               (cadr P)
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
;
;
;     Program written by Robert Livingston, 2015-03-13
;
;     RFL:GETPLIST returns a list of points along a polyline
;
;
(defun RFL:GETPLIST (ENT / ENTLIST P PLIST ZFLAG)
 (setq PLIST nil)
 (setq ENTLIST (entget ENT))
 (if (= "POLYLINE" (cdr (assoc 0 ENTLIST)))
  (progn
   (if (/ (cdr (assoc 70 ENTLIST)) 8) (setq ZFLAG T))
   (setq ENT (entnext ENT))
   (setq ENTLIST (entget ENT))
   (while (= "VERTEX" (cdr (assoc 0 ENTLIST)))
    (setq P (cdr (assoc 10 ENTLIST)))
    (if ZFLAG
     (setq P (list (car P) (cadr P) (caddr P)))
     (setq P (list (car P) (cadr P)))
    )
    (setq PLIST (append PLIST (list P)))
    (setq ENT (entnext ENT))
    (setq ENTLIST (entget ENT))
   )
  )
 )
 (if (= "LWPOLYLINE" (cdr (assoc 0 ENTLIST)))
  (progn
   (while (/= nil ENTLIST)
    (setq P (car ENTLIST))
    (setq ENTLIST (cdr ENTLIST))
    (if (= 10 (car P))
     (progn
      (setq P (list (cadr P) (caddr P)))
      (setq PLIST (append PLIST (list P)))
     )
    )
   )
  )
 )
 PLIST
)
;
;
;     Program written by Robert Livingston, 2015-03-13
;
;     RFL:POINTINSIDE checks if a point is inside a polyline formed by PLIST
;
;
(defun RFL:POINTINSIDE (P PLIST / CROSSINGCOUNT P0 P1 PBASE PTMP)
 (setq P0 (last PLIST))
 ;  Subtracted/added pi from to the 'X' and 'Y' coordinate to have a point that is outside PLIST and 'hopefully' prevent on edge case
 (setq PBASE (list (- (apply 'min (mapcar '(lambda (PTMP) (car PTMP)) PLIST)) pi)
                   (+ (apply 'min (mapcar '(lambda (PTMP) (cadr PTMP)) PLIST)) pi)
             )
 )
 (setq CROSSINGCOUNT 0)
 (foreach P1 PLIST
  (progn
   (if (inters PBASE P P0 P1)
    (setq CROSSINGCOUNT (1+ CROSSINGCOUNT))
   )
   (setq P0 P1)
  )
 )
 (if (= 0 (rem CROSSINGCOUNT 2))
  nil
  T
 )
)
;
;
;     Program written by Robert Livingston, 2016-09-23
;
;     RFL:INTERS returns the intersection of a line defined by P1/P2 and an RFL alignment
;
;
(defun RFL:ALINTERS (P1 P2 ALIGNLIST / ALSAVE C OS P SWAP TOL)
 (setq TOL 0.00001)
 (defun SWAP (/ TMP)
  (setq TMP ALIGNLIST ALIGNLIST ALSAVE ALSAVE TMP)
 )
 (setq C 0)
 (setq P (list (/ (+ (car P1) (car P2)) 2.0) (/ (+ (cadr P1) (cadr P2)) 2.0)))
 (setq ALSAVE (list (list 0.0 P1 P2 0.0)))
 (setq P (STAOFF P))
 (while (and P
             (> (abs (cadr P)) TOL)
             (< C 100)
        )
  (setq P (XY (list (car P) 0.0)))
  (SWAP)
  (setq P (STAOFF P))
  (setq C (+ C 1))
  (if (>= C 100)
   (princ (strcat "\n*** Warning - Maximum number of iterations reached at station " (rtos STA) "\n"))
  )
 )
 (if P (setq P (XY (list (car P) 0.0))))
 P
)
;
;
;     Program written by Robert Livingston
;
;
(defun RFL:ACADVER (/ ACADPROD)
 (if (= nil vlax-machine-product-key)
  (setq ACADPROD (vlax-product-key))
  (setq ACADPROD (vlax-machine-product-key))
 )
 (cond ((vl-string-search "\\R17.1\\" ACADPROD)
        "5.0"
       )
       ;;2008
       ((vl-string-search "\\R17.2\\" ACADPROD)
        "6.0"
       )
       ;;2009
       ((vl-string-search "\\R18.0\\" ACADPROD)
        "7.0"
       )
       ;;2010
       ((vl-string-search "\\R18.1\\" ACADPROD)
        "8.0"
       )
       ;;2011
       ((vl-string-search "\\R18.2\\" ACADPROD)
        "9.0"
       )
       ;;2012
       ((vl-string-search "\\R19.0\\" ACADPROD)
        "10.0"
       )
       ;;2013
       ((vl-string-search "\\R19.1\\" ACADPROD)
        "10.3"
       )
       ;;2014
       ((vl-string-search "\\R20.0\\" ACADPROD)
        "10.4"
       )
       ;;2015
       ((vl-string-search "\\R20.1\\" ACADPROD)
        "10.5"
       )
       ;;2016
       ((vl-string-search "\\R21.0\\" ACADPROD)
        "11.0"
       )
       ;;2017
 )
)
;
;
;     Program written by Robert Livingston
;
;
(defun RFL:GETC3DALIGNMENT (/ ENT ENTLIST GETFROMLIST OBALIGNMENT)
 (defun GETFROMLIST (/ *acad* ACADACTIVEDOCUMENT ACADPROD ACADVER C3DOBJECT C3DDOC C3DALIGNS C CMAX C3DALIGN)
  (textscr)
  (princ "\n")
  (setq ACADPROD (strcat "AeccXUiLand.AeccApplication." (RFL:ACADVER)))
  (setq *acad* (vlax-get-acad-object))
  (setq C3DOBJECT (vla-getinterfaceobject *acad* ACADPROD))
  (setq C3DDOC (vla-get-activedocument C3DOBJECT))
  (setq C3DALIGNS (vlax-get C3DDOC 'alignmentssiteless))
  (setq CMAX (vlax-get-property C3DALIGNS "Count"))
  (setq C 0)
  (while (< C CMAX)
   (setq C3DALIGN (vlax-invoke-method C3DALIGNS "Item" C))
   (setq C (+ C 1))
   (princ (strcat (itoa C) " - " (vlax-get-property C3DALIGN "DisplayName") "\n"))
  )
  (setq C (getint "Enter alignment number : "))
  (setq OBALIGNMENT (vlax-invoke-method C3DALIGNS "Item" (- C 1)))
  (graphscr)
 )
 (setq OBALIGNMENT nil)
 (setq ENT (car (entsel "\nSelect C3D alignment (<return> to choose from list) : ")))
 (if (= nil ENT)
  (GETFROMLIST)
  (progn
   (setq ENTLIST (entget ENT))
   (if (/= "AECC_ALIGNMENT" (cdr (assoc 0 ENTLIST)))
    (princ "\n*** Not a C3D Alignment ***")
    (setq OBALIGNMENT (vlax-ename->vla-object ENT))
   )
  )
 )
 OBALIGNMENT
)
;
;
;     Program written by Robert Livingston
;
;
(defun RFL:GETC3DSURFACE (/ ENT ENTLIST GETFROMLIST OBSURFACE)
 (defun GETFROMLIST (/ *acad* ACADPROD C3DOBJECT C3DDOC C3DSURFS C CMAX C3DSURF)
  (textscr)
  (princ "\n")
  (setq ACADPROD (strcat "AeccXUiLand.AeccApplication." (RFL:ACADVER)))
  (setq *acad* (vlax-get-acad-object))
  (setq C3DOBJECT (vla-getinterfaceobject *acad* ACADPROD))
  (setq C3DDOC (vla-get-activedocument C3DOBJECT))
  (setq C3DSURFS (vlax-get C3DDOC 'surfaces))
  (setq CMAX (vlax-get-property C3DSURFS "Count"))
  (setq C 0)
  (while (< C CMAX)
   (setq C3DSURF (vlax-get-property C3DSURFS "Item" C))
   (setq C (+ C 1))
   (princ (strcat (itoa C) " - " (vlax-get-property C3DSURF "DisplayName") "\n"))
  )
  (setq C (getint "Enter surface number : "))
  (setq OBSURFACE (vlax-get-property C3DSURFS "Item" (- C 1)))
  (graphscr)
 )

 (setq OBSURFACE nil)

 (setq ENT (car (entsel "\nSelect C3D surface or <return> to select from list : ")))
 (if (= nil ENT)
  (GETFROMLIST)
  (progn
   (setq ENTLIST (entget ENT))
   (if (/= "AECC_TIN_SURFACE" (cdr (assoc 0 ENTLIST)))
    (if (/= "AECC_GRID_SURFACE" (cdr (assoc 0 ENTLIST)))
     (princ "\n*** Not a C3D Surface ***")
     (setq OBSURFACE (vlax-ename->vla-object ENT))
    )
    (setq OBSURFACE (vlax-ename->vla-object ENT))
   )
  )
 )
 OBSURFACE
)
;
;
;     Program written by Robert Livingston
;
;
(defun RFL:GETSURFACELINE (P1 P2 OBSURFACE / C CATCHERROR OGLINE OGLINELIST VARLIST)
 (setq OGLINE nil)
 (setq VARLIST (list OBSURFACE "SampleElevations" (car P1) (cadr p1) (car P2) (cadr p2)))
 (setq OGLINE (vl-catch-all-apply 'vlax-invoke-method VARLIST))
 (if (not (vl-catch-all-error-p OGLINE))
  (if (/= nil OGLINE)
   (if (/= 0 (vlax-variant-type OGLINE))
    (progn
     (setq OGLINELIST nil)
     (setq OGLINE (vlax-variant-value OGLINE))
     (setq C (vlax-safearray-get-l-bound OGLINE 1))
     (while (<= C (vlax-safearray-get-u-bound OGLINE 1))
      (setq OGLINELIST (append OGLINELIST (list (list (vlax-safearray-get-element OGLINE C)
                                                      (vlax-safearray-get-element OGLINE (+ C 1))
                                                      (vlax-safearray-get-element OGLINE (+ C 2))))))
      (setq C (+ C 3))
     )
    )
   )
  )
 )
 OGLINELIST
)
;
;
;     Program written by Robert Livingston
;
;
(defun RFL:GETSURFACEPOINT (P OBSURFACE / VARLIST)
 (setq VARLIST (list OBSURFACE "FindElevationAtXY" (car P) (cadr P)))
 (setq Z (vl-catch-all-apply 'vlax-invoke-method VARLIST))
 (if (vl-catch-all-error-p Z)
  nil
  Z
 )
)
;
;
;     Program written by Robert Livingston
;
;
(defun RFL:GETSECTIONSET (STASTART STAEND SWATH STEP OBSURFACE RFL:ALIGNLIST / P1 P2 PLIST SECTIONSET STA SLIST)
 (princ "\nGetting sections : ")
 (setq STA STASTART)
 (while (<= STA STAEND)
  (princ (strcat "\n" (RFL:STATXT STA) "..."))
  (setq P1 (RFL:XY (list STA (/ SWATH -2.0))))
  (setq P2 (RFL:XY (list STA (/ SWATH 2.0))))
  (if (and (/= nil P1) (/= nil P2))
   (progn
    (setq PLIST (RFL:GETSURFACELINE P1 P2 OBSURFACE))
    (setq SLIST nil)
    (foreach NODE PLIST
     (progn
      (setq P (list (car NODE) (cadr NODE)))
      (setq SLIST (append SLIST (list (list (- (distance P1 P) (/ SWATH 2.0)) (last NODE)))))
     )
    )
    (setq SECTIONSET (append SECTIONSET (list (list STA SLIST))))
   )
  )
  (setq STA (+ STA STEP))
 )
 SECTIONSET
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
   (setq RFL:ALIGNLIST nil)
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
         (setq RFL:ALIGNLIST (RFL:ALIGNDEF (list ALIGNENT) PSTART STASTART))
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
           (setq RFL:ALIGNLIST (RFL:ALIGNDEF ALIGNENT PSTART STASTART))
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

 (if (/= nil RFL:ALIGNLIST)
  (progn
   (setq AL RFL:ALIGNLIST)
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
 (if (= RFL:ALIGNLIST nil)
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

 (if (/= nil RFL:ALIGNLIST)
  (progn
   (setq OS (getreal "\nEnter offset (-ve = left, +ve = right) : "))
   (setq AL RFL:ALIGNLIST)
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

 (if (/= nil RFL:PVILIST)
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
     (if (= REP "Yes") (command "._ERASE" (ssget "X" (list (cons 8 (cdr (assoc "PLAYER" RFL:PROFDEFLIST))))) ""))
     (RFL:DRAWPROF RFL:PVILIST)
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
           (setq VEXAG (getreal (strcat "\nEnter vertical exageration <" (rtos (if RFL:PROFDEFLIST (cdr (assoc "VEXAG" RFL:PROFDEFLIST)) 10.0)) "> :")))
           (if (= VEXAG nil) (setq VEXAG (if RFL:PROFDEFLIST (cdr (assoc "VEXAG" RFL:PROFDEFLIST)) 10.0)))
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
;   DPROFOG draws on the current layer the current OG profile defined in RFL:OGLIST
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

 (if (/= nil RFL:OGLIST)
  (progn
   (RFL:PROFDEF)
   (setq C 0)
   (command "._PLINE")
   (while (< C (length RFL:OGLIST))
    (command (RFL:PROFPOINT (nth 0 (nth C RFL:OGLIST)) (nth 1 (nth C RFL:OGLIST))))
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

 (setq RFL:PVILIST nil)
 (if (/= nil RFL:PROFDEFLIST)
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
                                   (cons 8 (cdr (assoc "PLAYER" RFL:PROFDEFLIST))))))
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
        (if (or (= (cdr (assoc "DIRECTION" RFL:PROFDEFLIST)) 1) (= (assoc "DIRECTION" RFL:PROFDEFLIST) nil))
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
                          (nth 0 (cdr (assoc "BPOINT" RFL:PROFDEFLIST))))
                       (if (or (= (cdr (assoc "DIRECTION" RFL:PROFDEFLIST)) 1) (= (assoc "DIRECTION" RFL:PROFDEFLIST) nil)) 1.0 -1.0)
                    )
                    (cdr (assoc "STA" RFL:PROFDEFLIST))
                 )
       )
       (setq ELEV (+ (/ (- (nth 1 (cdr (assoc 10 (entget PVIENT))))
                           (nth 1 (cdr (assoc "BPOINT" RFL:PROFDEFLIST))))
                        (cdr (assoc "VEXAG" RFL:PROFDEFLIST)))
                     (cdr (assoc "ELEV" RFL:PROFDEFLIST))))
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
       (setq RFL:PVILIST (append (list (list STA ELEV LR VAL)) RFL:PVILIST))
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

 (setq RFL:OGLIST nil)
 (if (/= nil RFL:PROFDEFLIST)
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
                             (nth 0 (cdr (assoc "BPOINT" RFL:PROFDEFLIST))))
                          (if (or (= (cdr (assoc "DIRECTION" RFL:PROFDEFLIST)) 1) (= (assoc "DIRECTION" RFL:PROFDEFLIST) nil)) 1.0 -1.0)
                       )
                       (cdr (assoc "STA" RFL:PROFDEFLIST))
                    )
          )
          (setq ELEV (+ (/ (- (nth 1 (cdr NODE))
                              (nth 1 (cdr (assoc "BPOINT" RFL:PROFDEFLIST))))
                           (cdr (assoc "VEXAG" RFL:PROFDEFLIST)))
                        (cdr (assoc "ELEV" RFL:PROFDEFLIST))))
          (setq RFL:OGLIST (append (list (list STA ELEV)) RFL:OGLIST))
          (setq NODEPREV NODE)
         )
        )
       )
      )
      (if (> (nth 0 (car RFL:OGLIST)) (nth 0 (last RFL:OGLIST)))
       (setq RFL:OGLIST (reverse RFL:OGLIST))
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
(defun C:GPROFP (/ ANGBASE ANGDIR CMDECHO ENT ENTLIST ELEV LR NODE NODEPREV P PLIST TOL)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 0)

 (setq TOL 0.0001)
 (setq NODEPREV nil)
 
 (RFL:PROFDEF)

 (setq PLIST nil)
 (if (/= nil RFL:PROFDEFLIST)
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
                             (nth 0 (cdr (assoc "BPOINT" RFL:PROFDEFLIST))))
                          (if (or (= (cdr (assoc "DIRECTION" RFL:PROFDEFLIST)) 1) (= (assoc "DIRECTION" RFL:PROFDEFLIST) nil)) 1.0 -1.0)
                       )
                       (cdr (assoc "STA" RFL:PROFDEFLIST))
                    )
          )
          (setq ELEV (+ (/ (- (nth 1 (cdr NODE))
                              (nth 1 (cdr (assoc "BPOINT" RFL:PROFDEFLIST))))
                           (cdr (assoc "VEXAG" RFL:PROFDEFLIST)))
                        (cdr (assoc "ELEV" RFL:PROFDEFLIST))))
          (setq PLIST (append (list (list STA ELEV)) PLIST))
          (setq NODEPREV NODE)
         )
        )
       )
      )
      (if (> (nth 0 (car PLIST)) (nth 0 (last PLIST)))
       (setq PLIST (reverse PLIST))
      )
     )
    )
   )
  )
 )
 (if (/= nil PLIST)
  (progn
   (setq RFL:PVILIST nil)
   (foreach NODE PLIST
    (progn
     (setq RFL:PVILIST (append RFL:PVILIST (list (list (car NODE) (cadr NODE) "L" 0.0))))
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
;   C:RPROFOG reads an OG vertical alignment from file and sets the global variable RFL:OGLIST
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
 (if (= RFL:PVILIST nil)
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

 (if (= RFL:OGLIST nil)
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
    (setq STA (float (* (fix (/ (caar RFL:ALIGNLIST)
                                (cdr (assoc "LABELINC" RFL:LALIGNLIST))
                             )
                        )
                        (cdr (assoc "LABELINC" RFL:LALIGNLIST))
                     )
              )
    )
    (setq STAEND (+ (caar RFL:ALIGNLIST) (RFL:GETALIGNLENGTH)))
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
    (setq STA (float (* (fix (/ (caar RFL:ALIGNLIST)
                                (cdr (assoc "TICKINC" RFL:LALIGNLIST))
                             )
                        )
                        (cdr (assoc "TICKINC" RFL:LALIGNLIST))
                     )
              )
    )
    (setq STAEND (+ (caar RFL:ALIGNLIST) (RFL:GETALIGNLENGTH)))
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
    (foreach NODE RFL:ALIGNLIST
     (setq STA (car NODE))
     (setq STAH (RFL:STATXT STA))
     (setq STAH (strcat (NODELABEL NODE NODEPREV) STAH))
     (NODEINSERT STA STAH)
     (setq NODEPREV NODE)
    )
    (setq STA (+ (caar RFL:ALIGNLIST) (RFL:GETALIGNLENGTH)))
    (setq STAH (RFL:STATXT STA))
    (setq STAH (strcat (NODELABEL (last RFL:ALIGNLIST) nil) STAH))
    (NODEINSERT STA STAH)
   )
   (princ "\n!!! Unable to locate or create Lable Block !!!")
  )
  (setvar "CLAYER" CLAYER)
  (setvar "ANGBASE" ANGBASE)
  (setvar "ANGDIR" ANGDIR)
  1
 )
 (if RFL:ALIGNLIST
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
;
;
;     Program written by Robert Livingston, 2016/07/11
;
;     C:LPROF is a utility for labelling profiles
;
;     xxxLAYER  :  '*' concatinates current layer
;
(setq RFL:LPROFLIST (list (cons "PROFDRAW" 0)           ;  Draw profile
                          (cons "PROFLAYER" "*")        ;  Profile Layer
                          (cons "TEXTLAYER" "*-LBL")    ;  Text Layer
                          (cons "TEXTHEIGHT" 3.5)       ;  Text Height
                          (cons "UNITS" "m")            ;  Linear Units
                          (cons "LSLOPE" 1)             ;  Label Slope
                          (cons "LL" 1)                 ;  Label 'L'
                          (cons "LK" 1)                 ;  Label 'K'
                          (cons "CNODES" 1)             ;  Circle Nodes
                          (cons "CNODERAD" 1.0)         ;  Node Circle Radius
                          (cons "CNODELENGTH" 10.0)     ;  Node Line Length
                          (cons "CNODEVOFFSET" 1.0)     ;  Node Text Vertical Offset
                          (cons "CNODEHOFFSET" 0.875)   ;  Node Text Horisontal Offset
                          (cons "DPVI" 1)               ;  Draw PVI
                          (cons "LPVI" 1)               ;  Label PVI
                          (cons "LBVC" 1)               ;  Label BVC and EVC
                          (cons "LHIGH" 0)              ;  Label 'high' Chainage (if not stations are labelled as '+234.567'
                          (cons "LELEVATIONSC" 1)       ;  Label Elevations Curves
                          (cons "LELEVATIONST" 1)       ;  Label Elevations Tangents
                          (cons "ELEVTEXTHEIGHT" 2.5)   ;  Elevation Text Height
                          (cons "ELEVTEXTINC" 20.0)     ;  Elevation Text Increment
                          (cons "ELEVTEXTOS" -20.0)     ;  Elevation Text Offset (from grid basepoint)
                          (cons "RAB" 0)                ;  Reverse Above/Below flag (1 = labels above with K/L below)
                          (cons "DIRECTION" "DIRRIGHT") ;  Label Direction (DIRRIGHT / DIRLEFT / DIRUP / DIRDOWN)
                          (cons "KPREC" 1)              ;  'K' Precision
                          (cons "LPREC" 0)              ;  'L' Precision
                          (cons "SLOPEPREC" 3)          ;  Slope Precision
                          (cons "STAPREC" 3)            ;  Station Precision
                          (cons "ELEVPREC" 3)           ;  Elevation Precision
                    )
)
(defun C:LPROF (/ ACTIVEDOC ACTIVESPC ADDTEXT ANG ANGBASE ANGDIR C CLAYER CMDECHO DIMZIN DIRECTIONT DIRECTIONS ENT
                  G1 G2 K L1 L2 L3 LUPREC MLMR MLMRT MLMRB NLAYER OSMODE P P1 P2 PLINETYPE PREVENT
                  REGENMODE REP SIDE STA STA1 STA2 STA3 STAH STAL STAHTXT SPLINETYPE
                  SPLINESEGS TEXTSTYLE TOL Z1 Z2 Z3 ZMAX ZMIN)
 (setq REGENMODE (getvar "REGENMODE"))
 (setvar "REGENMODE" 1)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq DIMZIN (getvar "DIMZIN"))
 (setq LUPREC (getvar "LUPREC"))
 (setvar "DIMZIN" 0)
 (setq CLAYER (getvar "CLAYER"))
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0.0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 0)
 (setq OSMODE (getvar "OSMODE"))
 (setvar "OSMODE" 0)
 (setq SPLINETYPE (getvar "SPLINETYPE"))
 (setvar "SPLINETYPE" 5)
 (setq SPLINESEGS (getvar "SPLINESEGS"))
 (setvar "SPLINESEGS" 65)
 (setq PLINETYPE (getvar "PLINETYPE"))
 (setvar "PLINETYPE" 0)
 (setq TEXTSTYLE (getvar "TEXTSTYLE"))

 (setq TOL 0.000001)
 
 (setq PREVENT nil)
 
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

 (defun ADDTEXT (STR P H AL A / ANGBASE ANGDIR OBJ P1)
  (setq ANGBASE (getvar "ANGBASE"))
  (setvar "ANGBASE" 0.0)
  (setq ANGDIR (getvar "ANGDIR"))
  (setvar "ANGDIR" 0)
  ; acAlignmentLeft          :   0
  ; acAlignmentCenter        :   1
  ; acAlignmentRight         :   2
  ; acAlignmentMiddle        :   4
  ; acAlignmentTopLeft       :   6
  ; acAlignmentTopCenter     :   7
  ; acAlignmentTopRight      :   8
  ; acAlignmentMiddleLeft    :   9
  ; acAlignmentMiddleCenter  :  10
  ; acAlignmentMiddleRight   :  11
  ; acAlignmentBottomLeft    :  12
  ; acAlignmentBottomCenter  :  13
  ; acAlignmentBottomRight   :  14
  (vla-addtext ACTIVESPC
               STR
               (vlax-3d-point P)
               H
  )
  (setq ENT (entlast))
  (setq OBJ (vlax-ename->vla-object ENT))
  (setq ENTLIST (entget ENT))
  (setq P1 (vlax-get-property OBJ 'InsertionPoint))
  (cond ((= AL "L")
         (progn
          (vlax-put-property OBJ 'Alignment acAlignmentLeft)
         )
        )
        ((= AL "C")
         (progn
          (vlax-put-property OBJ 'Alignment acAlignmentCenter)
          (vlax-put-property OBJ 'TextAlignmentPoint P1)
         )
        )
        ((= AL "R")
         (progn
          (vlax-put-property OBJ 'Alignment acAlignmentRight)
          (vlax-put-property OBJ 'TextAlignmentPoint P1)
         )
        )
        ((= AL "M")
         (progn
          (vlax-put-property OBJ 'Alignment acAlignmentMiddle)
          (vlax-put-property OBJ 'TextAlignmentPoint P1)
         )
        )
        ((= AL "TL")
         (progn
          (vlax-put-property OBJ 'Alignment acAlignmentTopLeft)
          (vlax-put-property OBJ 'TextAlignmentPoint P1)
         )
        )
        ((= AL "TC")
         (progn
          (vlax-put-property OBJ 'Alignment acAlignmentTopCenter)
          (vlax-put-property OBJ 'TextAlignmentPoint P1)
         )
        )
        ((= AL "TR")
         (progn
          (vlax-put-property OBJ 'Alignment acAlignmentTopRight)
          (vlax-put-property OBJ 'TextAlignmentPoint P1)
         )
        )
        ((= AL "ML")
         (progn
          (vlax-put-property OBJ 'Alignment acAlignmentMiddleLeft)
          (vlax-put-property OBJ 'TextAlignmentPoint P1)
         )
        )
        ((= AL "MC")
         (progn
          (vlax-put-property OBJ 'Alignment acAlignmentMiddleCenter)
          (vlax-put-property OBJ 'TextAlignmentPoint P1)
         )
        )
        ((= AL "MR")
         (progn
          (vlax-put-property OBJ 'Alignment acAlignmentMiddleRight)
          (vlax-put-property OBJ 'TextAlignmentPoint P1)
         )
        )
        ((= AL "BL")
         (progn
          (vlax-put-property OBJ 'Alignment acAlignmentBottomLeft)
          (vlax-put-property OBJ 'TextAlignmentPoint P1)
         )
        )
        ((= AL "BC")
         (progn
          (vlax-put-property OBJ 'Alignment acAlignmentBottomCenter)
          (vlax-put-property OBJ 'TextAlignmentPoint P1)
         )
        )
        ((= AL "BR")
         (progn
          (vlax-put-property OBJ 'Alignment acAlignmentBottomRight)
          (vlax-put-property OBJ 'TextAlignmentPoint P1)
         )
        )
  )
  (vlax-put-property OBJ 'Rotation A)
  (setvar "ANGBASE" ANGBASE)
  (setvar "ANGDIR" ANGDIR)
 )
 
 (if RFL:PVILIST
  (progn
   (RFL:PROFDEF)
   (if RFL:PROFDEFLIST
    (progn
     (if (= (cdr (assoc "PROFDRAW" RFL:LPROFLIST)) 1)
      (progn
       (setq NLAYER (cdr (assoc "PROFLAYER" RFL:LPROFLIST)))
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
       (command "._PLINE")
       (foreach TMP RFL:PVILIST
        (progn
         (if (< (cadddr TMP) TOL)
          (command (RFL:PROFPOINT (car TMP) (cadr TMP)))
          (progn
           (setq C 0)
           (while (<= C 64)
            (command (RFL:PROFPOINT (+ (- (car TMP) (/ (cadddr TMP) 2.0)) (* (/ (cadddr TMP) 64) C))
                                    (RFL:ELEVATION (+ (- (car TMP) (/ (cadddr TMP) 2.0)) (* (/ (cadddr TMP) 64) C)))
                     )
            )
            (setq C (+ C 1))
           )
          )
         )
        )
       )
       (command "")
       (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
      )
     )
     (if (or (= (cdr (assoc "LSLOPE" RFL:LPROFLIST)) 1)
             (= (cdr (assoc "LL" RFL:LPROFLIST)) 1)
             (= (cdr (assoc "LK" RFL:LPROFLIST)) 1)
             (= (cdr (assoc "CNODES" RFL:LPROFLIST)) 1)
             (= (cdr (assoc "DPVI" RFL:LPROFLIST)) 1)
             (= (cdr (assoc "LPVI" RFL:LPROFLIST)) 1)
             (= (cdr (assoc "LBVC" RFL:LPROFLIST)) 1)
             (= (cdr (assoc "LELEVATIONS" RFL:LPROFLIST)) 1)
         )
      (progn
       (if (or (= (cdr (assoc "DIRECTION" RFL:LPROFLIST)) 1)
               (= (cdr (assoc "DIRECTION" RFL:LPROFLIST)) nil)
           )
        (cond ((= (cdr (assoc "DIRECTION" RFL:LPROFLIST)) "DIRLEFT")
               (setq DIRECTIONT 1 DIRECTIONS 1)
              )
              ((= (cdr (assoc "DIRECTION" RFL:LPROFLIST)) "DIRRIGHT")
               (setq DIRECTIONT -1 DIRECTIONS 1)
              )
              ((= (cdr (assoc "DIRECTION" RFL:LPROFLIST)) "DIRUP")
               (setq DIRECTIONT 1 DIRECTIONS 1)
              )
              ((= (cdr (assoc "DIRECTION" RFL:LPROFLIST)) "DIRDOWN")
               (setq DIRECTIONT -1 DIRECTIONS 1)
              )
        )
        (cond ((= (cdr (assoc "DIRECTION" RFL:LPROFLIST)) "DIRLEFT")
               (setq DIRECTIONT 1 DIRECTIONS -1)
              )
              ((= (cdr (assoc "DIRECTION" RFL:LPROFLIST)) "DIRRIGHT")
               (setq DIRECTIONT -1 DIRECTIONS -1)
              )
              ((= (cdr (assoc "DIRECTION" RFL:LPROFLIST)) "DIRUP")
               (setq DIRECTIONT -1 DIRECTIONS -1)
              )
              ((= (cdr (assoc "DIRECTION" RFL:LPROFLIST)) "DIRDOWN")
               (setq DIRECTIONT 1 DIRECTIONS -1)
              )
        )
       )
       (if (= DIRECTIONT 1)
        (progn
         (if (= (cdr (assoc "RAB" RFL:LPROFLIST)) 0)
          (progn
           (setq MLMR "MR")
           (setq MLMRT "R")
           (setq MLMRB "TR")
           (setq SIDE 1)
          )
          (progn
           (setq MLMR "ML")
           (setq MLMRT "L")
           (setq MLMRB "TL")
           (setq SIDE -1)
          )
         )
        )
        (progn
         (if (= (cdr (assoc "RAB" RFL:LPROFLIST)) 0)
          (progn
           (setq MLMR "ML")
           (setq MLMRT "L")
           (setq MLMRB "TL")
           (setq SIDE 1)
          )
          (progn
           (setq MLMR "MR")
           (setq MLMRT "R")
           (setq MLMRB "TR")
           (setq SIDE -1)
          )
         )
        )
       )
       (setq ZMAX (nth 1 (nth 0 RFL:PVILIST)))
       (setq ZMIN (nth 1 (nth 0 RFL:PVILIST)))
       (setq C 1)
       (while (< C (length RFL:PVILIST))
        (if (> (nth 1 (nth C RFL:PVILIST)) ZMAX)
         (setq ZMAX (nth 1 (nth C RFL:PVILIST)))
        )
        (if (< (nth 1 (nth C RFL:PVILIST)) ZMIN)
         (setq ZMIN (nth 1 (nth C RFL:PVILIST)))
        )
        (setq C (+ C 1))
       )
       (setq C 1)
       (setq NLAYER (cdr (assoc "TEXTLAYER" RFL:LPROFLIST)))
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
       (setq STA1 (nth 0 (nth 0 RFL:PVILIST)))
       (setq Z1 (nth 1 (nth 0 RFL:PVILIST)))
       (setq L1 (nth 3 (nth 0 RFL:PVILIST)))
       (setq P (RFL:PROFPOINT STA1 Z1))
       (if (= (cdr (assoc "CNODES" RFL:LPROFLIST)) 1)
        (progn
         (command "._CIRCLE" P (cdr (assoc "CNODERAD" RFL:LPROFLIST)))
         (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
         (command "._LINE"
                  (list (nth 0 P) (+ (nth 1 P) (* SIDE (cdr (assoc "CNODERAD" RFL:LPROFLIST)))))
                  (list (nth 0 P) (+ (nth 1 P) (* SIDE (+ (cdr (assoc "CNODERAD" RFL:LPROFLIST)) (cdr (assoc "CNODELENGTH" RFL:LPROFLIST))))))
                  ""
         )
         (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
        )
       )
       (if (= (cdr (assoc "LBVC" RFL:LPROFLIST)) 1)
        (progn
         (setq TMP (RFL:STATXT STA1))
         (if (= (cdr (assoc "LHIGH" RFL:LPROFLIST)) 0)
          (setq TMP (substr TMP (1+ (vl-string-search "+" TMP))))
         )
         (ADDTEXT (strcat "STA " TMP)
                  (list (+ (nth 0 P) (* DIRECTIONT (* 1.0 (cdr (assoc "CNODEHOFFSET" RFL:LPROFLIST)))))
                        (+ (nth 1 P) (* SIDE (+ (cdr (assoc "CNODERAD" RFL:LPROFLIST)) (cdr (assoc "CNODELENGTH" RFL:LPROFLIST)) (cdr (assoc "CNODEVOFFSET" RFL:LPROFLIST)))))
                  )
                  (cdr (assoc "TEXTHEIGHT" RFL:LPROFLIST))
                  MLMRT
                  (if (= DIRECTIONT 1)
                   (/ pi -2.0)
                   (/ pi 2.0)
                  )
         )
         (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
         (setq TMP (RFL:ROUND Z1 (cdr (assoc "ELEVPREC" RFL:LPROFLIST))))
         (ADDTEXT (strcat "PIVC  "
                          (rtos TMP 2 (cdr (assoc "ELEVPREC" RFL:LPROFLIST)))
                          " "
                          (cdr (assoc "UNITS" RFL:LPROFLIST))
                  )
                  (list (+ (nth 0 P) (* DIRECTIONT (* -1.0 (cdr (assoc "CNODEHOFFSET" RFL:LPROFLIST)))))
                        (+ (nth 1 P) (* SIDE (+ (cdr (assoc "CNODERAD" RFL:LPROFLIST)) (cdr (assoc "CNODELENGTH" RFL:LPROFLIST)) (cdr (assoc "CNODEVOFFSET" RFL:LPROFLIST)))))
                  )
                  (cdr (assoc "TEXTHEIGHT" RFL:LPROFLIST))
                  MLMRB
                  (if (= DIRECTIONT 1)
                   (/ pi -2.0)
                   (/ pi 2.0)
                  )
         )
         (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
        )
       )
       (while (< (+ C 1) (length RFL:PVILIST))
        (setq STA1 (nth 0 (nth (- C 1) RFL:PVILIST)))
        (setq Z1 (nth 1 (nth (- C 1) RFL:PVILIST)))
        (setq L1 (nth 3 (nth (- C 1) RFL:PVILIST)))
        (setq STA2 (nth 0 (nth C RFL:PVILIST)))
        (setq Z2 (nth 1 (nth C RFL:PVILIST)))
        (setq L2 (nth 3 (nth C RFL:PVILIST)))
        (setq STA3 (nth 0 (nth (+ C 1) RFL:PVILIST)))
        (setq Z3 (nth 1 (nth (+ C 1) RFL:PVILIST)))
        (setq L3 (nth 3 (nth (+ C 1) RFL:PVILIST)))
        (setq G1 (* (/ (- Z2 Z1) (- STA2 STA1)) 100.0))
        (setq G2 (* (/ (- Z3 Z2) (- STA3 STA2)) 100.0))
        (setq STA (/ (+ (+ STA1
                           (/ L1 2.0)
                        )
                        (- STA2
                           (/ L2 2.0)
                        )
                     )
                     2.0
                  )
        )
        (if (= (cdr (assoc "LSLOPE" RFL:LPROFLIST)) 1)
         (progn
          (ADDTEXT (strcat (if (> G1 0.0) "+" "") (rtos G1 2 (cdr (assoc "SLOPEPREC" RFL:LPROFLIST))) "%")
                   (list (+ (car (RFL:PROFPOINT STA (RFL:ELEVATION STA)))
                            (* 1.75
                               SIDE
                               (if (= (cdr (assoc "DIRECTION" RFL:PROFDEFLIST)) -1) -1 1)
                               (sin (angle (RFL:PROFPOINT STA2 Z2) (RFL:PROFPOINT STA1 Z1)))
                            )
                         )
                         (+ (cadr (RFL:PROFPOINT STA (RFL:ELEVATION STA)))
                            (* -1.75
                               SIDE
                               (if (= (cdr (assoc "DIRECTION" RFL:PROFDEFLIST)) -1) -1 1)
                               (cos (angle (RFL:PROFPOINT STA2 Z2) (RFL:PROFPOINT STA1 Z1)))
                            )
                         )
                   )
                   (cdr (assoc "TEXTHEIGHT" RFL:LPROFLIST))
                   (if (= SIDE 1) "C" "TC")
                   (if (= DIRECTIONS 1)
                    (angle (RFL:PROFPOINT STA2 Z2) (RFL:PROFPOINT STA1 Z1))
                    (angle (RFL:PROFPOINT STA1 Z1) (RFL:PROFPOINT STA2 Z2))
                   )
          )
          (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
         )
        )
        (setq P (RFL:PROFPOINT STA2 Z2))
        (if (= (cdr (assoc "LPVI" RFL:LPROFLIST)) 1)
         (progn
          (setq TMP (RFL:STATXT STA2))
          (if (= (cdr (assoc "LHIGH" RFL:LPROFLIST)) 0)
           (setq TMP (substr TMP (1+ (vl-string-search "+" TMP))))
          )
          (ADDTEXT (strcat "STA  " TMP)
                   (list (+ (nth 0 P) (* DIRECTIONT (* 1.0 (cdr (assoc "CNODEHOFFSET" RFL:LPROFLIST)))))
                         (+ (nth 1 P) (* SIDE (+ (cdr (assoc "CNODERAD" RFL:LPROFLIST)) (cdr (assoc "CNODELENGTH" RFL:LPROFLIST)) (cdr (assoc "CNODEVOFFSET" RFL:LPROFLIST)))))
                   )
                   (cdr (assoc "TEXTHEIGHT" RFL:LPROFLIST))
                   MLMRT
                  (if (= DIRECTIONT 1)
                   (/ pi -2.0)
                   (/ pi 2.0)
                  )
          )
          (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
          (setq TMP (RFL:ROUND Z2 (cdr (assoc "ELEVPREC" RFL:LPROFLIST))))
          (ADDTEXT (strcat "PIVC  "
                           (rtos TMP 2 (cdr (assoc "ELEVPREC" RFL:LPROFLIST)))
                           " "
                           (cdr (assoc "UNITS" RFL:LPROFLIST))
                   )
                   (list (+ (nth 0 P) (* DIRECTIONT (* -1.0 (cdr (assoc "CNODEHOFFSET" RFL:LPROFLIST)))))
                         (+ (nth 1 P) (* SIDE (+ (cdr (assoc "CNODERAD" RFL:LPROFLIST)) (cdr (assoc "CNODELENGTH" RFL:LPROFLIST)) (cdr (assoc "CNODEVOFFSET" RFL:LPROFLIST)))))
                   )
                   (cdr (assoc "TEXTHEIGHT" RFL:LPROFLIST))
                   MLMRB
                  (if (= DIRECTIONT 1)
                   (/ pi -2.0)
                   (/ pi 2.0)
                  )
          )
          (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
         )
        )
        (if (= (cdr (assoc "DPVI" RFL:LPROFLIST)) 1)
         (progn
          (command "._CIRCLE" (RFL:PROFPOINT STA2 Z2) (cdr (assoc "CNODERAD" RFL:LPROFLIST)))
          (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
          (command "._LINE"
                   (list (nth 0 (RFL:PROFPOINT STA2 Z2)) (+ (nth 1 (RFL:PROFPOINT STA2 Z2)) (* SIDE (cdr (assoc "CNODERAD" RFL:LPROFLIST)))))
                   (list (nth 0 (RFL:PROFPOINT STA2 Z2)) (+ (nth 1 (RFL:PROFPOINT STA2 Z2)) (* SIDE (+ (cdr (assoc "CNODERAD" RFL:LPROFLIST)) (cdr (assoc "CNODELENGTH" RFL:LPROFLIST))))))
                   ""
          )
          (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
         )
        )
        (if (> L2 0.0)
         (progn
          (setq K (abs (/ L2
                          (- G2 G1)
                       )
                  )
          )
          (if (= (cdr (assoc "LL" RFL:LPROFLIST)) 1)
           (progn
            (setq TMP (RFL:ROUND L2 (cdr (assoc "LPREC" RFL:LPROFLIST))))
            (ADDTEXT (strcat (rtos TMP 2 (cdr (assoc "LPREC" RFL:LPROFLIST))) " VC")
                     (list (nth 0 P) (- (nth 1 P) (* SIDE 50.0)))
                     (cdr (assoc "TEXTHEIGHT" RFL:LPROFLIST))
                     "TC"
                    0.0
            )
            (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
           )
          )
          (if (= (cdr (assoc "LK" RFL:LPROFLIST)) 1)
           (progn
            (setq TMP (RFL:ROUND K (cdr (assoc "KPREC" RFL:LPROFLIST))))
            (ADDTEXT (strcat "K = " (rtos TMP 2 (cdr (assoc "KPREC" RFL:LPROFLIST))))
                     (list (nth 0 P) (- (nth 1 P) (* SIDE 50.0) 5.25))
                     (cdr (assoc "TEXTHEIGHT" RFL:LPROFLIST))
                     "TC"
                    0.0
            )
            (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
           )
          )
          (if (= (cdr (assoc "DPVI" RFL:LPROFLIST)) 1)
           (progn
            (setq ANG (angle P (RFL:PROFPOINT STA1 Z1)))
            (command "._LINE"
                     (list (+ (nth 0 P)
                              (* (cdr (assoc "CNODERAD" RFL:LPROFLIST))
                                 (cos ANG)
                              )
                           )
                           (+ (nth 1 P)
                              (* (cdr (assoc "CNODERAD" RFL:LPROFLIST))
                                 (sin ANG)
                              )
                           )
                     )
                     (list (+ (nth 0 P)
                              (* (+ (cdr (assoc "CNODERAD" RFL:LPROFLIST)) (cdr (assoc "CNODELENGTH" RFL:LPROFLIST)))
                                 (cos ANG)
                              )
                           )
                           (+ (nth 1 P)
                              (* (+ (cdr (assoc "CNODERAD" RFL:LPROFLIST)) (cdr (assoc "CNODELENGTH" RFL:LPROFLIST)))
                                 (sin ANG)
                              )
                           )
                     )
                     ""
            )
            (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
            (setq ANG (angle P (RFL:PROFPOINT STA3 Z3)))
            (command "._LINE"
                     (list (+ (nth 0 P)
                              (* (cdr (assoc "CNODERAD" RFL:LPROFLIST))
                                 (cos ANG)
                              )
                           )
                           (+ (nth 1 P)
                              (* (cdr (assoc "CNODERAD" RFL:LPROFLIST))
                                 (sin ANG)
                              )
                           )
                     )
                     (list (+ (nth 0 P)
                              (* (+ (cdr (assoc "CNODERAD" RFL:LPROFLIST)) (cdr (assoc "CNODELENGTH" RFL:LPROFLIST)))
                                 (cos ANG)
                              )
                           )
                           (+ (nth 1 P)
                              (* (+ (cdr (assoc "CNODERAD" RFL:LPROFLIST)) (cdr (assoc "CNODELENGTH" RFL:LPROFLIST)))
                                 (sin ANG)
                              )
                           )
                     )
                     ""
            )
            (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
           )
          )
          (setq P (RFL:PROFPOINT (- STA2 (/ L2 2.0)) (RFL:ELEVATION (- STA2 (/ L2 2.0)))))
          (if (= (cdr (assoc "LBVC" RFL:LPROFLIST)) 1)
           (progn
            (setq TMP (RFL:ROUND (RFL:ELEVATION (- STA2 (/ L2 2.0))) (cdr (assoc "ELEVPREC" RFL:LPROFLIST))))
            (ADDTEXT (strcat "BVC  "
                             (rtos TMP 2 (cdr (assoc "ELEVPREC" RFL:LPROFLIST)))
                             " "
                             (cdr (assoc "UNITS" RFL:LPROFLIST))
                     )
                     (list (nth 0 P)
                           (+ (nth 1 P) (* SIDE (+ (cdr (assoc "CNODERAD" RFL:LPROFLIST)) (cdr (assoc "CNODELENGTH" RFL:LPROFLIST)) (cdr (assoc "CNODEVOFFSET" RFL:LPROFLIST)))))
                     )
                     (cdr (assoc "TEXTHEIGHT" RFL:LPROFLIST))
                     MLMR
                     (if (= DIRECTIONT 1)
                      (/ pi -2.0)
                      (/ pi 2.0)
                     )
            )
            (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
           )
          )
          (if (= (cdr (assoc "CNODES" RFL:LPROFLIST)) 1)
           (progn
            (command "._CIRCLE" P (cdr (assoc "CNODERAD" RFL:LPROFLIST)))
            (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
            (command "._LINE"
                     (list (nth 0 P) (+ (nth 1 P) (* SIDE (cdr (assoc "CNODERAD" RFL:LPROFLIST)))))
                     (list (nth 0 P) (+ (nth 1 P) (* SIDE (+ (cdr (assoc "CNODERAD" RFL:LPROFLIST)) (cdr (assoc "CNODELENGTH" RFL:LPROFLIST))))))
                     ""
            )
            (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
           )
          )
          (setq P (RFL:PROFPOINT (+ STA2 (/ L2 2.0)) (RFL:ELEVATION (+ STA2 (/ L2 2.0)))))
          (if (= (cdr (assoc "LBVC" RFL:LPROFLIST)) 1)
           (progn
            (setq TMP (RFL:ROUND (RFL:ELEVATION (+ STA2 (/ L2 2.0))) (cdr (assoc "ELEVPREC" RFL:LPROFLIST))))
            (ADDTEXT (strcat "EVC  "
                             (rtos TMP 2 (cdr (assoc "ELEVPREC" RFL:LPROFLIST)))
                             " "
                             (cdr (assoc "UNITS" RFL:LPROFLIST))
                     )
                     (list (nth 0 P)
                           (+ (nth 1 P) (* SIDE (+ (cdr (assoc "CNODERAD" RFL:LPROFLIST)) (cdr (assoc "CNODELENGTH" RFL:LPROFLIST)) (cdr (assoc "CNODEVOFFSET" RFL:LPROFLIST)))))
                     )
                     (cdr (assoc "TEXTHEIGHT" RFL:LPROFLIST))
                     MLMR
                     (if (= DIRECTIONT 1)
                      (/ pi -2.0)
                      (/ pi 2.0)
                     )
            )
            (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
           )
          )
          (if (= (cdr (assoc "CNODES" RFL:LPROFLIST)) 1)
           (progn
            (command "._CIRCLE" P (cdr (assoc "CNODERAD" RFL:LPROFLIST)))
            (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
            (command "._LINE"
                     (list (nth 0 P) (+ (nth 1 P) (* SIDE (cdr (assoc "CNODERAD" RFL:LPROFLIST)))))
                     (list (nth 0 P) (+ (nth 1 P) (* SIDE (+ (cdr (assoc "CNODERAD" RFL:LPROFLIST)) (cdr (assoc "CNODELENGTH" RFL:LPROFLIST))))))
                     ""
            )
            (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
           )
          )
          (if (= (cdr (assoc "LELEVATIONSC" RFL:LPROFLIST)) 1)
           (progn
            (setq STA (float (* (+ (fix (/ (- STA2 (/ L2 2.0)) (cdr (assoc "ELEVTEXTINC" RFL:LPROFLIST)))) 1) (cdr (assoc "ELEVTEXTINC" RFL:LPROFLIST)))))
            (while (< STA (+ STA2 (/ L2 2.0)))
             (setq P (RFL:PROFPOINT STA (cdr (assoc "ELEV" RFL:PROFDEFLIST))))
             (setq P (list (car P)
                           (+ (cadr P)
                              (cdr (assoc "ELEVTEXTOS" RFL:LPROFLIST))
                           )
                     )
             )
             (setq TMP (RFL:ROUND (RFL:ELEVATION STA) (cdr (assoc "ELEVPREC" RFL:LPROFLIST))))
             (ADDTEXT (rtos TMP 2 (cdr (assoc "ELEVPREC" RFL:LPROFLIST)))
                      P
                      (cdr (assoc "ELEVTEXTHEIGHT" RFL:LPROFLIST))
                      MLMR
                      (if (= DIRECTIONT 1)
                       (/ pi -2.0)
                       (/ pi 2.0)
                      )
             )
             (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
             (setq STA (+ STA (cdr (assoc "ELEVTEXTINC" RFL:LPROFLIST))))
            )
           )
          )
          (if (= (cdr (assoc "LELEVATIONST" RFL:LPROFLIST)) 1)
           (progn
            (setq STA (float (* (+ (fix (/ (+ STA1 (/ L1 2.0)) (cdr (assoc "ELEVTEXTINC" RFL:LPROFLIST)))) 1) (cdr (assoc "ELEVTEXTINC" RFL:LPROFLIST)))))
            (while (< STA (- STA2 (/ L2 2.0)))
             (setq P (RFL:PROFPOINT STA (cdr (assoc "ELEV" RFL:PROFDEFLIST))))
             (setq P (list (car P)
                           (+ (cadr P)
                              (cdr (assoc "ELEVTEXTOS" RFL:LPROFLIST))
                           )
                     )
             )
             (setq TMP (RFL:ROUND (RFL:ELEVATION STA) (cdr (assoc "ELEVPREC" RFL:LPROFLIST))))
             (ADDTEXT (rtos TMP 2 (cdr (assoc "ELEVPREC" RFL:LPROFLIST)))
                      P
                      (cdr (assoc "ELEVTEXTHEIGHT" RFL:LPROFLIST))
                      MLMR
                      (if (= DIRECTIONT 1)
                       (/ pi -2.0)
                       (/ pi 2.0)
                      )
             )
             (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
             (setq STA (+ STA (cdr (assoc "ELEVTEXTINC" RFL:LPROFLIST))))
            )
           )
          )
         )
        )
        (setq C (+ C 1))
       )
       (setq STA (/ (+ (+ STA2
                          (/ L2 2.0)
                       )
                       (- STA3
                          (/ L3 2.0)
                       )
                    )
                    2.0
                 )
       )
       (if (= (cdr (assoc "LSLOPE" RFL:LPROFLIST)) )
        (progn
         (ADDTEXT (strcat (if (> G2 0.0) "+" "") (rtos G2 2 (cdr (assoc "SLOPEPREC" RFL:LPROFLIST))) "%")
                  (list (+ (car (RFL:PROFPOINT STA (RFL:ELEVATION STA)))
                           (* 1.75
                              SIDE
                              (if (= (cdr (assoc "DIRECTION" RFL:PROFDEFLIST)) -1) -1 1)
                              (sin (angle (RFL:PROFPOINT STA3 Z3) (RFL:PROFPOINT STA2 Z2)))
                           )
                        )
                        (+ (cadr (RFL:PROFPOINT STA (RFL:ELEVATION STA)))
                           (* -1.75
                              SIDE
                              (if (= (cdr (assoc "DIRECTION" RFL:PROFDEFLIST)) -1) -1 1)
                              (cos (angle (RFL:PROFPOINT STA2 Z2) (RFL:PROFPOINT STA1 Z1)))
                           )
                        )
                  )
                  (cdr (assoc "TEXTHEIGHT" RFL:LPROFLIST))
                  (if (= SIDE 1) "C" "TC")
                  (if (= DIRECTIONS 1)
                   (angle (RFL:PROFPOINT STA3 Z3) (RFL:PROFPOINT STA2 Z2))
                   (angle (RFL:PROFPOINT STA2 Z2) (RFL:PROFPOINT STA3 Z3))
                  )
         )
         (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
        )
       )
       (setq P (RFL:PROFPOINT STA3 Z3))
       (if (= (cdr (assoc "CNODES" RFL:LPROFLIST)) 1)
        (progn
         (command "._CIRCLE" P (cdr (assoc "CNODERAD" RFL:LPROFLIST)))
         (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
         (command "._LINE"
                  (list (nth 0 P) (+ (nth 1 P) (* SIDE (cdr (assoc "CNODERAD" RFL:LPROFLIST)))))
                  (list (nth 0 P) (+ (nth 1 P) (* SIDE (+ (cdr (assoc "CNODERAD" RFL:LPROFLIST)) (cdr (assoc "CNODELENGTH" RFL:LPROFLIST))))))
                  ""
         )
         (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
        )
       )
       (if (= (cdr (assoc "LBVC" RFL:LPROFLIST)) 1)
        (progn
         (setq TMP (RFL:STATXT STA3))
         (if (= (cdr (assoc "LHIGH" RFL:LPROFLIST)) 0)
          (setq TMP (substr TMP (1+ (vl-string-search "+" TMP))))
         )
         (ADDTEXT (strcat "STA  " TMP)
                  (list (+ (nth 0 P) (* DIRECTIONT (* 1.0 (cdr (assoc "CNODEHOFFSET" RFL:LPROFLIST)))))
                        (+ (nth 1 P) (* SIDE (+ (cdr (assoc "CNODERAD" RFL:LPROFLIST)) (cdr (assoc "CNODELENGTH" RFL:LPROFLIST)) (cdr (assoc "CNODEVOFFSET" RFL:LPROFLIST)))))
                  )
                  (cdr (assoc "TEXTHEIGHT" RFL:LPROFLIST))
                  MLMRT
                  (if (= DIRECTIONT 1)
                   (/ pi -2.0)
                   (/ pi 2.0)
                  )
         )
         (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
         (setq TMP (RFL:ROUND Z3 (cdr (assoc "SLOPEPREC" RFL:LPROFLIST))))
         (ADDTEXT (strcat "PIVC  "
                          (rtos TMP 2 (cdr (assoc "ELEVPREC" RFL:LPROFLIST)))
                          " "
                          (cdr (assoc "UNITS" RFL:LPROFLIST))
                  )
                  (list (+ (nth 0 P) (* DIRECTIONT (* -1.0 (cdr (assoc "CNODEHOFFSET" RFL:LPROFLIST)))))
                        (+ (nth 1 P) (* SIDE (+ (cdr (assoc "CNODERAD" RFL:LPROFLIST)) (cdr (assoc "CNODELENGTH" RFL:LPROFLIST)) (cdr (assoc "CNODEVOFFSET" RFL:LPROFLIST)))))
                  )
                  (cdr (assoc "TEXTHEIGHT" RFL:LPROFLIST))
                  MLMRB
                  (if (= DIRECTIONT 1)
                   (/ pi -2.0)
                   (/ pi 2.0)
                  )
         )
         (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
        )
       )
       (if (= (cdr (assoc "LELEVATIONSC" RFL:LPROFLIST)) 1)
        (progn
         (setq STA (float (* (+ (fix (/ (+ STA2 (/ L2 2.0)) (cdr (assoc "ELEVTEXTINC" RFL:LPROFLIST)))) 1) (cdr (assoc "ELEVTEXTINC" RFL:LPROFLIST)))))
         (while (< STA (- STA3 (/ L3 2.0)))
          (setq P (RFL:PROFPOINT STA (cdr (assoc "ELEV" RFL:PROFDEFLIST))))
          (setq P (list (car P)
                        (+ (cadr P)
                           (cdr (assoc "ELEVTEXTOS" RFL:LPROFLIST))
                        )
                  )
          )
          (setq TMP (RFL:ROUND (RFL:ELEVATION STA) (cdr (assoc "ELEVPREC" RFL:LPROFLIST))))
          (ADDTEXT (rtos TMP 2 (cdr (assoc "ELEVPREC" RFL:LPROFLIST)))
                   P
                   (cdr (assoc "ELEVTEXTHEIGHT" RFL:LPROFLIST))
                   MLMR
                   (if (= DIRECTIONT 1)
                    (/ pi -2.0)
                    (/ pi 2.0)
                   )
          )
          (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
          (setq STA (+ STA (cdr (assoc "ELEVTEXTINC" RFL:LPROFLIST))))
         )
        )
       )
      )
     )
    )
   )
  )
 )

 (setvar "REGENMODE" REGENMODE)
 (setvar "CMDECHO" CMDECHO)
 (setvar "DIMZIN" DIMZIN)
 (setvar "LUPREC" LUPREC)
 (setvar "CLAYER" CLAYER)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
 (setvar "OSMODE" OSMODE)
 (setvar "SPLINETYPE" SPLINETYPE)
 (setvar "SPLINESEGS" SPLINESEGS)
 (setvar "PLINETYPE" PLINETYPE)
 (setvar "TEXTSTYLE" TEXTSTYLE)
);
;
;     Program written by Robert Livingston, 2016/07/11
;
;     C:PVI0 draws a pviblock with 0.0 for K and L at the selected point
(defun C:PVI0 (/ ACTIVEDOC ACTIVESPACE P)
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
 (if (setq P (getpoint "\nSelect point : "))
  (progn
   (if (= nil (tblsearch "BLOCK" "PVI2")) (RFL:MAKEENT "PVI2"))
   (vla-insertblock ACTIVESPC
                    (vlax-3D-point P)
                    "PVI2"
                    25.4
                    25.4
                    25.4
                    0.0
   )
   (entlast)
  )
  nil
 )
)
;
;
;   Program written by Robert Livingston, 99/10/08
;
;   DSUPER inserts SUPER blocks along the current alignment
;
;
(defun C:DSUPER (/ AL DIMZIN CMDECHO REP SFLAG STEP STEPSTA)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (RFL:DSUPER)

 (setvar "CMDECHO" CMDECHO)
);
;
;   Program written by Robert Livingston, 99/10/08
;
;   WSUPER writes the superelevation to file
;
;
(defun C:WSUPER (/ CMDECHO OUTFILENAME)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (if (= (vl-registry-read "HKEY_CURRENT_USER\\rflAlignDirectory") nil)
  (vl-registry-write "HKEY_CURRENT_USER\\rflAlignDirectory" "" "")
 )
 (if (= SUPERLIST nil)
  (princ "\n*** NO SUPERELEVATION EXISTS - USE RSUPER OR GSUPER ***\n")
  (progn
   (setq OUTFILENAME (getfiled "Select a Superelevation File" (vl-registry-read "HKEY_CURRENT_USER\\rflAlignDirectory") "e" 1))
   (RFL:WSUPER OUTFILENAME)
  )
 )
 (setvar "CMDECHO" CMDECHO)
 nil
)
;
;
;   Program written by Robert Livingston, 99/10/08
;
;   RSUPER reads the Superelevation from file
;
;
(defun C:RSUPER (/ CMDECHO INFILENAME)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (if (= (vl-registry-read "HKEY_CURRENT_USER\\rflAlignDirectory") nil)
  (vl-registry-write "HKEY_CURRENT_USER\\rflAlignDirectory" "" "")
 )
 (setq INFILENAME (getfiled "Select a Superelevation File" (vl-registry-read "HKEY_CURRENT_USER\\rflAlignDirectory") "e" 2))
 (RFL:RSUPER INFILENAME)
 (setvar "CMDECHO" CMDECHO)
 nil
)
;
;
;   Program written by Robert Livingston, 99/10/08
;
;   C:GSUPER extracts superelevation from the current drawing for the current alignment
;
;
(defun C:GSUPER (/ ENTSET)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (if (/= RFL:ALIGNLIST nil)
  (progn
   (if (/= RFL:SUPERDEF nil)
    (progn
     (princ "\nSelect SUPER blocks :")
     (setq ENTSET (ssget))
     (if (/= ENTSET nil)
      (progn
       (RFL:SUPERDEF ENTSET)
      )
     )
    )
    (progn
     (princ "\n!!!!! Superelevation tools not loaded !!!!!\n")
    )
   )
  )
  (progn
   (princ "\n!!!!! Horizontal Alignment Not Set !!!!!\n")
  )
 )
 (setvar "CMDECHO" CMDECHO)
 nil
)
;
;
;    Program written by Robert Livingston, 95/04/25
;                                 Revised, 98/05/12
;
;    RNE labels the Northing and Easting
;
;
(setq RFL:RNELIST (list (cons "NE" 1)   ;  Label Northing and Easting
                        (cons "SO" 1)   ;  Label Station and Offset
                        (cons "Z" 1)    ;  Label Control Elevations
                        (cons "G" 1)    ;  Label Control Grades
                        (cons "SE" 1)   ;  Label Superelevations
                 )
)
(defun C:RNE (/ P1 P2)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (setq P1 (getpoint "\nEnter point :"))
 (setq P2 (getpoint P1 "Second point for leader :"))
 (RFL:RNE P1 P2)

 (setvar "CMDECHO" CMDECHO)
)
(defun RFL:RNE (P1 P2 / *error* ANGBASE ANGDIR CMDECHO ENT ENTLIST G OFFSET P3
                        S STA STR STRLIST TMP Z)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 0)

 (defun *error* (msg)
  (setvar "CMDECHO" CMDECHO)
  (setvar "ANGBASE" ANGBASE)
  (setvar "ANGDIR" ANGDIR)
  (alert msg)
  (setq *error* nil)
 )

 (setq STRLIST nil
       STA nil
       OFFSET nil
       G nil
       Z nil
 )
 (if (setq STA (RFL:STAOFF P1))
  (setq OFFSET (cadr STA)
        STA (car STA)
        Z (RFL:ELEVATION STA)
        G (RFL:SLOPE STA)
        S (RFL:SUPER STA)
  )
 )
 (if (and (= (cdr (assoc "NE" RFL:RNELIST)) 1)
          P1
     )
  (setq STRLIST (append STRLIST (list (strcat "N " (rtos (cadr P1)) ", E " (rtos (car P1))))))
 )
 (if (and (= (cdr (assoc "SO" RFL:RNELIST)) 1)
          STA
          OFFSET
     )
  (setq STRLIST (append STRLIST (list (strcat "Sta." (RFL:STATXT STA) ", O/S " (rtos OFFSET)))))
 )
 (if (and (= (cdr (assoc "Z" RFL:RNELIST)) 1)
          Z
     )
  (setq STRLIST (append STRLIST (list (strcat "Ctrl Elev " (rtos Z)))))
 )
 (if (and (= (cdr (assoc "G" RFL:RNELIST)) 1)
          G
     )
  (setq STRLIST (append STRLIST (list (strcat "Ctrl grade " (rtos (* 100.0 G)) "%"))))
 )
 (if (and (= (cdr (assoc "SE" RFL:RNELIST)) 1)
          S
     )
  (setq STRLIST (append STRLIST (list (strcat "Ctrl Super: L:"
                                              (rtos (abs (nth 0 S)))
                                              "%"
                                              (if (= (RFL:SIGN (nth 0 S)) 1) " up" " down")
                                              ", R:"
                                              (rtos (abs (nth 1 S)))
                                              "%"
                                              (if (= (RFL:SIGN (nth 1 S)) 1) " up" " down")
                                      )
                                )
                )
  )
 )
 (if STRLIST
  (progn
   (command "LEADER" "_NON" P1 "_NON" P2 "")
   (foreach STR STRLIST
    (command STR)
   )
   (command "")
  )
 )

 (setvar "CMDECHO" CMDECHO)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
)
;
;
;    Program Written by Robert Livingston 00/03/07
;    AARC is a utility for attaching an arc to the end of a line or arc
;
;
(defun C:AARC (/ *error* ANG ANG1 ANG2 ANGBASE ANGDIR CMDECHO DELTA DIR DRAWARC DX DY ENT ENTLIST L OSMODE P P1 P2 PC R R2 TMP)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0.0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 0)
 (setq OSMODE (getvar "OSMODE"))
 (setvar "OSMODE" 0)

 (defun *error* (msg)
  (command "._UCS" "P")
  (setvar "CMDECHO" CMDECHO)
  (setvar "OSMODE" OSMODE)
  (setvar "ANGBASE" ANGBASE)
  (setvar "ANGDIR" ANGDIR)
  (princ msg)
  (setq *error* nil)
 )

 (defun DRAWARC (P1 P2 R / ANG D PC X Y)
  (setq D (distance P1 P2)
        X (/ D 2.0)
        Y (* (RFL:SIGN R) (sqrt (- (* R R) (* X X))))
        ANG (angle P1 P2)
        PC (list (+ (car P1) (* X (cos ANG)) (* -1.0 Y (sin ANG)))
                 (+ (cadr P1) (* X (sin ANG)) (* Y (cos ANG)))
           )
  )
  
  (if (> R 0.0)
   (entmake (list (cons 0 "ARC")
                  (cons 10 PC)
                  (cons 40 (abs R))
                  (cons 50 (angle PC P1))
                  (cons 51 (angle PC P2))
            )
   )
   (entmake (list (cons 0 "ARC")
                  (cons 10 PC)
                  (cons 40 (abs R))
                  (cons 50 (angle PC P2))
                  (cons 51 (angle PC P1))
            )
   )
  )
 )
 
 (command "._UCS" "W")

 (setq ENT (entsel))
 (if (/= ENT nil)
  (progn
   (setq P (nth 1 ENT))
   (setq P (list (nth 0 P) (nth 1 P)))
   (setq ENT (car ENT))
   (setq ENTLIST (entget ENT))
   (setq R nil)
   (if (and (setq R (getdist "\nEnter radius :")) (> R 0.0))
    (progn
     (initget "Left Right")
     (if (= (setq DIR (getkword "\nLeft or Right <Left> : ")) "Right") (setq R (* -1.0 R)))
     (setq DELTA 1 L nil)
     (while (or (= DELTA nil) (= L nil))
      (if (= L nil)
       (progn
        (setq DELTA (getangle "\nEnter DELTA (<return> for L) :"))
        (if (= DELTA nil)
         (progn
          (setq DELTA nil)
          (setq L 1)
         )
         (progn
          (setq DELTA (abs DELTA))
          (setq L (abs (* R DELTA)))
         )
        )
       )
       (progn
        (setq L (getreal "\nEnter L (<return> for DELTA) :"))
        (if (= L nil)
         (progn
          (setq DELTA 1)
          (setq L nil)
         )
         (progn
          (setq L (abs L))
          (setq DELTA (abs (/ L R)))
         )
        )
       )
      )
     )
    )
    (progn
     (princ "\nR must be greater than 0.0!\n")
     (setq R nil)
    )
   )
   (if (/= R nil)
    (if (= (cdr (assoc 0 ENTLIST)) "LINE")
     (progn
      (setq P1 (cdr (assoc 10 ENTLIST)))
      (setq P1 (list (nth 0 P1) (nth 1 P1)))
      (setq P2 (cdr (assoc 11 ENTLIST)))
      (setq P2 (list (nth 0 P2) (nth 1 P2)))
      (if (< (distance P P2) (distance P P1))
       (progn
        (setq TMP P1)
        (setq P1 P2)
        (setq P2 TMP)
       )
      )
      (setq ANG (angle P2 P1))
      (setq DX (* (abs R) (sin DELTA)))
      (setq DY (* R (- 1.0 (cos DELTA))))
      (setq P (list (+ (nth 0 P1) (- (* DX (cos ANG)) (* DY (sin ANG))))
                    (+ (nth 1 P1) (+ (* DX (sin ANG)) (* DY (cos ANG))))
              )
      )
      (DRAWARC P1 P R)
     )
     (progn
      (if (= (cdr (assoc 0 ENTLIST)) "ARC")
       (progn
        (setq PC (cdr (assoc 10 ENTLIST)))
        (setq PC (list (nth 0 PC) (nth 1 PC)))
        (setq R2 (cdr (assoc 40 ENTLIST)))
        (setq ANG1 (cdr (assoc 50 ENTLIST)))
        (setq ANG2 (cdr (assoc 51 ENTLIST)))
        (setq P1 (list (+ (nth 0 PC) (* R2 (cos ANG1)))
                       (+ (nth 1 PC) (* R2 (sin ANG1)))))
        (setq ANG1 (- ANG1 (/ pi 2.0)))
        (setq P2 (list (+ (nth 0 PC) (* R2 (cos ANG2)))
                       (+ (nth 1 PC) (* R2 (sin ANG2)))))
        (setq ANG2 (+ ANG2 (/ pi 2.0)))
        (setq ANG ANG1)
        (if (< (distance P P2) (distance P P1))
         (progn
          (setq TMP P1)
          (setq P1 P2)
          (setq P2 TMP)
          (setq ANG ANG2)
         )
        )
        (setq DX (* (abs R) (sin DELTA)))
        (setq DY (* R (- 1.0 (cos DELTA))))
        (setq P (list (+ (nth 0 P1) (- (* DX (cos ANG)) (* DY (sin ANG))))
                      (+ (nth 1 P1) (+ (* DX (sin ANG)) (* DY (cos ANG))))
                )
        )
        (DRAWARC P1 P R)
       )
       (progn
        (if (/= (setq ENTLIST (RFL:GETSPIRALDATA ENT)) nil)
         (progn
          (setq TMP (nth 0 (RFL:SPIRALSTAOFF P ENT)))
          (if (< (- TMP (nth 3 ENTLIST)) (- (RFL:GETSPIRALLS ENT) TMP))
           (progn
            (setq P1 (RFL:SPIRALXY (list (nth 3 ENTLIST) 0.0) ENT))
            (setq ANG (angle (nth 1 ENTLIST) (nth 0 ENTLIST)))
            (if (> (nth 3 ENTLIST) 0.0)
             (progn
              (if (> (sin (- (angle (nth 1 ENTLIST) (nth 0 ENTLIST)) (angle (nth 2 ENTLIST) (nth 1 ENTLIST)))) 0.0)
               (setq TMP -1.0)
               (setq TMP 1.0)
              )
              (setq ANG (+ ANG
                           (* TMP
                              (expt (nth 3 ENTLIST) 2)
                              (RFL:GETSPIRALTHETA ENT)
                              (/ 1.0 (expt (RFL:GETSPIRALLS ENT) 2))
                           )
                        )
              )
             )
            )
           )
           (progn
            (setq P1 (nth 2 ENTLIST))
            (setq ANG (angle (nth 1 ENTLIST) (nth 2 ENTLIST)))
           )
          )
          (setq DX (* (abs R) (sin DELTA)))
          (setq DY (* R (- 1.0 (cos DELTA))))
          (setq P (list (+ (nth 0 P1) (- (* DX (cos ANG)) (* DY (sin ANG))))
                        (+ (nth 1 P1) (+ (* DX (sin ANG)) (* DY (cos ANG))))
                  )
          )
          (DRAWARC P1 P R)
         )
         (progn
          (princ "\n*** ENTITY NOT SPIRAL/ARC/LINE ***")
         )
        )
       )
      )
     )
    )
   )
  )
 )

 (command "._UCS" "P")
 (setvar "CMDECHO" CMDECHO)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
 (setvar "OSMODE" OSMODE)
 P2
)
;
;
;    Program Written by Robert Livingston 00/03/07
;
;    ALINE is a utility for attaching a line to the end of a line or arc
;
;
(defun C:ALINE (/ *error* ANG ANG1 ANG2 ANGBASE ANGDIR CMDECHO DELTA DRAWLINE
                  DX DY ENT ENTLIST L OSMODE P P1 P2 PC R R2 TMP)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0.0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 0)
 (setq OSMODE (getvar "OSMODE"))
 (setvar "OSMODE" 0)

 (defun *error* (msg)
  (command "._UCS" "P")
  (setvar "CMDECHO" CMDECHO)
  (setvar "OSMODE" OSMODE)
  (setvar "ANGBASE" ANGBASE)
  (setvar "ANGDIR" ANGDIR)
  (princ msg)
  (setq *error* nil)
 )

 (defun DRAWLINE (P1 P2)
  (entmake (list (cons 0 "LINE")
                 (cons 10 P1)
                 (cons 11 P2)
           )
  )
 )
 
 (command "._UCS" "W")

 (setq ENT (entsel))
 (if (/= ENT nil)
  (progn
   (setq P (nth 1 ENT))
   (setq P (list (nth 0 P) (nth 1 P)))
   (setq ENT (car ENT))
   (setq ENTLIST (entget ENT))
   (if (/= (setq L (getdist "\nEnter length :")) nil)
    (progn
     (if (= (cdr (assoc 0 ENTLIST)) "LINE")
      (progn
       (setq P1 (cdr (assoc 10 ENTLIST)))
       (setq P1 (list (nth 0 P1) (nth 1 P1)))
       (setq P2 (cdr (assoc 11 ENTLIST)))
       (setq P2 (list (nth 0 P2) (nth 1 P2)))
       (if (< (distance P P2) (distance P P1))
        (progn
         (setq TMP P1)
         (setq P1 P2)
         (setq P2 TMP)
        )
       )
       (setq ANG (angle P2 P1))
       (setq P (list (+ (nth 0 P1) (+ (* L (cos ANG))))
                     (+ (nth 1 P1) (+ (* L (sin ANG))))
               )
       )
       (DRAWLINE P1 P)
      )
      (progn
       (if (= (cdr (assoc 0 ENTLIST)) "ARC")
        (progn
         (setq PC (cdr (assoc 10 ENTLIST)))
         (setq PC (list (nth 0 PC) (nth 1 PC)))
         (setq R2 (cdr (assoc 40 ENTLIST)))
         (setq ANG1 (cdr (assoc 50 ENTLIST)))
         (setq ANG2 (cdr (assoc 51 ENTLIST)))
         (setq P1 (list (+ (nth 0 PC) (* R2 (cos ANG1)))
                        (+ (nth 1 PC) (* R2 (sin ANG1)))))
         (setq ANG1 (- ANG1 (/ pi 2.0)))
         (setq P2 (list (+ (nth 0 PC) (* R2 (cos ANG2)))
                        (+ (nth 1 PC) (* R2 (sin ANG2)))))
         (setq ANG2 (+ ANG2 (/ pi 2.0)))
         (setq ANG ANG1)
         (if (< (distance P P2) (distance P P1))
          (progn
           (setq TMP P1)
           (setq P1 P2)
           (setq P2 TMP)
           (setq ANG ANG2)
          )
         )
         (setq P (list (+ (nth 0 P1) (+ (* L (cos ANG))))
                       (+ (nth 1 P1) (+ (* L (sin ANG))))
                 )
         )
         (DRAWLINE P1 P)
        )
        (progn
         (if (/= (setq ENTLIST (RFL:GETSPIRALDATA ENT)) nil)
          (progn
           (setq TMP (nth 0 (RFL:SPIRALSTAOFF P ENT)))
           (if (< (- TMP (nth 3 ENTLIST)) (- (RFL:GETSPIRALLS ENT) TMP))
            (progn
             (setq P1 (RFL:SPIRALXY (list (nth 3 ENTLIST) 0.0) ENT))
             (setq ANG (angle (nth 1 ENTLIST) (nth 0 ENTLIST)))
             (if (> (nth 3 ENTLIST) 0.0)
              (progn
               (if (> (sin (- (angle (nth 1 ENTLIST) (nth 0 ENTLIST)) (angle (nth 2 ENTLIST) (nth 1 ENTLIST)))) 0.0)
                (setq TMP -1.0)
                (setq TMP 1.0)
               )
               (setq ANG (+ ANG
                            (* TMP
                               (expt (nth 3 ENTLIST) 2)
                               (RFL:GETSPIRALTHETA ENT)
                               (/ 1.0 (expt (RFL:GETSPIRALLS ENT) 2))
                            )
                         )
               )
              )
             )
            )
            (progn
             (setq P1 (nth 2 ENTLIST))
             (setq ANG (angle (nth 1 ENTLIST) (nth 2 ENTLIST)))
            )
           )
           (setq P (list (+ (nth 0 P1) (* L (cos ANG)))
                         (+ (nth 1 P1) (* L (sin ANG)))))
           (DRAWLINE P1 P)
          )
          (progn
           (princ "\n*** ENTITY NOT SPIRAL/ARC/LINE ***")
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

 (command "._UCS" "P")
 (setvar "CMDECHO" CMDECHO)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
 (setvar "OSMODE" OSMODE)
 (setq P2 P2)
)
;
;
;    Program Written by Robert Livingston 00/03/07
;
;    ASPIRAL is a utility for attaching a spiral
;
;
(defun C:ASPIRAL (/ *error* A ANG ANG1 ANG2 ANGBASE ANGDIR CMDECHO DIR ENT ENTLIST
                    GETR L L0 LR LS MSG OSMODE P P1 P2 PC R R1 R2 SR1 SR2 THETA TMP)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0.0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 0)
 (setq OSMODE (getvar "OSMODE"))
 (setvar "OSMODE" 0)

 (defun *error* (msg)
  (command "._UCS" "P")
  (setvar "CMDECHO" CMDECHO)
  (setvar "OSMODE" OSMODE)
  (setvar "ANGBASE" ANGBASE)
  (setvar "ANGDIR" ANGDIR)
  (princ msg)
  (setq *error* nil)
 )

 (command "._UCS" "W")

 (defun GETR (ENT P / ENTLIST)
  (setq ENTLIST (entget ENT))
  (if (= "LINE" (cdr (assoc 0 ENTLIST)))
   (eval 0.0)
   (if (= "ARC" (cdr (assoc 0 ENTLIST)))
    (eval (cdr (assoc 40 ENTLIST)))
    (if (/= (setq ENTLIST (RFL:GETSPIRALDATA ENT)) nil)
     (if (< (distance P (nth 0 ENTLIST)) (distance P (nth 2 ENTLIST)))
      (if (= 0.0 (last ENTLIST))
       (eval 0.0)
       (/ (* (RFL:GETSPIRALR ENT) (RFL:GETSPIRALLS ENT)) (last ENTLIST))
      )
      (RFL:GETSPIRALR ENT)
     )
     (eval 0.0)
    )
   )
  )
 )

 (setq ENT (entsel))
 (if (/= ENT nil)
  (progn
   (setq P (nth 1 ENT))
   (setq P (list (nth 0 P) (nth 1 P)))
   (setq ENT (car ENT))
   (setq ENTLIST (entget ENT))
   (setq SR1 (getreal (strcat "\nEnter start radius (enter 0 for tangent) <" (rtos (GETR ENT P)) "> :")))
   (if (= SR1 nil) (setq SR1 (GETR ENT P)))
   (if (/= (setq SR2 (getreal "\nEnter end radius (enter 0 for tangent) :")) nil)
    (if (= SR1 SR2)
     (princ "\n*** Both ends can not be same radius! ***")
     (progn
      (setq LS nil)
      (while (= LS nil)
       (if (= AL "L")
        (progn
         (setq LS (getreal "Spiral length <return for A>:"))
         (if (= LS nil)
          (progn
           (setq AL "A")
          )
          (if (or (= 0.0 SR1) (= 0.0 SR2))
           (setq A (sqrt (* LS (max SR1 SR2))))
           (setq A (sqrt (abs (/ (* LS SR1 SR2) (- SR2 SR1)))))
          )
         )
        )
        (progn
         (setq A (getreal "Spiral A <return for length>:"))
         (if (= A nil)
          (progn
           (setq AL "L")
          )
          (if (or (= 0.0 SR1) (= 0.0 SR2))
           (setq LS (/ (* A A) (max SR1 SR2)))
           (setq LS (abs (/ (* A A (- SR2 SR1)) SR1 SR2)))
          )
         )
        )
       )
      )
      (if (or (= 0.0 SR1) (= 0.0 SR2))
       (setq L0 0.0)
       (setq L0 (- (/ (* A A) (min SR1 SR2)) LS))
      )
      (setq LS (+ LS L0))
      (initget 1 "Left Right")
      (setq LR (getkword "\n Left or Right : "))
      (if (= LR "Left")
       (setq DIR -1.0)
       (setq DIR 1.0)
      )
      (setq P1 nil ANG nil)
      (if (= (cdr (assoc 0 ENTLIST)) "LINE")
       (progn
        (setq P1 (cdr (assoc 10 ENTLIST)))
        (setq P1 (list (nth 0 P1) (nth 1 P1)))
        (setq P2 (cdr (assoc 11 ENTLIST)))
        (setq P2 (list (nth 0 P2) (nth 1 P2)))
        (if (< (distance P P2) (distance P P1))
         (progn
          (setq TMP P1)
          (setq P1 P2)
          (setq P2 TMP)
         )
        )
        (setq ANG (angle P2 P1))
       )
       (if (= (cdr (assoc 0 ENTLIST)) "ARC")
        (progn
         (setq PC (cdr (assoc 10 ENTLIST)))
         (setq PC (list (nth 0 PC) (nth 1 PC)))
         (setq R2 (cdr (assoc 40 ENTLIST)))
         (setq ANG1 (cdr (assoc 50 ENTLIST)))
         (setq ANG2 (cdr (assoc 51 ENTLIST)))
         (setq P1 (list (+ (nth 0 PC) (* R2 (cos ANG1)))
                        (+ (nth 1 PC) (* R2 (sin ANG1)))))
         (setq ANG1 (- ANG1 (/ pi 2.0)))
         (setq P2 (list (+ (nth 0 PC) (* R2 (cos ANG2)))
                        (+ (nth 1 PC) (* R2 (sin ANG2)))))
         (setq ANG2 (+ ANG2 (/ pi 2.0)))
         (setq ANG ANG1)
         (if (< (distance P P2) (distance P P1))
          (progn
           (setq TMP P1)
           (setq P1 P2)
           (setq P2 TMP)
           (setq ANG ANG2)
          )
         )
        )
        (if (/= (setq ENTLIST (RFL:GETSPIRALDATA ENT)) nil)
         (progn
          (setq TMP (nth 0 (RFL:SPIRALSTAOFF P ENT)))
          (if (< (- TMP (nth 3 ENTLIST)) (- (RFL:GETSPIRALLS ENT) TMP))
           (progn
            (setq P1 (RFL:SPIRALXY (list (nth 3 ENTLIST) 0.0) ENT))
            (setq ANG (angle (nth 1 ENTLIST) (nth 0 ENTLIST)))
            (if (> (nth 3 ENTLIST) 0.0)
             (progn
              (if (> (sin (- (angle (nth 1 ENTLIST) (nth 0 ENTLIST)) (angle (nth 2 ENTLIST) (nth 1 ENTLIST)))) 0.0)
               (setq TMP -1.0)
               (setq TMP 1.0)
              )
              (setq ANG (+ ANG
                           (* TMP
                              (expt (nth 3 ENTLIST) 2)
                              (RFL:GETSPIRALTHETA ENT)
                              (/ 1.0 (expt (RFL:GETSPIRALLS ENT) 2))
                           )
                        )
              )
             )
            )
           )
           (progn
            (setq P1 (nth 2 ENTLIST))
            (setq ANG (angle (nth 1 ENTLIST) (nth 2 ENTLIST)))
           )
          )
         )
         (progn
          (princ "\n*** ENTITY NOT SPIRAL/ARC/LINE ***")
         )
        )
       )
      )
      (if (/= nil P)
       (if (or (= SR1 0.0) (and (/= SR2 0.0) (> SR1 SR2)))
        (progn
         (if (= L0 0.0)
          (progn
           (setq R (max SR1 SR2))
           (setq THETA (/ LS (* 2.0 R)))
           (setq P P1)
          )
          (progn
           (setq R (min SR1 SR2))
           (setq THETA (/ LS (* 2.0 R)))
           (setq ANG2 (/ L0 (* 2.0 (max SR1 SR2))))
           (setq ANG (+ ANG (* DIR ANG2)))
           (setq P (list (+ (nth 0 P1) (* -1.0 (max SR1 SR2) (RFL:SPIRALFXR ANG2) (cos ANG)) (* -1.0 DIR (max SR1 SR2) (RFL:SPIRALFYR ANG2) (sin ANG)))
                         (+ (nth 1 P1) (* -1.0 (max SR1 SR2) (RFL:SPIRALFXR ANG2) (sin ANG)) (* DIR (max SR1 SR2) (RFL:SPIRALFYR ANG2) (cos ANG)))
                   )
           )
          )
         )
         (setq PLT P)
         (setq PST (list (+ (nth 0 P) (* R (RFL:SPIRALFXR THETA) (cos ANG)) (* DIR R (RFL:SPIRALFYR THETA) (sin ANG)))
                         (+ (nth 1 P) (* R (RFL:SPIRALFXR THETA) (sin ANG)) (* -1.0 DIR R (RFL:SPIRALFYR THETA) (cos ANG)))
                   )
         )
         (setq TMP (- (* R (RFL:SPIRALFXR THETA)) (/ (* R (RFL:SPIRALFYR THETA)) (RFL:TAN THETA))))
         (setq PLTST (list (+ (nth 0 P) (* TMP (cos ANG)))
                           (+ (nth 1 P) (* TMP (sin ANG)))
                     )
         )
         (RFL:DRAWSPIRAL PLT PLTST PST L0 0.0)
        )
        (progn
         (if (= L0 0.0)
          (progn
           (setq R (max SR1 SR2))
           (setq THETA (/ LS (* 2.0 R)))
          )
          (progn
           (setq R (min SR1 SR2))
           (setq THETA (/ LS (* 2.0 R)))
          )
         )
         (setq PST P1)
         (setq TMP (/ (* R (RFL:SPIRALFYR THETA)) (sin THETA)))
         (setq PLTST (list (+ (nth 0 P1) (* TMP (cos ANG)))
                           (+ (nth 1 P1) (* TMP (sin ANG)))
                     )
         )
         (setq TMP (- (* R (RFL:SPIRALFXR THETA)) (/ (* R (RFL:SPIRALFYR THETA)) (RFL:TAN THETA))))
         (setq ANG (+ (* -1.0 DIR THETA) ANG))
         (setq PLT (list (+ (nth 0 PLTST) (* TMP (cos ANG)))
                         (+ (nth 1 PLTST) (* TMP (sin ANG)))
                   )
         )
         (RFL:DRAWSPIRAL PLT PLTST PST L0 0.0)
        )
       )
      )
     )
    )
   )
  )
 )



 (command "._UCS" "P")
 (setvar "CMDECHO" CMDECHO)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
 (setvar "OSMODE" OSMODE)
)
;
;
;     Program written by Robert Livingston, 2008-11-04
;
;     RAB loads hor/vrt/E/OG from a selected RFLALign block
;
;
(defun C:RAB (/ BLKENT BLKENTLIST CMDECHO)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (setq BLKENT (car (entsel "\nSelect RFL Alignment Block : ")))
 (setq BLKENTLIST (entget BLKENT))
 (if (and (= "INSERT" (cdr (assoc 0 BLKENTLIST))) (= "RFLALIGN" (strcase (cdr (assoc 2 BLKENTLIST)))))
  (progn
   (RFL:RALIGNB BLKENT)
   (RFL:RPROFB BLKENT)
   (RFL:RSUPERB BLKENT)
   (RFL:RPROFOGB BLKENT)
  )
  (princ "\n*** NOT AN RFL ALIGNMENT BLOCK ***")
 )

 (setvar "CMDECHO" CMDECHO)
)
;
;
;     Program written by Robert Livingston, 2011/11/22
;
;     RAB2C3D writes the current RFL alignment to a C3D Drawing.
;
;
(defun C:RAB2C3D (/ *error* ALIGNLISTSAVE ALIGNMENTNAME ALIGNMENTSTYLENAME ALIGNMENTLABELSTYLESETNAME BLKENT BLKENTLIST BULGE
                    C CCW CMAX GETOBAECC ID LANDPROFILESTYLENAME LO LS NAME NODE
                    OALIGNMENT OALIGNMENTENTITIES OALIGNMENTSTYLES OALIGNMENTLABELSTYLESETS OALIGNMENTSSITELESS OCIVILAPP ODOCUMENT OLANDPROFILESTYLES OPVIS
                    P1 P2 PLT PLTST PST PREVENT PVILISTSAVE RADIUS SUPERLISTSAVE)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (defun *error* (msg)
  (setvar "CMDECHO" CMDECHO)
  (setq *error* nil)
  (setq RFL:ALIGNLIST ALIGNLISTSAVE)
  (setq RFL:PVILIST PVILISTSAVE)
  (setq RFL:SUPERLIST SUPERLISTSAVE)
  (princ msg)
 )

 (defun GETOBAECC (/ *acad* ACADACTIVEDOCUMENT ACADPROD ACADVER C3DOBJECT C3DDOC C3DSURFS C CMAX c3DSURF)
  (princ "\n")
  (setq ACADPROD (vlax-product-key))
  (setq ACADVER (RFL:ACADVER))
  (setq ACADPROD (strcat "AeccXUiLand.AeccApplication." ACADVER))
  (setq *acad* (vlax-get-acad-object))
  (vla-getinterfaceobject *acad* ACADPROD)
 )

 (defun RADIUS (P1 P2 BULGE / ATOTAL CHORD R)
  (if (listp BULGE)
   (setq R (RFL:GETSPIRALR2 (nth 0 BULGE) (nth 1 BULGE) (nth 2 BULGE)))
   (progn
    (setq ATOTAL (* 4.0 (atan (abs BULGE))))
    (setq CHORD (distance P1 P2))
    (if (< (abs BULGE) TOL)
     (setq R nil)
     (setq R (/ CHORD (* 2 (sin (/ ATOTAL 2)))))
    )
   )
  )
  R
 )

 (command "._UNDO" "M")
 (setq ALIGNLISTSAVE RFL:ALIGNLIST)
 (setq PVILISTSAVE RFL:PVILIST)
 (setq SUPERLISTSAVE RFL:SUPERLIST)

 (setq BLKENT (car (entsel "\nSelect RFL Alignment Block <return to use current alignment> : ")))
 (if (= nil BLKENT)
  (progn
   (setq ALIGNMENTNAME "RFL Alignment")
  )
  (progn
   (setq BLKENTLIST (entget BLKENT))
   (if (and (= "INSERT" (cdr (assoc 0 BLKENTLIST))) (= "RFLALIGN" (strcase (cdr (assoc 2 BLKENTLIST)))))
    (progn
     (RFL:RALIGNB BLKENT)
     (RFL:RPROFB BLKENT)
     (RFL:RSUPERB BLKENT)
     (setq BLKENT (entnext BLKENT))
     (setq BLKENTLIST (entget BLKENT))
     (setq ALIGNMENTNAME "")
     (while (and (= "ATTRIB" (cdr (assoc 0 BLKENTLIST))) (/= "TITLE" (cdr (assoc 2 BLKENTLIST))))
      (setq BLKENT (entnext BLKENT))
      (setq BLKENTLIST (entget BLKENT))
     )
     (if (= "TITLE" (cdr (assoc 2 BLKENTLIST)))
      (setq ALIGNMENTNAME (cdr (assoc 1 BLKENTLIST)))
     )
    )
    (princ "\n*** NOT AN RFL ALIGNMENT BLOCK ***")
   )
  )
 )

 (if (= nil RFL:ALIGNLIST)
  (princ "\n*****  NO RFL ALIGNMENT DEFINED  *****")
  (progn
   (textscr)
   (setq NAME (getstring T (strcat "\nEnter new alignment name <" ALIGNMENTNAME "> : ")))
   (if (= NAME "") (setq NAME ALIGNMENTNAME))
   (if (/= "" NAME)
    (progn
     (setq OCIVILAPP (GETOBAECC))
     (if (= nil OCIVILAPP)
      (alert "C3D not loaded!")
      (progn
       (setq ODOCUMENT (vlax-get-property OCIVILAPP "ActiveDocument"))
       (if (= nil ODOCUMENT)
        (alert "Error getting Document!")
        (progn
         (setq OALIGNMENTSTYLES (vlax-get-property ODOCUMENT "AlignmentStyles"))
         (if (= nil OALIGNMENTSTYLES)
          (alert "Error getting Alignment Styles!")
          (progn
           (setq CMAX (vlax-get-property OALIGNMENTSTYLES "Count"))
           (setq C 0)
           (while (< C CMAX)
            (princ (strcat (itoa (+ C 1)) " : " (vlax-get-property (vlax-get-property OALIGNMENTSTYLES "Item" C) "Name") "\n"))
            (setq C (+ C 1))
           )
           (setq C 0)
           (while (or (< C 1) (> C CMAX))
            (setq C (getint (strcat "Select Alignment Style < 1 to " (itoa CMAX) " > : ")))
           )
           (setq C (- C 1))
           (setq ALIGNMENTSTYLENAME (vlax-get-property (vlax-get-property OALIGNMENTSTYLES "Item" C) "Name"))
           (princ "\n")
           (setq OALIGNMENTLABELSTYLESETS (vlax-get-property ODOCUMENT "AlignmentLabelStyleSets"))
           (if (= nil OALIGNMENTLABELSTYLESETS)
            (alert "Error getting Alignment Label Style Sets!")
            (progn
             (setq CMAX (vlax-get-property OALIGNMENTLABELSTYLESETS "Count"))
             (setq C 0)
             (while (< C CMAX)
              (princ (strcat (itoa (+ C 1)) " : " (vlax-get-property (vlax-get-property OALIGNMENTLABELSTYLESETS "Item" C) "Name") "\n"))
              (setq C (+ C 1))
             )
             (setq C 0)
             (while (or (< C 1) (> C CMAX))
              (setq C (getint (strcat "Select Alignment Label Style Set < 1 to " (itoa CMAX) " > : ")))
             )
             (setq C (- C 1))
             (setq ALIGNMENTLABELSTYLESETNAME (vlax-get-property (vlax-get-property OALIGNMENTLABELSTYLESETS "Item" C) "Name"))
             (setq OALIGNMENTSSITELESS (vlax-get-property ODOCUMENT "AlignmentsSiteless"))
             (if (= nil OALIGNMENTSSITELESS)
              (alert "Error getting Alignments!")
              (progn
               (setq OALIGNMENT (vlax-invoke-method OALIGNMENTSSITELESS "Add" NAME (getvar "CLAYER") ALIGNMENTSTYLENAME ALIGNMENTLABELSTYLESETNAME))
               (if (= nil OALIGNMENT)
                (alert "Error creating new alignment!")
                (progn
                 (vlax-put-property OALIGNMENT "ReferencePointStation" (caar RFL:ALIGNLIST))
                 (setq OALIGNMENTENTITIES (vlax-get-property OALIGNMENT "Entities"))
                 (if (= nil OALIGNMENTENTITIES)
                  (alert "Error accessing entities!")
                  (progn
                   (setq PREVENT nil)
                   (setq C 0)
                   (while (< C (length RFL:ALIGNLIST))
                    (setq NODE (nth C RFL:ALIGNLIST))
                    (setq BULGE (nth 3 NODE))
                    (if (listp BULGE)
                     (progn  ;  Spiral
                      (setq P1 (nth 1 NODE))
                      (setq P2 (nth 2 NODE))
                      (setq PLT (nth 0 BULGE))
                      (setq PLTST (nth 1 BULGE))
                      (setq PST (nth 2 BULGE))
                      (setq LO (nth 3 BULGE))
                      (setq LS (- (RFL:GETSPIRALLS2 PLT PLTST PST) LO))
                      (if (> (distance P2 PLT) (distance P1 PLT))
                       (progn
                        (setq R2 (RADIUS P1 P2 BULGE))
                        (if (< LO RFL:TOLFINE)
                         (setq R1 0.0)
                         (setq R1 (/ (* R2 (RFL:GETSPIRALLS2 PLT PLTST PST)) LO))
                        )
                       )
                       (progn
                        (setq R1 (RADIUS P1 P2 BULGE))
                        (if (< LO RFL:TOLFINE)
                         (setq R2 0.0)
                         (setq R2 (/ (* R1 (RFL:GETSPIRALLS2 PLT PLTST PST)) LO))
                        )
                       )
                      )
                      (if (< (abs LO) RFL:TOLFINE) (setq LO 0.0))
                      (setq PLTST (RFL:GETSPIRALPI2 LO PLT PLTST PST))
                      (setq PT1 (vlax-make-safearray vlax-vbDouble '(0 . 2)))
                      (vlax-safearray-put-element PT1 0 (car P1))
                      (vlax-safearray-put-element PT1 1 (cadr P1))
                      (vlax-safearray-put-element PT1 2 0.0)
                      (setq PT2 (vlax-make-safearray vlax-vbDouble '(0 . 2)))
                      (vlax-safearray-put-element PT2 0 (car PLTST))
                      (vlax-safearray-put-element PT2 1 (cadr PLTST))
                      (vlax-safearray-put-element PT2 2 0.0)
                      (if (> (sin (- (angle PLTST P2) (angle P1 PLTST))) 0.0)
                       (setq CCW 0)
                       (setq CCW T)
                      )
                      (if (= nil PREVENT)
                       (setq ID 0)
                       (setq ID (vlax-get-property PREVENT "Id"))
                      )
                      (if (= 0.0 R1)
                       (setq PREVENT (vlax-invoke-method OALIGNMENTENTITIES "AddFixedSpiral1" ID PT1 PT2 R2 LS 1 CCW 1))
                       (if (= 0.0 R2)
                        (setq PREVENT (vlax-invoke-method OALIGNMENTENTITIES "AddFixedSpiral1" ID PT1 PT2 R1 LS 2 CCW 1))
                        (setq PREVENT (vlax-invoke-method OALIGNMENTENTITIES "AddFixedSpiral2" ID PT1 PT2 R1 R2 LS CCW 1))
                       )
                      )
                     )
                     (if (< (abs BULGE) RFL:TOLFINE)
                      (progn  ;  Tangent
                       (setq PT1 (vlax-make-safearray vlax-vbDouble '(0 . 2)))
                       (vlax-safearray-put-element PT1 0 (car (nth 1 NODE)))
                       (vlax-safearray-put-element PT1 1 (cadr (nth 1 NODE)))
                       (vlax-safearray-put-element PT1 2 0.0)
                       (setq PT2 (vlax-make-safearray vlax-vbDouble '(0 . 2)))
                       (vlax-safearray-put-element PT2 0 (car (nth 2 NODE)))
                       (vlax-safearray-put-element PT2 1 (cadr (nth 2 NODE)))
                       (vlax-safearray-put-element PT2 2 0.0)
                       (setq PREVENT (vlax-invoke-method OALIGNMENTENTITIES "AddFixedLine1" PT1 PT2))
                      )
                      (progn  ;  Arc
                       (setq PT1 (vlax-make-safearray vlax-vbDouble '(0 . 2)))
                       (vlax-safearray-put-element PT1 0 (car (nth 1 NODE)))
                       (vlax-safearray-put-element PT1 1 (cadr (nth 1 NODE)))
                       (vlax-safearray-put-element PT1 2 0.0)
                       (setq PT2 (vlax-make-safearray vlax-vbDouble '(0 . 2)))
                       (vlax-safearray-put-element PT2 0 (car (nth 2 NODE)))
                       (vlax-safearray-put-element PT2 1 (cadr (nth 2 NODE)))
                       (vlax-safearray-put-element PT2 2 0.0)
                       (setq PC (RFL:CENTER (nth 1 NODE) (nth 2 NODE) BULGE))
                       (setq PT3  (vlax-make-safearray vlax-vbDouble '(0 . 2)))
                       (vlax-safearray-put-element PT3 0 (car PC))
                       (vlax-safearray-put-element PT3 1 (cadr PC))
                       (vlax-safearray-put-element PT3 2 0.0)
                       (if (> BULGE 0.0) (setq CCW 0) (setq CCW T))
                       (setq PREVENT (vlax-invoke-method OALIGNMENTENTITIES "AddFixedCurve6" PT1 PT2 (RADIUS (nth 1 NODE) (nth 2 NODE) BULGE) CCW))
                      )
                     )
                    )
                    (setq C (+ C 1))
                   )
                   (if (/= nil RFL:PVILIST)
                    (progn
                     (setq OPROFILES (vlax-get-property OALIGNMENT "Profiles"))
                     (setq OLANDPROFILESTYLES (vlax-get-property ODOCUMENT "LandProfileStyles"))
                     (if (= nil OLANDPROFILESTYLES)
                      (alert "Error getting Profile Styles!")
                      (progn
                       (setq CMAX (vlax-get-property OLANDPROFILESTYLES "Count"))
                       (setq C 0)
                       (while (< C CMAX)
                        (princ (strcat (itoa (+ C 1)) " : " (vlax-get-property (vlax-get-property OLANDPROFILESTYLES "Item" C) "Name") "\n"))
                        (setq C (+ C 1))
                       )
                       (setq C 0)
                       (while (or (< C 1) (> C CMAX))
                        (setq C (getint (strcat "Select Profile Style < 1 to " (itoa CMAX) " > : ")))
                       )
                       (setq C (- C 1))
                       (setq LANDPROFILESTYLENAME (vlax-get-property (vlax-get-property OLANDPROFILESTYLES "Item" C) "Name"))
                       (princ "\n")
                       (setq OPROFILE (vlax-invoke-method OPROFILES "Add" NAME 2 LANDPROFILESTYLENAME))
                       (setq OPVIS (vlax-get-property OPROFILE "PVIs"))
                       (setq NODE (car RFL:PVILIST))
                       (vlax-invoke-method OPVIS "Add" (nth 0 NODE) (nth 1 NODE) 1)
                       (setq NODE (last RFL:PVILIST))
                       (vlax-invoke-method OPVIS "Add" (nth 0 NODE) (nth 1 NODE) 1)
                       (setq C 1)
                       (while (< C (- (length RFL:PVILIST) 1))
                        (setq NODE (nth C RFL:PVILIST))
                        (if (< (nth 3 NODE) RFL:TOLFINE)
                         (vlax-invoke-method OPVIS "Add" (nth 0 NODE) (nth 1 NODE) 1)
                         (vlax-invoke-method OPVIS "Add" (nth 0 NODE) (nth 1 NODE) 3 (nth 3 NODE))
                        )
                        (setq C (+ C 1))
                       )
                      )
                     )
                    )
                   )
;                   (if (/= nil RFL:SUPERLIST)
;                    (progn
;                     (setq OSUPERELEVATIONDATA (vlax-get-property OALIGNMENT "SuperelevationData"))
;                     (setq C 0)
;                     (while (< C (length RFL:SUPERLIST))
;                      (setq NODE (nth C RFL:SUPERLIST))
;                      (setq SUPERDATA (vlax-make-variant vlax-vbArray))
;                      (vlax-invoke-method OSUPERELEVATIONDATA "Add" (car NODE) SUPERDATA)
;                      (setq C (+ C 1))
;                     )
;                    )
;                   )
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
      )
     )
    )
   )
  )
 )

 (graphscr)
 (setq RFL:ALIGNLIST ALIGNLISTSAVE)
 (setq RFL:PVILIST PVILISTSAVE)
 (setq RFL:SUPERLIST SUPERLISTSAVE)
 (eval T)
);
;
;     Program written by Robert Livingston, 2008-11-04
;
;     RABKILL removes alignment definition lists from RFLALIGN blocks
;
;
(defun C:RABKILL (/ ENT)
 (command "._UNDO" "M")
 (setq ENT (car (entsel "\nSelect Alignment Block : ")))
 (RFL:RABKILL ENT "HOR")
 (setq ENT (entlast))
 (RFL:RABKILL ENT "VRT")
 (setq ENT (entlast))
 (RFL:RABKILL ENT "OG")
 (setq ENT (entlast))
 (RFL:RABKILL ENT "E")
)
;
;
;   Program written by Robert Livingston, 2008/11/04
;
;   RALIGNB reads a horizontal alignment from a RFLAlign Block
;
;
(defun C:RALIGNB (/ CMDECHO BLKENT BLKENTLIST)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (setq BLKENT (car (entsel "\nSelect RFL Alignment Block : ")))
 (setq BLKENTLIST (entget BLKENT))
 (if (and (= "INSERT" (cdr (assoc 0 BLKENTLIST))) (= "RFLALIGN" (strcase (cdr (assoc 2 BLKENTLIST)))))
  (RFL:RALIGNB BLKENT)
  (princ "\n*** NOT AN RFL ALIGNMENT BLOCK ***")
 )

 (setvar "CMDECHO" CMDECHO)
)
;
;
;   Program written by Robert Livingston, 2008/11/04
;
;   RALIGNBN reads a horizontal alignment from a nested RFLAlign Block
;
;
(defun C:RALIGNBN (/ CMDECHO BLKENT BLKENTLIST)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (setq BLKENT (car (nentsel "\nSelect nested RFL Alignment Block : ")))
 (setq BLKENT (cdr (assoc 330 (entget BLKENT))))
 (setq BLKENTLIST (entget BLKENT))
 (if (and (= "INSERT" (cdr (assoc 0 BLKENTLIST))) (/= nil (vl-string-search "RFLALIGN" (strcase (cdr (assoc 2 BLKENTLIST))))))
  (RFL:RALIGNB BLKENT)
  (princ "\n*** NOT AN RFL ALIGNMENT BLOCK ***")
 )

 (setvar "CMDECHO" CMDECHO)
)
;
;
;     Program written by Robert Livingston, 10-04-30
;
;     RALIGNC3D reads the alignment from a selected C3D alignment
;     NOTE - Must be using C3D, will not work in straight AutoCAD
;     NOTE - Works for type 1, type 2, type 3 and type 4 alignment entities
;
;
(defun C:RALIGNC3D (/ *error* ALSAVE C CMAX CMDECHO E1 E2 ENT ENTITY ENTLIST NODE
                      OBALIGNMENT OBENTITIES SETARC SETSPIRAL SETTANGENT SPIRALENTITY STA STALIST)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (if (= nil vlax-create-object) (vl-load-com))

 (defun *error* (msg)
  (setvar "CMDECHO" CMDECHO)
  (setq *error* nil)
  (princ msg)
 )

 (defun SETTANGENT (ENTITY / P1 P2)
  (setq P1 (list (vlax-get-property ENTITY "StartEasting")
                 (vlax-get-property ENTITY "StartNorthing")
           )
  )
  (setq P2 (list (vlax-get-property ENTITY "EndEasting")
                 (vlax-get-property ENTITY "EndNorthing")
           )
  )
  (setq RFL:ALIGNLIST (append RFL:ALIGNLIST (list (list (vlax-get-property ENTITY "StartingStation") P1 P2 0.0))))
 )

 (defun SETARC (ENTITY / P1 P2 PC CCW R ANG BULGE)
  (setq P1 (list (vlax-get-property ENTITY "StartEasting")
                 (vlax-get-property ENTITY "StartNorthing")
           )
  )
  (setq P2 (list (vlax-get-property ENTITY "EndEasting")
                 (vlax-get-property ENTITY "EndNorthing")
           )
  )
  (setq PC (list (vlax-get-property ENTITY "CenterEasting")
                 (vlax-get-property ENTITY "CenterNorthing")
           )
  )
  (setq CCW (vlax-get-property ENTITY "Clockwise"))
  (setq R (vlax-get-property ENTITY "Radius"))
  (setq ANG (vlax-get-property ENTITY "Delta"))
  (setq BULGE (TAN (/ ANG 4.0)))
  (if (= :vlax-true CCW) (setq BULGE (* -1.0 BULGE)))
  (setq RFL:ALIGNLIST (append RFL:ALIGNLIST (list (list (vlax-get-property ENTITY "StartingStation") P1 P2 BULGE))))
 )

 (defun SETSPIRAL (ENTITY / A ANG BULGE L0 LT P1 P2 PLTST PINT PST RIN ROUT ST THETA TMP X Y)
  (setq RIN (vlax-get-property ENTITY "RadiusIn"))
  (setq ROUT (vlax-get-property ENTITY "RadiusOut"))
  (setq TMP (/ 1.0 (max RIN ROUT)))
  ;(setq TMP (vlax-get-property ENTITY "Compound"))
  ;(if (= TMP :vlax-false)
  (if (< TMP RFL:TOL)
   (progn
    (setq P1 (list (vlax-get-property ENTITY "StartEasting")
                   (vlax-get-property ENTITY "StartNorthing")
             )
    )
    (setq P2 (list (vlax-get-property ENTITY "EndEasting")
                   (vlax-get-property ENTITY "EndNorthing")
             )
    )
    (setq PLTST (list (vlax-get-property ENTITY "SPIEasting")
                      (vlax-get-property ENTITY "SPINorthing")
                )
    )
    (setq LO 0.0)
    (if (< (distance P2 PLTST) (distance P1 PLTST))
     (setq PLT P1 PST P2)
     (setq PLT P2 PST P1)
    )
    (setq BULGE (list PLT PLTST PST LO))
    (setq RFL:ALIGNLIST (append RFL:ALIGNLIST (list (list (vlax-get-property ENTITY "StartingStation") P1 P2 BULGE))))
   )
   (progn
    (setq P1 (list (vlax-get-property ENTITY "StartEasting")
                   (vlax-get-property ENTITY "StartNorthing")
             )
    )
    (setq P2 (list (vlax-get-property ENTITY "EndEasting")
                   (vlax-get-property ENTITY "EndNorthing")
             )
    )
    (setq PINT (list (vlax-get-property ENTITY "SPIEasting")
                     (vlax-get-property ENTITY "SPINorthing")
               )
    )
    (setq RIN (vlax-get-property ENTITY "RadiusIn"))
    (setq ROUT (vlax-get-property ENTITY "RadiusOut"))
    (setq A (vlax-get-property ENTITY "A"))
    (if (< RIN ROUT)
     (progn
      (setq THETA (/ (* A A) (* 2.0 RIN RIN)))
      (setq PST P1)
      ;(setq LO (- (/ (* A A) RIN) (vlax-get-property ENTITY "Length")))
      (setq LO (/ (* A A) ROUT))
      (setq ANG (angle PST PINT))
      (setq X (* RIN (SPIRALFXR THETA)))
      (setq Y (* RIN (SPIRALFYR THETA)))
      (setq ST (/ Y (sin THETA)))
      (setq LT (- X (/ Y (TAN THETA))))
      (setq PLTST (list (+ (car PST) (* ST (cos ANG)))
                        (+ (cadr PST) (* ST (sin ANG)))))
      (if (> (sin (- (angle PINT P2) (angle P1 PINT))) 0.0)
       (setq ANG (+ ANG THETA))
       (setq ANG (- ANG THETA))
      )
      (setq PLT (list (+ (car PLTST) (* LT (cos ANG)))
                      (+ (cadr PLTST) (* LT (sin ANG)))))
     )
     (progn
      (setq THETA (/ (* A A) (* 2.0 ROUT ROUT)))
      (setq PST P2)
      ;(setq LO (- (/ (* A A) ROUT) (vlax-get-property ENTITY "Length")))
      (setq LO (/ (* A A) RIN))
      (setq ANG (angle PST PINT))
      (setq X (* ROUT (SPIRALFXR THETA)))
      (setq Y (* ROUT (SPIRALFYR THETA)))
      (setq ST (/ Y (sin THETA)))
      (setq LT (- X (/ Y (TAN THETA))))
      (setq PLTST (list (+ (car PST) (* ST (cos ANG)))
                        (+ (cadr PST) (* ST (sin ANG)))))
      (if (> (sin (- (angle PINT P1) (angle P2 PINT))) 0.0)
       (setq ANG (+ ANG THETA))
       (setq ANG (- ANG THETA))
      )
      (setq PLT (list (+ (car PLTST) (* LT (cos ANG)))
                      (+ (cadr PLTST) (* LT (sin ANG)))))
     )
    )
    (setq BULGE (list PLT PLTST PST LO))
    (setq RFL:ALIGNLIST (append RFL:ALIGNLIST (list (list (vlax-get-property ENTITY "StartingStation") P1 P2 BULGE))))
   )
  )
 )

 (setq OBALIGNMENT (RFL:GETC3DALIGNMENT))
 (if (/= OBALIGNMENT nil) 
  (progn
   (setq RFL:ALIGNLIST nil)
   (setq OBENTITIES (vlax-get-property OBALIGNMENT "Entities"))
   (setq CMAX (vlax-get-property OBENTITIES "Count"))
   (setq C 0)
   (while (< C CMAX)
    (setq ENTITY (vlax-invoke-method OBENTITIES "Item" C))
    (cond
     ((= 1 (vlax-get-property ENTITY "Type"))
      (progn
       (SETTANGENT ENTITY)
      )
     )
     ((= 2 (vlax-get-property ENTITY "Type"))
      (progn
       (SETARC ENTITY)
      )
     )
     ((= 3 (vlax-get-property ENTITY "Type"))
      (progn
       (SETSPIRAL ENTITY)
      )
     )
     ((= 4 (vlax-get-property ENTITY "Type"))
      (progn
       (SETSPIRAL (vlax-get-property ENTITY "SpiralIn"))
       (SETARC (vlax-get-property ENTITY "Arc"))
       (SETSPIRAL (vlax-get-property ENTITY "SpiralOut"))
      )
     )
    )
    (setq C (1+ C))
   )
  )
 )

 (setq ALSAVE RFL:ALIGNLIST RFL:ALIGNLIST nil)
 (if (/= nil ALSAVE)
  (progn
   (setq STALIST nil)
   (foreach NODE ALSAVE
    (setq STALIST (append STALIST (list (car NODE))))
   )
   (setq STALIST (vl-sort STALIST '<))
   (foreach STA STALIST
    (setq RFL:ALIGNLIST (append RFL:ALIGNLIST (list (assoc STA ALSAVE))))
   )
  )
 )
 
 (setvar "CMDECHO" CMDECHO)
);
;
;   Program written by Robert Livingston, 2008-11-04
;
;   RPROFB reads a vertical profile from a RFLAlign Block
;
;
(defun C:RPROFB (/ CMDECHO BLKENT BLKENTLIST)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (setq BLKENT (car (entsel "\nSelect RFL Alignment Block : ")))
 (setq BLKENTLIST (entget BLKENT))
 (if (and (= "INSERT" (cdr (assoc 0 BLKENTLIST))) (= "RFLALIGN" (strcase (cdr (assoc 2 BLKENTLIST)))))
  (RFL:RPROFB BLKENT)
  (princ "\n*** NOT AN RFL ALIGNMENT BLOCK ***")
 )

 (setvar "CMDECHO" CMDECHO)
)
;
;
;   Program written by Robert Livingston, 2008-11-04
;
;   RPROFBN reads a vertical profile from a nested RFLAlign Block
;
;
(defun C:RPROFBN (/ CMDECHO BLKENT BLKENTLIST)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (setq BLKENT (car (nentsel "\nSelect nested RFL Alignment Block : ")))
 (setq BLKENT (cdr (assoc 330 (entget BLKENT))))
 (setq BLKENTLIST (entget BLKENT))
 (if (and (= "INSERT" (cdr (assoc 0 BLKENTLIST))) (/= nil (vl-string-search "RFLALIGN" (strcase (cdr (assoc 2 BLKENTLIST))))))
  (RFL:RPROFB BLKENT)
  (princ "\n*** NOT AN RFL ALIGNMENT BLOCK ***")
 )

 (setvar "CMDECHO" CMDECHO)
)
;
;
;     Program written by Robert Livingston, 11-03-09
;
;     RPROF3D reads the profile from a selected C3D profile
;     NOTE - Must be using C3D, will not work in straight AutoCAD
;     NOTE - Works for type 1 and type 3 vertical curves
;
;
(defun C:RPROFC3D (/ *error* ENT)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (defun *error* (msg)
  (setvar "CMDECHO" CMDECHO)
  (setq *error* nil)
  (princ msg)
 )

 (if (setq ENT (car (entsel "\nSelect C3D profile : ")))
  (RFL:RPROFC3D ENT)
 )

 (setvar "CMDECHO" CMDECHO)
 T
);
;
;   Program written by Robert Livingston, 2008-11-04
;
;   RPROFOGB reads a vertical OG profile from a RFLAlign Block
;
;
(defun C:RPROFOGB (/ CMDECHO BLKENT BLKENTLIST)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (setq BLKENT (car (entsel "\nSelect RFL Alignment Block : ")))
 (setq BLKENTLIST (entget BLKENT))
 (if (and (= "INSERT" (cdr (assoc 0 BLKENTLIST))) (= "RFLALIGN" (strcase (cdr (assoc 2 BLKENTLIST)))))
  (RFL:RPROFOGB BLKENT)
  (princ "\n*** NOT AN RFL ALIGNMENT BLOCK ***")
 )

 (setvar "CMDECHO" CMDECHO)
)
;
;
;   Program written by Robert Livingston, 2008-11-04
;
;   RPROFOGBN reads a vertical OG profile from a nested RFLAlign Block
;
;
(defun C:RPROFOGBN (/ CMDECHO BLKENT BLKENTLIST)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (setq BLKENT (car (nentsel "\nSelect nested RFL Alignment Block : ")))
 (setq BLKENT (cdr (assoc 330 (entget BLKENT))))
 (setq BLKENTLIST (entget BLKENT))
 (if (and (= "INSERT" (cdr (assoc 0 BLKENTLIST))) (/= nil (vl-string-search "RFLALIGN" (strcase (cdr (assoc 2 BLKENTLIST))))))
  (RFL:RPROFOGB BLKENT)
  (princ "\n*** NOT AN RFL ALIGNMENT BLOCK ***")
 )

 (setvar "CMDECHO" CMDECHO)
)
;
;
;   Program written by Robert Livingston, 2008/11/04
;
;   WALIGNB writes a horizontal alignment to a RFLALIGN Block
;
;
(defun C:WALIGNB (/ CMDECHO BLKENT BLKENTLIST ENT ENTLIST)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (setq BLKENT (car (entsel "\nSelect RFL Alignment Block : ")))
 (setq BLKENTLIST (entget BLKENT))
 (if (and (= "INSERT" (cdr (assoc 0 BLKENTLIST))) (= "RFLALIGN" (strcase (cdr (assoc 2 BLKENTLIST)))))
  (if (= nil RFL:ALIGNLIST)
   (RFL:RABKILL BLKENT "HOR")
   (RFL:WALIGNB BLKENT)
  )
  (princ "\n*** NOT AN RFL ALIGNMENT BLOCK ***")
 )

 (setvar "CMDECHO" CMDECHO)
)
;
;
;   Program written by Robert Livingston, 2008/11/04
;
;   WPROFB writes a vertical alinment to a RFLALIGN Block
;
;
(defun C:WPROFB (/ CMDECHO BLKENT BLKENTLIST ENT ENTLIST)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (setq BLKENT (car (entsel "\nSelect RFL Alignment Block : ")))
 (setq BLKENTLIST (entget BLKENT))
 (if (and (= "INSERT" (cdr (assoc 0 BLKENTLIST))) (= "RFLALIGN" (strcase (cdr (assoc 2 BLKENTLIST)))))
  (if (= nil RFL:PVILIST)
   (RFL:RABKILL BLKENT "VRT")
   (RFL:WPROFB BLKENT)
  )
  (princ "\n*** NOT AN RFL ALIGNMENT BLOCK ***")
 )

 (setvar "CMDECHO" CMDECHO)
)
;
;
;   Program written by Robert Livingston, 2008/11/04
;
;   WPROFOGB writes a OG vertical alinment to a RFLALIGN Block
;
;
(defun C:WPROFOGB (/ CMDECHO BLKENT BLKENTLIST ENT ENTLIST)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (setq BLKENT (car (entsel "\nSelect RFL Alignment Block : ")))
 (setq BLKENTLIST (entget BLKENT))
 (if (and (= "INSERT" (cdr (assoc 0 BLKENTLIST))) (= "RFLALIGN" (strcase (cdr (assoc 2 BLKENTLIST)))))
  (if (= nil RFL:OGLIST)
   (RFL:RABKILL BLKENT "OG")
   (RFL:WPROFOGB BLKENT)
  )
  (princ "\n*** NOT AN RFL ALIGNMENT BLOCK ***")
 )

 (setvar "CMDECHO" CMDECHO)
)
;
;
;   Program written by Robert Livingston, 2008/11/04
;
;   WSUPERB writes the superelevation to a RFLALIGN Block
;
;
(defun C:WSUPERB (/ CMDECHO BLKENT BLKENTLIST ENT ENTLIST)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (setq BLKENT (car (entsel "\nSelect RFL Alignment Block : ")))
 (setq BLKENTLIST (entget BLKENT))
 (if (and (= "INSERT" (cdr (assoc 0 BLKENTLIST))) (= "RFLALIGN" (strcase (cdr (assoc 2 BLKENTLIST)))))
  (if (= nil RFL:SUPERLIST)
   (RFL:RABKILL BLKENT "E")
   (RFL:WSUPERB BLKENT)
  )
  (princ "\n*** NOT AN RFL ALIGNMENT BLOCK ***")
 )

 (setvar "CMDECHO" CMDECHO)
)
;
;
;   Program written by Robert Livingston, 2008-11-04
;
;   RSUPERB reads the Superelevation from a RFLAlign Block
;
;
(defun C:RSUPERB (/ CMDECHO BLKENT BLKENTLIST)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (setq BLKENT (car (entsel "\nSelect RFL Alignment Block : ")))
 (setq BLKENTLIST (entget BLKENT))
 (if (and (= "INSERT" (cdr (assoc 0 BLKENTLIST))) (= "RFLALIGN" (strcase (cdr (assoc 2 BLKENTLIST)))))
  (RFL:RSUPERB BLKENT)
  (princ "\n*** NOT AN RFL ALIGNMENT BLOCK ***")
 )

 (setvar "CMDECHO" CMDECHO)
)
;
;
;   Program written by Robert Livingston, 2008-11-04
;
;   RSUPERBN reads the Superelevation from a nested RFLAlign Block
;
;
(defun C:RSUPERBN (/ CMDECHO BLKENT BLKENTLIST)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (setq BLKENT (car (nentsel "\nSelect nested RFL Alignment Block : ")))
 (setq BLKENT (cdr (assoc 330 (entget BLKENT))))
 (setq BLKENTLIST (entget BLKENT))
 (if (and (= "INSERT" (cdr (assoc 0 BLKENTLIST))) (/= nil (vl-string-search "RFLALIGN" (strcase (cdr (assoc 2 BLKENTLIST))))))
  (RFL:RSUPERB BLKENT)
  (princ "\n*** NOT AN RFL ALIGNMENT BLOCK ***")
 )

 (setvar "CMDECHO" CMDECHO)
)
;
;
;     Program written by Robert Livingston, 2008-11-04
;
;     RAB loads hor/vrt/E/OG from a selected nested RFLALign block
;
;
(defun C:RABN (/ BLKENT BLKENTLIST CMDECHO)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (setq BLKENT (car (nentsel "\nSelect nested RFL Alignment Block : ")))
 (setq BLKENT (cdr (assoc 330 (entget BLKENT))))
 (setq BLKENTLIST (entget BLKENT))
 (if (and (= "INSERT" (cdr (assoc 0 BLKENTLIST))) (/= nil (vl-string-search "RFLALIGN" (strcase (cdr (assoc 2 BLKENTLIST))))))
  (progn
   (RFL:RALIGNB BLKENT)
   (RFL:RPROFB BLKENT)
   (RFL:RSUPERB BLKENT)
   (RFL:RPROFOGB BLKENT)
  )
  (princ "\n*** NOT AN RFL ALIGNMENT BLOCK ***")
 )

 (setvar "CMDECHO" CMDECHO)
)
;
;
;     Program written by Robert Livingston, 2015-03-13
;
;     FIX+ modifies a text entity to adjust it's '+' to align with its insertion point.
;
;
(defun C:FIX+ (/ *error* ANGBASE ANGDIR ATTREQ CMDECHO ENT ORTHOMODE OSMODE P P1 TB TBL TBR W WL WR W+)
 (setq ATTREQ (getvar "ATTREQ"))
 (setvar "ATTREQ" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0.0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 1)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq OSMODE (getvar "OSMODE"))
 (setvar "OSMODE" 0)
 (setq ORTHOMODE (getvar "ORTHOMODE"))
 (setvar "ORTHOMODE" 0)

 (defun *error* (msg)
  (setvar "ATTREQ" ATTREQ)
  (setvar "ANGBASE" ANGBASE)
  (setvar "ANGDIR" ANGDIR)
  (setvar "CMDECHO" CMDECHO)
  (setvar "OSMODE" OSMODE)
  (setvar "ORTHOMODE" ORTHOMODE)
  (print msg)
 )

 (while (/= nil (setq ENT (car (entsel))))
  (RFL:FIX+ ENT)
 )
 
 (setvar "ATTREQ" ATTREQ)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
 (setvar "CMDECHO" CMDECHO)
 (setvar "OSMODE" OSMODE)
 (setvar "ORTHOMODE" ORTHOMODE)
)
;
;
;     Program written by Robert Livingston, 02/05/08
;
;     VCPTAN draws a tangent at the specified point on a vertical curve
;
(defun C:VCPTAN (/ *error* ANGBASE ANGDIR A B C CMDECHO ENT ENTLIST G G1 G2 L OSMODE P P1 P2 P3 S X X1 X2 X3 Y Y1 Y2 Y3)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq OSMODE (getvar "OSMODE"))
 (setvar "OSMODE" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0.0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 0)

 (defun *error* (msg)
  (command "._UCS" "P")
  (setvar "ANGBASE" ANGBASE)
  (setvar "ANGDIR" ANGDIR)
  (setvar "CMDECHO" CMDECHO)
  (setvar "OSMODE" OSMODE)
  (setq *error* nil)
  (print msg)
 )

 (command "._UCS" "W")

 (command "._UNDO" "M")

 (setq ENT (car (entsel "\nSelect vertical curve :")))
 (if (/= nil ENT)
  (progn
   (setq ENTLIST (entget ENT))
   (if (/= "POLYLINE" (cdr (assoc 0 ENTLIST)))
    (princ "\n*** Entity not a polyline ***")
    (progn
     (setq ENT (entnext ENT))
     (setq ENTLIST (entget ENT))
     (setq P1 (cdr (assoc 10 ENTLIST)))
     (setq ENT (entnext ENT))
     (setq ENTLIST (entget ENT))
     (setq P2 (cdr (assoc 10 ENTLIST)))
     (if (/= nil P2)
      (progn
       (setq ENT (entnext ENT))
       (setq ENTLIST (entget ENT))
       (setq P3 (cdr (assoc 10 ENTLIST)))
       (if (/= nil P3)
        (progn
         (setq ENT (entnext ENT))
         (setq ENTLIST (entget ENT))
         (while (/= "SEQEND" (cdr (assoc 0 ENTLIST)))
          (setq P2 P3)
          (setq P3 (cdr (assoc 10 ENTLIST)))
          (setq ENT (entnext ENT))
          (setq ENTLIST (entget ENT))
         )
        )
       )
      )
     )
     (if (and (/= nil P1) (/= nil P2) (/= nil P3))
      (progn
       (setq VEXAG (getreal (strcat "\nEnter vertical exageration (" (rtos 10.0) ") : ")))
       (if (= nil VEXAG) (setq VEXAG 10.0))
       (setq X1 (nth 0 P1))
       (setq Y1 (/ (nth 1 P1) VEXAG))
       (setq X2 (nth 0 P2))
       (setq Y2 (/ (nth 1 P2) VEXAG))
       (setq X3 (nth 0 P3))
       (setq Y3 (/ (nth 1 P3) VEXAG))
       (setq G1 (/ (- Y2 Y1) (- X2 X1)))
       (setq G2 (/ (- Y3 Y2) (- X3 X2)))
       (setq P (getpoint (strcat "\nG1 = " (rtos (* G1 100.0)) ", G2 = " (rtos (* G2 100.0)) ", enter point : ")))
       (if (/= P nil)
        (progn
         (setq A (/ (- G2 G1) (- X3 X1) 2.0))
         (setq B (/ (- G2 (* G1 (/ X3 X1))) (- 1.0 (/ X3 X1))))
         (setq C (- Y1 (+ (* A X1 X1) (* B X1))))
         (setq X (nth 0 P))
         (setq Y (+ (* A X X) (* B X) C))
         (setq G (+ (* 2.0 A X) B))
         (setq L (getdist (strcat "\nEnter length (" (rtos (abs (- X3 X1))) ") : ")))
         (if (= nil L) (setq L (abs (- X3 X1))))
         (setq P1 (list (- X (/ L 2.0))
                        (* (- Y (* (/ L 2.0) G)) VEXAG)))
         (setq P2 (list (+ X (/ L 2.0))
                        (* (+ Y (* (/ L 2.0) G)) VEXAG)))
         (command "._LINE" P1 P2 "")
        )
       )
      )
     )
    )
   )
  )
 )

 (command "._UCS" "P")
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
 (setvar "CMDECHO" CMDECHO)
 (setvar "OSMODE" OSMODE)
);
;
;     Program written by Robert Livingston, 0/12/18
;
;     VC2TAN draws a line tangent to two vertical curves
;
(defun C:VC2TAN (/ A1 A2 B1 B2 C1 C2 ENT ENTLIST F G GA GB G1 G2 P P1 P2 P3 SIGN V VA VB X XA XB X1 X2 Y YA YB Y1 Y2 TMP)

 (defun SIGN (X)
  (if (< X 0.0)
   (eval -1)
   (eval 1)
  )
 )

 (defun F (X Y / SA SB SC X1A X1B Y1A Y1B)
  (setq SA A2)
  (setq SB (* -2.0 A2 X))
  (setq SC (- Y C2 (* B2 X)))
  (setq X1A (/ (+ (* -1.0 SB) (sqrt (- (* SB SB) (* 4.0 SA SC)))) (* 2.0 SA)))
  (setq Y1A (+ (* A2 X1A X1A) (* B2 X1A) C2))
  (setq X1B (/ (- (* -1.0 SB) (sqrt (- (* SB SB) (* 4.0 SA SC)))) (* 2.0 SA)))
  (setq Y1B (+ (* A2 X1B X1B) (* B2 X1B) C2))
  (if (> X1A X1B)
   (list X1A Y1A)
   (list X1B Y1B)
  )
 )

 (setq ENT (car (entsel "\nSelect 'from' vertical curve :")))
 (if (/= nil ENT)
  (progn
   (setq ENTLIST (entget ENT))
   (if (= "POLYLINE" (cdr (assoc 0 ENTLIST)))
    (progn
     (setq ENT (entnext ENT))
     (setq ENTLIST (entget ENT))
     (setq P1 (cdr (assoc 10 ENTLIST)))
     (setq ENT (entnext ENT))
     (setq ENTLIST (entget ENT))
     (setq P2 (cdr (assoc 10 ENTLIST)))
     (if (/= nil P2)
      (progn
       (setq ENT (entnext ENT))
       (setq ENTLIST (entget ENT))
       (setq P3 (cdr (assoc 10 ENTLIST)))
       (if (/= nil P3)
        (progn
         (setq ENT (entnext ENT))
         (setq ENTLIST (entget ENT))
         (while (/= "SEQEND" (cdr (assoc 0 ENTLIST)))
          (setq P2 P3)
          (setq P3 (cdr (assoc 10 ENTLIST)))
          (setq ENT (entnext ENT))
          (setq ENTLIST (entget ENT))
         )
        )
       )
      )
     )
     (if (and (/= nil P1) (/= nil P2) (/= nil P3))
      (progn
       (setq X1 (nth 0 P1))
       (setq Y1 (nth 1 P1))
       (setq X2 (nth 0 P2))
       (setq Y2 (nth 1 P2))
       (setq X3 (nth 0 P3))
       (setq Y3 (nth 1 P3))
       (setq XA (min X1 X3))
       (setq XB (max X1 X3))
       (setq G1 (/ (- Y2 Y1) (- X2 X1)))
       (setq G2 (/ (- Y3 Y2) (- X3 X2)))
       (setq A1 (/ (- G2 G1) (- X3 X1) 2.0))
       (setq B1 (/ (- G2 (* G1 (/ X3 X1))) (- 1.0 (/ X3 X1))))
       (setq C1 (- Y1 (+ (* A1 X1 X1) (* B1 X1))))
       (setq ENT (car (entsel "\nSelect 'to' vertical curve :")))
       (if (/= nil ENT)
        (progn
         (setq ENTLIST (entget ENT))
         (if (= "POLYLINE" (cdr (assoc 0 ENTLIST)))
          (progn
           (setq ENT (entnext ENT))
           (setq ENTLIST (entget ENT))
           (setq P1 (cdr (assoc 10 ENTLIST)))
           (setq ENT (entnext ENT))
           (setq ENTLIST (entget ENT))
           (setq P2 (cdr (assoc 10 ENTLIST)))
           (if (/= nil P2)
            (progn
             (setq ENT (entnext ENT))
             (setq ENTLIST (entget ENT))
             (setq P3 (cdr (assoc 10 ENTLIST)))
             (if (/= nil P3)
              (progn
               (setq ENT (entnext ENT))
               (setq ENTLIST (entget ENT))
               (while (/= "SEQEND" (cdr (assoc 0 ENTLIST)))
                (setq P2 P3)
                (setq P3 (cdr (assoc 10 ENTLIST)))
                (setq ENT (entnext ENT))
                (setq ENTLIST (entget ENT))
               )
              )
             )
            )
           )
           (if (and (/= nil P1) (/= nil P2) (/= nil P3))
            (progn
             (setq X1 (nth 0 P1))
             (setq Y1 (nth 1 P1))
             (setq X2 (nth 0 P2))
             (setq Y2 (nth 1 P2))
             (setq X3 (nth 0 P3))
             (setq Y3 (nth 1 P3))
             (setq G1 (/ (- Y2 Y1) (- X2 X1)))
             (setq G2 (/ (- Y3 Y2) (- X3 X2)))
             (setq A2 (/ (- G2 G1) (- X3 X1) 2.0))
             (setq B2 (/ (- G2 (* G1 (/ X3 X1))) (- 1.0 (/ X3 X1))))
             (setq C2 (- Y1 (+ (* A2 X1 X1) (* B2 X1))))
             (setq YA (+ (* A1 XA XA) (* B1 XA) C1))
             (setq GA (+ (* 2.0 A1 XA) B1))
             (setq VA (F XA YA))
             (setq VA (- GA (/ (- (nth 1 VA) YA) (- (nth 0 VA) XA))))
             (setq TMP (/ (- XB XA) 25.0))
             (setq X (+ XB TMP))
             (setq XB (+ XA TMP))
             (setq YB (+ (* A1 XB XB) (* B1 XB) C1))
             (setq GB (+ (* 2.0 A1 XB) B1))
             (setq VB (F XB YB))
             (setq VB (- GB (/ (- (nth 1 VB) YB) (- (nth 0 VB) XB))))
             (while (and (= (SIGN VA) (SIGN VB)) (< XB X))
              (setq XA XB)
              (setq YA YB)
              (setq GA GB)
              (setq VA VB)
              (setq XB (+ XB TMP))
              (setq YB (+ (* A1 XB XB) (* B1 XB) C1))
              (setq GB (+ (* 2.0 A1 XB) B1))
              (setq VB (F XB YB))
              (setq VB (- GB (/ (- (nth 1 VB) YB) (- (nth 0 VB) XB))))
             )
             (setq X (/ (+ XA XB) 2.0))
             (setq Y (+ (* A1 X X) (* B1 X) C1))
             (setq G (+ (* 2.0 A1 X) B1))
             (setq V (F X Y))
             (setq V (- G (/ (- (nth 1 V) Y) (- (nth 0 V) X))))
             (if (= (SIGN VA) (SIGN VB))
              (princ "\n*** No solution found ***")
              (progn
               (setq TMP 0)
               (while (and (/= XA XB) (< TMP 1000))
                (if (< (* (SIGN V) (SIGN VB)) 0.0)
                 (progn
                  (setq XA X)
                  (setq YA Y)
                  (setq VA V)
                 )
                 (progn
                  (setq XB X)
                  (setq YB Y)
                  (setq VB V)
                 )
                )
                (setq X (/ (+ XA XB) 2.0))
                (setq Y (+ (* A1 X X) (* B1 X) C1))
                (setq G (+ (* 2.0 A1 X) B1))
                (setq V (F X Y))
                (setq V (- G (/ (- (nth 1 V) Y) (- (nth 0 V) X))))
                (setq TMP (+ TMP 1))
               )
               (command "._LINE" "_NON" (list X Y) "_NON" (F X Y) "")
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
  )
 )
);
;
;     Program written by Robert Livingston, 0/12/18
;
;     VCTAN returns the tangent from a point to a vertical curve
;
(defun C:VCTAN (/ A B C ENT ENTLIST G1 G2 P P1 P2 P3 PE RFL:PVILIST SA SB SC X X0A X0B X1 X2 X3 Y Y0A Y0B Y1 Y2 Y3 TMP)
 (setq P (getvar "LASTPOINT"))
 (setq X (nth 0 P))
 (setq Y (nth 1 P))
 (setq ENT (entsel "\nSelect vertical curve :"))
 (setq PE (cadr ENT))
 (setq ENT (car ENT))
 (if (and (/= nil ENT) (/= nil P))
  (progn
   (setq P1 nil P2 nil P3 nil)
   (setq ENTLIST (entget ENT))
   (if (= "POLYLINE" (cdr (assoc 0 ENTLIST)))
    (progn
     (setq ENT (entnext ENT))
     (setq ENTLIST (entget ENT))
     (setq P1 (cdr (assoc 10 ENTLIST)))
     (setq ENT (entnext ENT))
     (setq ENTLIST (entget ENT))
     (setq P2 (cdr (assoc 10 ENTLIST)))
     (if (/= nil P2)
      (progn
       (setq ENT (entnext ENT))
       (setq ENTLIST (entget ENT))
       (setq P3 (cdr (assoc 10 ENTLIST)))
       (if (/= nil P3)
        (progn
         (setq ENT (entnext ENT))
         (setq ENTLIST (entget ENT))
         (while (/= "SEQEND" (cdr (assoc 0 ENTLIST)))
          (setq P2 P3)
          (setq P3 (cdr (assoc 10 ENTLIST)))
          (setq ENT (entnext ENT))
          (setq ENTLIST (entget ENT))
         )
        )
       )
      )
     )
    )
   )
   (if (= "AECC_PROFILE" (cdr (assoc 0 ENTLIST)))
    (progn
     (setq RFL:PVILIST nil)
     (RFL:RPROFC3D ENT)
     (if (/= RFL:PVILIST nil)
      (progn
       (setq C 0)
       (while (< C (length RFL:PVILIST))
        
        
        (setq C (1+ C))
       )
      )
     )
    )
   )
   (if (and (/= nil P1) (/= nil P2) (/= nil P3))
    (progn
     (setq X1 (nth 0 P1))
     (setq Y1 (nth 1 P1))
     (setq X2 (nth 0 P2))
     (setq Y2 (nth 1 P2))
     (setq X3 (nth 0 P3))
     (setq Y3 (nth 1 P3))
     (setq G1 (/ (- Y2 Y1) (- X2 X1)))
     (setq G2 (/ (- Y3 Y2) (- X3 X2)))
     (setq A (/ (- G2 G1) (- X3 X1) 2.0))
     (setq B (/ (- G2 (* G1 (/ X3 X1))) (- 1.0 (/ X3 X1))))
     (setq C (- Y1 (+ (* A X1 X1) (* B X1))))
     (setq SA A)
     (setq SB (* -2.0 A X))
     (setq SC (- Y C (* B X)))
     (setq X0A (/ (+ (* -1.0 SB) (sqrt (- (* SB SB) (* 4.0 SA SC)))) (* 2.0 SA)))
     (setq Y0A (+ (* A X0A X0A) (* B X0A) C))
     (setq X0B (/ (- (* -1.0 SB) (sqrt (- (* SB SB) (* 4.0 SA SC)))) (* 2.0 SA)))
     (setq Y0B (+ (* A X0B X0B) (* B X0B) C))
     (if (> X0A X0B)
      (progn
       (setq TMP X0A)
       (setq X0A X0B)
       (setq X0B TMP)
       (setq TMP Y0A)
       (setq Y0A Y0B)
       (setq Y0B TMP)
      )
     )
     (princ "\nPick side :")
     (while (= (car (setq TMP (grread nil 1))) 5)
     )
     (if (= (car TMP) 3)
      (if (< (nth 0 (car (cdr TMP))) X)
       (list X0A Y0A)
       (list X0B Y0B)
      )
      nil
     )
    )
    nil
   )
  )
 )
);
;
;   Program written by Robert Livingston, 00/09/18
;
;   VC3P is a utility for drawing vertical curves through 3 points
;
;
(defun C:VC3P (/ A B CMDECHO ENT1 ENT2 G1 G2 L OSMODE P1 P2 P3 P1 VCURVE VEXAG X1 X2 Y1 Y2 TMP Z1 Z2 Z3)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq OSMODE (getvar "OSMODE"))

 (defun VCURVE (ENT1 ENT2 VEXAG L / ATTREQ ENT ENTLIST G1 G2 K P P1 P2 P3 P4 SPLINETYPE SPLINESEGS TMP)
  (setq ATTREQ (getvar "ATTREQ"))
  (setvar "ATTREQ" 1)
  (setq SPLINETYPE (getvar "SPLINETYPE"))
  (setvar "SPLINETYPE" 5)
  (setq SPLINESEGS (getvar "SPLINESEGS"))
  (setvar "SPLINESEGS" 65)

  (setq ENTLIST (entget ENT1))
  (setq P1 (cdr (assoc 10 ENTLIST)))
  (setq P2 (cdr (assoc 11 ENTLIST)))
  (setq ENTLIST (entget ENT2))
  (setq P3 (cdr (assoc 10 ENTLIST)))
  (setq P4 (cdr (assoc 11 ENTLIST)))
  (setq P (inters P1 P2 P3 P4 nil))
  (if (/= nil P)
   (progn
    (if (> (distance P2 P) (distance P1 P))
     (setq P1 P2)
    )
    (if (> (distance P3 P) (distance P4 P))
     (setq P4 P3)
    )
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
    (setq K (abs (/ L (- G2 G1) 100.0)))
    (setvar "ATTREQ" 0)
    (if (= nil (tblsearch "BLOCK" "PVI2")) (MAKEENT "PVI2"))
    (command "._INSERT"
             "PVI2"
             P
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
    (command "._PLINE" P2 P P3 "")
    (setq ENT (entlast))
    (command "._PEDIT" ENT "S" "")
   )
  )
  (setvar "ATTREQ" ATTREQ)
  (setvar "SPLINETYPE" SPLINETYPE)
  (setvar "SPLINESEGS" SPLINESEGS)
 )

 (setq VEXAG (getreal "Vertical exageration <10.000> :"))
 (if (= nil VEXAG) (setq VEXAG 10.0))
 (setq P1 (getpoint "\nFirst point :"))
 (setq P2 (getpoint "\nSecond point :"))
 (setq P3 nil K nil)
 (while (or (= P3 nil) (= K nil))
  (if (= P3 nil)
   (progn
    (setq P3 (getpoint "\nThird point (<Return> to enter K) :"))
    (if (= P3 nil)
     (setq P3 1)
     (setq K 1)
    )
   )
   (progn
    (setq K (getreal "\nK (<Return> to enter third point) :"))
    (if (= K nil)
     (setq P3 nil)
     (progn
      (setq L (/ (- (nth 0 P2) (nth 0 P1)) 2.0))
      (setq Z1 (/ (nth 1 P1) VEXAG))
      (setq Z3 (/ (nth 1 P2) VEXAG))
      (setq Z2 (* (/ (+ Z1 Z3 (/ (* L L) (* 100.0 K))) 2.0) VEXAG))
      (setq P3 (list (+ (nth 0 P1) L) Z2 0.0))
     )
    )
   )
  )
 )
 (setvar "OSMODE" 0)
 (if (< (nth 0 P2) (nth 0 P1))
  (progn
   (setq TMP P1)
   (setq P1 P2)
   (setq P2 TMP)
  )
 )
 (if (< (nth 0 P3) (nth 0 P1))
  (progn
   (setq TMP P1)
   (setq P1 P3)
   (setq P3 TMP)
  )
 )
 (if (< (nth 0 P3) (nth 0 P2))
  (progn
   (setq TMP P2)
   (setq P2 P3)
   (setq P3 TMP)
  )
 )
 (setq X1 (- (nth 0 P2) (nth 0 P1)))
 (setq Y1 (/ (- (nth 1 P2) (nth 1 P1)) VEXAG))
 (setq X2 (- (nth 0 P3) (nth 0 P1)))
 (setq Y2 (/ (- (nth 1 P3) (nth 1 P1)) VEXAG))

 (setq A (/ (- (/ Y2 X2)
               (/ Y1 X1)
            )
            (- X2 X1)
         )
 )
 (setq B (/ (- (/ Y2 (* X2 X2))
               (/ Y1 (* X1 X1))
            )
            (- (/ 1.0 X2)
               (/ 1.0 X1)
            )
         )
 )

 (setq G1 (* B 100.0))
 (setq G2 (* (+ (* 2.0 A X2) B) 100.0))

 (setq TMP nil)
 (setq TMP (getreal (strcat "G1 = " (rtos G1) ", desired (<return> to accept) : ")))
 (if (/= TMP nil)
  (setq G1 TMP)
 )
 (setq TMP nil)
 (setq TMP (getreal (strcat "G2 = " (rtos G2) ", desired (<return> to accept) : ")))
 (if (/= TMP nil)
  (setq G2 TMP)
 )

 (setq X1 (/ (- (/ G1 100.0)
                B
             )
             (* 2.0 A)
          )
 )
 (setq Y1 (+ (* A X1 X1)
             (* B X1)
          )
 )
 (setq X2 (/ (- (/ G2 100.0)
                B
             )
             (* 2.0 A)
          )
 )
 (setq Y2 (+ (* A X2 X2)
             (* B X2)
          )
 )
 (setq P2 (list (+ (nth 0 P1) X2)
                (+ (nth 1 P1)
                   (* VEXAG Y2)
                )
          )
 )
 (setq P1 (list (+ (nth 0 P1) X1)
                (+ (nth 1 P1)
                   (* VEXAG Y1)
                )
          )
 )
 (setq P3 (list (+ (nth 0 P1)
                   (/ (- (nth 0 P2) (nth 0 P1))
                      2.0
                   )
                )
                (+ (nth 1 P1)
                   (* (/ (- (nth 0 P2) (nth 0 P1))
                         2.0
                      )
                      (/ G1 100.0)
                      VEXAG
                   )
                )
          )
 )
 (command "._LINE" P1 P3 "")
 (setq ENT1 (entlast))
 (command "._LINE" P3 P2 "")
 (setq ENT2 (entlast))
 (VCURVE ENT1 ENT2 VEXAG (- X2 X1))

 (setvar "CMDECHO" CMDECHO)
 (setvar "OSMODE" OSMODE)
);
;
;     Program written by Robert Livingston, 02/05/08
;
;     VCSTAN draws a tangent at the specified slope on a vertical curve
;
(defun C:VCSTAN (/ *error* ANGBASE ANGDIR A B C CMDECHO ENT ENTLIST G G1 G2 L OSMODE P P1 P2 P3 S X X1 X2 X3 Y Y1 Y2 Y3)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq OSMODE (getvar "OSMODE"))
 (setvar "OSMODE" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0.0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 0)

 (defun *error* (msg)
  (command "._UCS" "P")
  (setvar "ANGBASE" ANGBASE)
  (setvar "ANGDIR" ANGDIR)
  (setvar "CMDECHO" CMDECHO)
  (setvar "OSMODE" OSMODE)
  (setq *error* nil)
  (print msg)
 )

 (command "._UCS" "W")

 (command "._UNDO" "M")

 (setq ENT (car (entsel "\nSelect vertical curve :")))
 (if (/= nil ENT)
  (progn
   (setq ENTLIST (entget ENT))
   (if (/= "POLYLINE" (cdr (assoc 0 ENTLIST)))
    (princ "\n*** Entity not a polyline ***")
    (progn
     (setq ENT (entnext ENT))
     (setq ENTLIST (entget ENT))
     (setq P1 (cdr (assoc 10 ENTLIST)))
     (setq ENT (entnext ENT))
     (setq ENTLIST (entget ENT))
     (setq P2 (cdr (assoc 10 ENTLIST)))
     (if (/= nil P2)
      (progn
       (setq ENT (entnext ENT))
       (setq ENTLIST (entget ENT))
       (setq P3 (cdr (assoc 10 ENTLIST)))
       (if (/= nil P3)
        (progn
         (setq ENT (entnext ENT))
         (setq ENTLIST (entget ENT))
         (while (/= "SEQEND" (cdr (assoc 0 ENTLIST)))
          (setq P2 P3)
          (setq P3 (cdr (assoc 10 ENTLIST)))
          (setq ENT (entnext ENT))
          (setq ENTLIST (entget ENT))
         )
        )
       )
      )
     )
     (if (and (/= nil P1) (/= nil P2) (/= nil P3))
      (progn
       (setq VEXAG (getreal (strcat "\nEnter vertical exageration (" (rtos 10.0) ") : ")))
       (if (= nil VEXAG) (setq VEXAG 10.0))
       (setq X1 (nth 0 P1))
       (setq Y1 (/ (nth 1 P1) VEXAG))
       (setq X2 (nth 0 P2))
       (setq Y2 (/ (nth 1 P2) VEXAG))
       (setq X3 (nth 0 P3))
       (setq Y3 (/ (nth 1 P3) VEXAG))
       (setq G1 (/ (- Y2 Y1) (- X2 X1)))
       (setq G2 (/ (- Y3 Y2) (- X3 X2)))
       (setq G (getreal (strcat "\nG1 = " (rtos (* G1 100.0)) ", G2 = " (rtos (* G2 100.0)) ", enter slope (%) : ")))
       (if (/= G nil)
        (progn
         (setq G (/ G 100.0))
         (setq A (/ (- G2 G1) (- X3 X1) 2.0))
         (setq B (/ (- G2 (* G1 (/ X3 X1))) (- 1.0 (/ X3 X1))))
         (setq C (- Y1 (+ (* A X1 X1) (* B X1))))
         (setq X (+ X1 (* (/ (- G G1) (- G2 G1)) (- X3 X1))))
         (setq Y (+ (* A X X) (* B X) C))
         (setq L (getdist (strcat "\nEnter length (" (rtos (abs (- X3 X1))) ") : ")))
         (if (= nil L) (setq L (abs (- X3 X1))))
         (setq P1 (list (- X (/ L 2.0))
                        (* (- Y (* (/ L 2.0) G)) VEXAG)))
         (setq P2 (list (+ X (/ L 2.0))
                        (* (+ Y (* (/ L 2.0) G)) VEXAG)))
         (command "._LINE" P1 P2 "")
        )
       )
      )
     )
    )
   )
  )
 )

 (command "._UCS" "P")
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
 (setvar "CMDECHO" CMDECHO)
 (setvar "OSMODE" OSMODE)
);
;
;     Program written by Robert Livingston, 2009-11-25
;
;     VCDTAN is a routine for drawing tangents off of vertical curves.
;
;
(defun C:VCDTAN (/ A B C D ENT ENTLIST G1 G2 ORTHOMODE OSMODE P P1 P2 P3 ROUNDSLOPE S SA SB SC X X0A X0B X1 X2 X3 XB Y Y0A Y0B Y1 Y2 Y3 YB TMP VEXAG)
 (setq OSMODE (getvar "OSMODE"))
 (setvar "OSMODE" 0)
 (setq ORTHOMODE (getvar "ORTHOMODE"))
 (setvar "ORTHOMODE" 0)
 (command "._UNDO" "M")
 (if (= nil VCURVEVEXAG) (setq VCURVEVEXAG 10.0))
 (setq VEXAG (getdist (strcat "\nVertical exaggeration <" (rtos VCURVEVEXAG) "> : ")))
 (if (= nil VEXAG)
  (setq VEXAG VCURVEVEXAG)
  (setq VCURVEVEXAG VEXAG)
 )
 (if (= nil VCURVETANROUND) (setq VCURVETANROUND 0.1))
 (setq ROUNDSLOPE (getdist (strcat "\nRound slope to nearest <" (rtos VCURVETANROUND) "> : ")))
 (if (= nil ROUNDSLOPE)
  (setq ROUNDSLOPE VCURVETANROUND)
  (setq VCURVETANROUND ROUNDSLOPE)
 )
 (setq ROUNDSLOPE (* ROUNDSLOPE VEXAG 0.01))
 (setq ENT (car (entsel "\nSelect vertical curve : ")))
 (if (/= nil ENT)
  (progn
   (setq ENTLIST (entget ENT))
   (if (= "POLYLINE" (cdr (assoc 0 ENTLIST)))
    (progn
     (setq ENT (entnext ENT))
     (setq ENTLIST (entget ENT))
     (setq P1 (cdr (assoc 10 ENTLIST)))
     (setq ENT (entnext ENT))
     (setq ENTLIST (entget ENT))
     (setq P2 (cdr (assoc 10 ENTLIST)))
     (if (/= nil P2)
      (progn
       (setq ENT (entnext ENT))
       (setq ENTLIST (entget ENT))
       (setq P3 (cdr (assoc 10 ENTLIST)))
       (if (/= nil P3)
        (progn
         (setq ENT (entnext ENT))
         (setq ENTLIST (entget ENT))
         (while (/= "SEQEND" (cdr (assoc 0 ENTLIST)))
          (setq P2 P3)
          (setq P3 (cdr (assoc 10 ENTLIST)))
          (setq ENT (entnext ENT))
          (setq ENTLIST (entget ENT))
         )
        )
       )
      )
     )
     (if (and (/= nil P1) (/= nil P2) (/= nil P3))
      (progn
       (setq X1 (nth 0 P1))
       (setq Y1 (nth 1 P1))
       (setq X2 (nth 0 P2))
       (setq Y2 (nth 1 P2))
       (setq X3 (nth 0 P3))
       (setq Y3 (nth 1 P3))
       (setq G1 (/ (- Y2 Y1) (- X2 X1)))
       (setq G2 (/ (- Y3 Y2) (- X3 X2)))
       (setq A (/ (- G2 G1) (- X3 X1) 2.0))
       (setq B (/ (- G2 (* G1 (/ X3 X1))) (- 1.0 (/ X3 X1))))
       (setq C (- Y1 (+ (* A X1 X1) (* B X1))))
       (while (= (car (setq TMP (grread nil 1))) 5)
        (setq X (caadr TMP))
        (setq Y (cadadr TMP))
        (command "._REDRAW")
        (setq SA A)
        (setq SB (* -2.0 A X))
        (setq SC (- Y C (* B X)))
        (if (> (- (* SB SB) (* 4.0 SA SC)) 0.0)
         (progn
          (setq X0A (/ (+ (* -1.0 SB) (sqrt (- (* SB SB) (* 4.0 SA SC)))) (* 2.0 SA)))
          (setq Y0A (+ (* A X0A X0A) (* B X0A) C))
          (setq X0B (/ (- (* -1.0 SB) (sqrt (- (* SB SB) (* 4.0 SA SC)))) (* 2.0 SA)))
          (setq Y0B (+ (* A X0B X0B) (* B X0B) C))
          (if (> X0A X0B)
           (progn
            (setq TMP X0A)
            (setq X0A X0B)
            (setq X0B TMP)
            (setq TMP Y0A)
            (setq Y0A Y0B)
            (setq Y0B TMP)
           )
          )
          (if (< (nth 0 P2) X)
           (progn
            (setq XB X0A YB Y0A)
           )
           (progn
            (setq XB X0B YB Y0B)
           )
          )
          (setq D (distance (list XB YB) (list X Y)))
          (setq S (/ (- Y YB) (- X XB)))
          (if (> S 0.0) (setq TMP 1.0) (setq TMP -1.0))
          (setq S (abs S))
          (setq S (* TMP (* (float (fix (+ 0.5 (/ S ROUNDSLOPE)))) ROUNDSLOPE)))
          (setq XB (/ (- S B) (* 2.0 A)))
          (setq YB (+ (* A XB XB) (* B XB) C))
          (if (< (nth 0 P2) X)
           (progn
            (setq X (+ XB (* D (cos (atan S)))))
            (setq Y (+ YB (* D (sin (atan S)))))
           )
           (progn
            (setq X (- XB (* D (cos (atan S)))))
            (setq Y (- YB (* D (sin (atan S)))))
           )
          )
          (grdraw (list XB YB) (list X Y) 7)
         )
        )
       )
       (command "._REDRAW")
       (if (> (- (* SB SB) (* 4.0 SA SC)) 0.0)
        (command "._LINE" (list XB YB) (list X Y) "")
        (princ "\nBad Point!")
       )
      )
     )
    )
   )
  )
 )
 (setvar "OSMODE" OSMODE)
 (setvar "ORTHOMODE" ORTHOMODE)
);
;
;     Program written by Robert Livingston, 2015-01-29
;
;     C:QSECTION is a utility for drawing a cross section at a specified station
;
;
(defun C:QSECTION (/ *error* ANGBASE ATTREQ CMDECHO GETQSECTIONLIST INFILE INFILE2 INFILENAME INLINE INLINE2 OSMODE ORTHOMODE PBASE STA STALIST)
;(defun C:QSECTION ()
 (setq ATTREQ (getvar "ATTREQ"))
 (setvar "ATTREQ" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0.0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 1)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq OSMODE (getvar "OSMODE"))
 (setq ORTHOMODE (getvar "ORTHOMODE"))

 (defun *error* (msg)
  (setvar "ATTREQ" ATTREQ)
  (setvar "ANGBASE" ANGBASE)
  (setvar "ANGDIR" ANGDIR)
  (setvar "CMDECHO" CMDECHO)
  (setvar "OSMODE" OSMODE)
  (setvar "ORTHOMODE" ORTHOMODE)
  (if (not INFILE) (close INFILE))
  (if (not INFILE2) (close INFILE2))
  (setq *error* nil)
  (print msg)
 )
 
 (defun GETQSECTIONLIST (/ DCIRCLE OBSURFACE REP SWATH THEIGHT VEXAG)
  (setq DCIRCLE nil)
  (setq RFL:QSECTIONALIGNLIST nil)
  (setq RFL:QSECTIONPROFILELIST nil)
  (setq VEXAG 10.0)
  (setq REP (getdist (strcat "\nVertical exaggeration <" (rtos VEXAG) "> : ")))
  (if (/= nil REP) (setq VEXAG REP))
  (setq SWATH 100.0)
  (setq REP (getdist (strcat "\nSwath width <" (rtos SWATH) "> : ")))
  (if (/= nil REP) (setq SWATH REP))
  (setq THEIGHT 5.0)
  (setq REP (getdist (strcat "\nText height <" (rtos THEIGHT) "> : ")))
  (if (/= nil REP) (setq THEIGHT REP))
  (initget "Yes No Block Template")
  (setq REP (getkword "\nDraw circle at design elevation (<Yes>/No/Block/Template) ? "))
  (if (= "No" REP)
   (setq DCIRCLE 0)
   (if (= "Block" REP)
    (progn
     (setq DCIRCLE nil)
     (while (= nil DCIRCLE)
      (setq DCIRCLE (car (entsel "\nSelect block (<return> to enter name) : ")))
      (if (= nil DCIRCLE)
       (setq DCIRCLE (getstring "\nBlock name : "))
       (setq DCIRCLE (cdr (assoc 2 (entget DCIRCLE))))
      )
      (if (/= nil DCIRCLE)
       (if (= nil (tblsearch "BLOCK" DCIRCLE))
        (setq DCIRCLE nil)
       )
      )
     )
    )
    (if (= "Template" REP)
     (progn
      (if (= (vl-registry-read "HKEY_CURRENT_USER\\rflAlignDirectory") nil)
       (vl-registry-write "HKEY_CURRENT_USER\\rflAlignDirectory" "" "")
      )
      (setq INFILENAME (getfiled "Select a Template File" (vl-registry-read "HKEY_CURRENT_USER\\rflAlignDirectory") "tpl" 2))
      (vl-registry-write "HKEY_CURRENT_USER\\rflAlignDirectory" "" (strcat (vl-filename-directory INFILENAME) "\\"))
      (setq INFILE (open INFILENAME "r"))
      (while (setq INLINE (read-line INFILE))
       (if (= "STA" (RFL:COLUMN INLINE 1 ","))
        (progn
         (setq TPLLIST nil)
         (setq STALIST (list (atof (RFL:COLUMN INLINE 2 ",")) (atof (RFL:COLUMN INLINE 3 ","))))
         (setq INFILE2 (open (RFL:COLUMN INLINE 4 ",") "r"))
         (while (setq INLINE2 (read-line INFILE2))
          (setq TPLLIST (append TPLLIST (list INLINE2)))
         )
         (close INFILE2)
         (setq DCIRCLE (append DCIRCLE (list (list STALIST TPLLIST))))
        )
        (setq DCIRCLE (append DCIRCLE (list INLINE)))
       )
      )
      (close INFILE)
     )
     (setq DCIRCLE 1)
    )
   )
  )
  (setq OBSURFACE nil)
  (setq REP "Yes")
  (while (= "Yes" REP)
   (setq OBSURFACE (append OBSURFACE (list (RFL:GETC3DSURFACE))))
   (initget "Yes No")
   (setq REP (getkword "\nAdd another (Yes/<No>) : "))
  )
  (setq RFL:QSECTIONLIST (list (cons "VEXAG" VEXAG)
                               (cons "SWATH" SWATH)
                               (cons "THEIGHT" THEIGHT)
                               (cons "DCIRCLE" DCIRCLE)
                               (cons "OBSURFACE" OBSURFACE)
                         )
  )
 )
 (if (= nil RFL:ALIGNLIST)
  (princ "\n!!! No Alignment Defined !!!")
  (progn
   (if (= nil RFL:QSECTIONLIST) (GETQSECTIONLIST))
   (while (= "" (setq STA (getstring "\nStation (<return> to pick point, <P> to pick point) : ")))
    (GETQSECTIONLIST)
   )
   (if (= "P" (strcase (substr STA 1 1)))
    (setq STA (car (RFL:STAOFF (getpoint "\nPick point for section : "))))
    (setq STA (atof STA))
   )
   (setq PBASE (getpoint "\nBase point : "))
   (setvar "OSMODE" 0)
   (setvar "ORTHOMODE" 0)
   (QSECTION STA
             (cdr (assoc "SWATH" RFL:QSECTIONLIST))
             PBASE
             nil
             (cdr (assoc "VEXAG" RFL:QSECTIONLIST))
             (cdr (assoc "THEIGHT" RFL:QSECTIONLIST))
             (cdr (assoc "DCIRCLE" RFL:QSECTIONLIST))
             (cdr (assoc "OBSURFACE" RFL:QSECTIONLIST))
   )
  )
 )
 (setvar "ATTREQ" ATTREQ)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
 (setvar "CMDECHO" CMDECHO)
 (setvar "OSMODE" OSMODE)
 (setvar "ORTHOMODE" ORTHOMODE)
 (eval nil)
)
(defun QSECTION (STA SWATH PBASE ZBASE VEXAG THEIGHT DCIRCLE OBSURFACE / A ADDHANDLE AFLAG ALLIST ALSAVE C D DX DY DY2 DZ ENT ENTLIST ENTSET H HANDENTLIST GETTPL ISABOVE NODE OX P P0 P1 P2 PA PB OSLIST PLIST PVISAVE S1 S2 SLIST SLISTDEFAULT TLIST TLISTL TLISTCL TLISTFL TLISTR TLISTCR TLISTFR TMP TOL TX TY Z ZHEIGHT)
;(defun QSECTION (STA SWATH PBASE ZBASE VEXAG THEIGHT DCIRCLE OBSURFACE)
 (setq TOL 0.0001)
 (defun ISABOVE (P OSLIST / C RES)
  (if P
   (if OSLIST
    (progn
     (setq C 1)
     (while (< C (length OSLIST))
      (if (and (>= (car P) (car (nth (1- C) OSLIST)))
               (<= (car P) (car (nth C OSLIST)))
          )
       (if (> (cadr P)
              (+ (cadr (nth (1- C) OSLIST))
                 (* (- (car P) (car (nth (1- C) OSLIST)))
                    (/ (- (cadr (nth C OSLIST)) (cadr (nth (1- C) OSLIST)))
                       (- (car (nth C OSLIST)) (car (nth (1- C) OSLIST)))
                    )
                 )
              )
           )
        (setq RES T)
        (setq RES nil)
       )
      )
      (setq C (1+ C))
     )
    )
    (setq RES nil)
   )
   (setq RES nil)
  )
  RES
 )
 (if (or RFL:QSECTIONALIGNLIST RFL:QSECTIONPROFILELIST)
  (setq AFLAG nil)
  (setq AFLAG T)
 )
 (defun GETTPL (STA DCIRCLE / NODE TPLLIST)
  (if (listp (car DCIRCLE))
   (progn
    (setq TPLLIST nil)
    (foreach NODE DCIRCLE
     (if (and (>= STA (car (car NODE)))
              (< STA (cadr (car NODE)))
         )
      (setq TPLLIST (cadr NODE))
     )
    )
    TPLLIST
   )
   DCIRCLE
  )
 )
 (defun ADDHANDLE (/ C ENT FLAG NAME)
  (setq ENT (cdr (assoc 5 (entget (entlast)))))
  (setq C 0)
  (setq FLAG T)
  (while (and FLAG (< C (length HANDENTLIST)))
   (if (= ENT (nth C HANDENTLIST)) (setq FLAG nil))
   (setq C (+ C 1))
  )
  (if FLAG (setq HANDENTLIST (append HANDENTLIST (list ENT))))
 )
 (setq HANDENTLIST nil)
 (setq P0 (RFL:XY (list STA 0.0)))
 (setq P1 (RFL:XY (list STA (/ SWATH -2.0))))
 (setq P2 (RFL:XY (list STA (/ SWATH 2.0))))
 (setq PLIST nil)
 (foreach NODE OBSURFACE
  (setq PLIST (append PLIST (RFL:GETSURFACELINE P1 P2 NODE)))
  (setq PLIST (append PLIST (list "NewLine")))
 )
 (if (/= nil PLIST)
  (progn
   (if (= nil ZBASE)
    (progn
     (foreach P PLIST
      (if (/= "NewLine" P)
       (if (= nil ZBASE)
        (setq ZBASE (caddr P))
        (if (< (caddr P) ZBASE) (setq ZBASE (caddr P)))
       )
      )
     )
    )
   )
   (setq ZHEIGHT nil)
   (foreach P PLIST
    (if (/= "NewLine" P)
     (if (= nil ZHEIGHT)
      (setq ZHEIGHT (caddr P))
      (if (> (caddr P) ZHEIGHT) (setq ZHEIGHT (caddr P)))
     )
    )
   )
   (setq D (/ SWATH 2.0))
   (setq OSLIST nil)
   (foreach P PLIST
    (if (= P "NewLine")
     (setq OSLIST (append OSLIST (list "NewLine")))
     (setq OSLIST (append OSLIST (list (list (- (distance (list (car P) (cadr P)) P1) D) (caddr P)))))
    )
   )
   (setq Z (RFL:ELEVATION STA))
   (setq ENTLIST (list (cons 0 "TEXT")
                       (cons 1 (strcat "Sta: "
                                       (RFL:STATXT STA)
                                       (if Z
                                        (strcat " / Ctrl Elev: " (rtos Z))
                                        (strcat " / Text Elev: " (rtos ZBASE))
                                       )
                               )
                       )
                       (list 10 (car PBASE) (cadr PBASE) 0.0)
                       (list 11 (car PBASE) (cadr PBASE) 0.0)
                       (cons 40 (/ THEIGHT 10.0))
                       (cons 50 0.0)
                       (cons 72 4)
                 )
   )
   (entmake ENTLIST)
   (ADDHANDLE)
   (DRAWGRID (strcat "Sta: " (RFL:STATXT STA))                           ; Title Text
             THEIGHT                                                     ; Title Height
             THEIGHT                                                     ; Title OFFSET
             (list (- (car PBASE) (/ SWATH 2.0)) (cadr PBASE))           ; Basepoint
             (/ SWATH -2.0)                                              ; Base Station
             ZBASE                                                       ; Base Elevation
             SWATH                                                       ; Grid Width
             (* (- ZHEIGHT ZBASE) VEXAG)                                 ; Grid Height
             VEXAG                                                       ; Vertical Exageration
             THEIGHT                                                     ; Text Height
             THEIGHT                                                     ; Text OFFSET
             (/ (expt 10.0 (fix (/ (log SWATH) (log 10.0)))) 10.0)       ; Horizontal Grid
             nil                                                         ; Horizontal Fine Grid
             (/ (expt 10.0 (fix (/ (log SWATH) (log 10.0)))) 10.0)       ; Horizontal Text
             (/ (expt 10.0 (fix (/ (log SWATH) (log 10.0)))) 10.0 VEXAG) ; Vertical Grid
             nil                                                         ; Vertical Fine Grid
             (/ (expt 10.0 (fix (/ (log SWATH) (log 10.0)))) 10.0 VEXAG) ; Vertical Text
             "PR-GRID"                                                   ; Grid Layer
             (getvar "CLAYER")                                           ; Fine Grid Layer
             (getvar "CLAYER")                                           ; Text Layer
             nil                                                         ; Label as Station
             1.0                                                         ; Master Scale
             1                                                           ; Direction (1 = Left to Right, -1 = Right to Left)
   )
   (ADDHANDLE)
   (command "._PLINE")
   (foreach P OSLIST
    (progn
     (if (= "NewLine" P)
      (progn
       (command)
       ;(setq HANDENTLIST (append HANDENTLIST (list (cdr (assoc 5 (entget (entlast)))))))
       (ADDHANDLE)
       (command "._PLINE")
      )
      (command (list (+ (car PBASE) (car P))
                     (+ (cadr PBASE) (* VEXAG (- (cadr P) ZBASE)))
               )
      )
     )
    )
   )
   (command)
   ;(setq HANDENTLIST (append HANDENTLIST (list (cdr (assoc 5 (entget (entlast)))))))
   (if (listp DCIRCLE)
    (progn
     (if (/= nil Z)
      (progn
       (setq TLISTL nil
             TLISTCL nil
             TLISTFL nil
             TLISTR nil
             TLISTCR nil
             TLISTFR nil
             SLISTDEFAULT (list -2.0 -2.0)
       )
       (setq SLIST (RFL:SUPER STA))
       (foreach NODE (GETTPL STA DCIRCLE)
        (progn
         (setq DX nil DY nil)
         (cond ((= "S" (strcase (RFL:COLUMN NODE 1 ","))) ; S = Default Superelevation
                (setq SLISTDEFAULT (list (atof (RFL:COLUMN NODE 2 ",")) (atof (RFL:COLUMN NODE 3 ","))))
               )
               ((and (= "AP" (strcase (RFL:COLUMN NODE 1 ","))) ; AP = Polyline Alignments
                     AFLAG ; only redo if QSection reset
                )
                (progn
                 (princ "\nLoading control alignments .")
                 (setq ALSAVE RFL:ALIGNLIST)
                 (setq A (atoi (RFL:COLUMN NODE 2 ",")))
                 (setq TMP nil)
                 (if (setq ENTSET (ssget "X" (list (cons 0 "POLYLINE") (cons 8 (RFL:COLUMN NODE 3 ",")))))
                  (progn
                   (setq C 0)
                   (while (< C (sslength ENTSET))
                    (princ ".")
                    (setq ENT (ssname ENTSET C))
                    (command "._CONVERT" "P" "S" ENT "")
                    (setq C (1+ C))
                   )
                  )
                 )
                 (if (setq ENTSET (ssget "X" (list (cons 0 "LWPOLYLINE") (cons 8 (RFL:COLUMN NODE 3 ",")))))
                  (progn
                   (setq C 0)
                   (while (< C (sslength ENTSET))
                    (princ ".")
                    (setq ENT (ssname ENTSET C))
                    (if (setq RFL:ALIGNLIST (RFL:ALIGNDEF ENT (cdr (assoc 10 (entget ENT))) 0.0))
                     (setq TMP (append TMP (list RFL:ALIGNLIST)))
                    )
                    (setq C (1+ C))
                   )
                  )
                 )
                 (setq RFL:QSECTIONALIGNLIST (append RFL:QSECTIONALIGNLIST (list (list A TMP))))
                 (setq RFL:ALIGNLIST ALSAVE)
                )
               )
               ((= "L" (strcase (RFL:COLUMN NODE 1 ","))) ; L = Left control
                (progn
                 (cond ((= "X" (strcase (substr (RFL:COLUMN NODE 2 ",") 1 1))) ; X = X offset
                        (setq DX (atof (substr (RFL:COLUMN NODE 2 ",") 2)))
                       )
                       ((= "A" (strcase (substr (RFL:COLUMN NODE 2 ",") 1 1))) ; A = Alignment offset
                        (progn
                         (setq A (atoi (substr (RFL:COLUMN NODE 2 ",") 2)))
                         (setq TMP nil)
                         (setq ALSAVE RFL:ALIGNLIST)
                         (foreach RFL:ALIGNLIST (cadr (assoc A RFL:QSECTIONALIGNLIST))
                          (if (setq P (RFL:ALINTERS P0 P1 RFL:ALIGNLIST))
                           (setq TMP (append TMP (list P)))
                          )
                         )
                         (setq RFL:ALIGNLIST ALSAVE)
                         (if TMP
                          (progn
                           (setq P (caadar (vl-sort (mapcar '(lambda (A) (list (distance P0 A) (list A)))  TMP) '(lambda (A B) (< (car A) (car B))))))
                           (if (last TLISTL)
                            (setq DX (- (* -1.0 (car (last TLISTL))) (distance P0 P)))
                            (setq DX 0.0)
                           )
                          )
                          (setq DX 0.0)
                         )
                        )
                       )
                       ((= "Y" (strcase (substr (RFL:COLUMN NODE 2 ",") 1 1))) ; Y = Y offset
                        (setq DY (atof (substr (RFL:COLUMN NODE 2 ",") 2)))
                       )
                 )
                 (cond ((= "Y" (strcase (substr (RFL:COLUMN NODE 3 ",") 1 1))) ; Y = Y offset
                        (setq DY (atof (substr (RFL:COLUMN NODE 3 ",") 2)))
                       )
                       ((= "DS-L" (strcase (substr (RFL:COLUMN NODE 3 ",") 1 3))) ; DS-L = Left depth super * -1.0
                        (setq DY (+ (* DX 0.01 (if SLIST (car SLIST) (car SLISTDEFAULT))) (atof (substr (RFL:COLUMN NODE 3 ",") 5))))
                       )
                       ((= "DSL" (strcase (substr (RFL:COLUMN NODE 3 ",") 1 3))) ; DSL = Left depth super
                        (setq DY (+ (* DX -0.01 (if SLIST (car SLIST) (car SLISTDEFAULT))) (atof (substr (RFL:COLUMN NODE 3 ",") 4))))
                       )
                       ((= "S-L" (strcase (substr (RFL:COLUMN NODE 3 ",") 1 3))) ; S-L = Left super * -1.0
                        (setq DY (* DX 0.01 (if SLIST (car SLIST) (car SLISTDEFAULT))))
                       )
                       ((= "SL" (strcase (substr (RFL:COLUMN NODE 3 ",") 1 2))) ; SL = Left Super
                        (setq DY (* DX -0.01 (if SLIST (car SLIST) (car SLISTDEFAULT))))
                       )
                       ((= "DS-R" (strcase (substr (RFL:COLUMN NODE 3 ",") 1 3))) ; DS-R = Right depth super * -1.0
                        (setq DY (+ (* DX 0.01 (if SLIST (cadr SLIST) (cadr SLISTDEFAULT))) (atof (substr (RFL:COLUMN NODE 3 ",") 5))))
                       )
                       ((= "DSR" (strcase (substr (RFL:COLUMN NODE 3 ",") 1 3))) ; DSR = Right depth super
                        (setq DY (+ (* DX -0.01 (if SLIST (cadr SLIST) (cadr SLISTDEFAULT))) (atof (substr (RFL:COLUMN NODE 3 ",") 4))))
                       )
                       ((= "S-R" (strcase (substr (RFL:COLUMN NODE 3 ",") 1 3))) ; S-R = Right super * -1.0
                        (setq DY (* DX 0.01 (if SLIST (cadr SLIST) (cadr SLISTDEFAULT))))
                       )
                       ((= "SR" (strcase (substr (RFL:COLUMN NODE 3 ",") 1 2))) ; SR = Right super
                        (setq DY (* DX -0.01 (if SLIST (cadr SLIST) (cadr SLISTDEFAULT))))
                       )
                       ((= "OX" (strcase (substr (RFL:COLUMN NODE 3 ",") 1 2))) ; OX = Right offset at X:1
                        (setq OX (atof (substr (RFL:COLUMN NODE 3 ",") 3))
                              DX (/ DY (+ (/ 1.0 OX) (* 0.01 (if SLIST (car SLIST) (car SLISTDEFAULT)))))
                              DY (+ DY (* -0.01 (if SLIST (car SLIST) (car SLISTDEFAULT)) DX))
                        )
                       )
                 )
                 (if (= nil TLISTL)
                  (setq TLISTL (list (list DX (+ Z DY))))
                  (setq TLISTL (append TLISTL (list (list (+ (car (last TLISTL)) DX)  (+ (cadr (last TLISTL)) DY)))))
                 )
                )
               )
               ((= "CL" (strcase (RFL:COLUMN NODE 1 ","))) ; CL = Cut Slope Left
                (progn
                 (setq DX nil DY nil)
                 (cond ((= "X" (strcase (substr (RFL:COLUMN NODE 2 ",") 1 1))) ; X = X offset
                        (progn
                         (setq DX (atof (substr (RFL:COLUMN NODE 2 ",") 2)))
                         (setq DY (atof (substr (RFL:COLUMN NODE 3 ",") 2)))
                        )
                       )
                       (T  ;  Cut/Fill only accept DX/DY offsets, if not will look for toe
                           ;  
                        (progn
                         (setq PA nil PB nil P nil)
                         (if (/= nil TLISTCL)
                          (setq PB (last TLISTCL))
                          (if (/= nil TLISTL)
                           (setq PB (last TLISTL))
                          )
                         )
                         (if PB
                          (progn
                           (setq P nil)
                           (setq PA (list (/ SWATH -2.0)
                                          (+ (cadr PB) (/ (- (car PB) (/ SWATH -2.0)) (atof (RFL:COLUMN NODE 2 ","))))
                                    )
                           )
                           (setq C 1)
                           (while (and (< C (length OSLIST))
                                       (/= "NewLine" (nth C OSLIST))
                                  )
                            (if (setq TMP (inters PA PB (nth (1- C) OSLIST) (nth C OSLIST)))
                             (setq P TMP)
                            )
                            (setq C (1+ C))
                           )
                           (if (= nil P)
                            (progn
                             (setq PA (list (/ SWATH -2.0)
                                            (+ (cadr PB) (/ (- (car PB) (/ SWATH -2.0)) (atof (RFL:COLUMN NODE 3 ","))))
                                      )
                             )
                             (setq C 1)
                             (while (and (< C (length OSLIST))
                                         (/= "NewLine" (nth C OSLIST))
                                    )
                              (if (setq TMP (inters PA PB (nth (1- C) OSLIST) (nth C OSLIST)))
                               (setq P TMP)
                              )
                              (setq C (1+ C))
                             )
                            )
                           )
                           (if P
                            (setq DX (- (car P) (car PB))
                                  DY (- (cadr P) (cadr PB))
                            )
                           )
                          )
                         )
                        )
                       )
                 )
                 (if (and DX DY)
                  (if (= nil TLISTCL)
                   (if (= nil TLISTL)
                    (setq TLISTCL (list (list DX (+ Z DY))))
                    (setq TLISTCL (append TLISTCL (list (list (+ (car (last TLISTL)) DX)  (+ (cadr (last TLISTL)) DY)))))
                   )
                   (setq TLISTCL (append TLISTCL (list (list (+ (car (last TLISTCL)) DX)  (+ (cadr (last TLISTCL)) DY)))))
                  )
                 )
                )
               )
               ((= "FL" (strcase (RFL:COLUMN NODE 1 ","))) ; FL = Fill Slope Left
                (progn
                 (setq DX nil DY nil)
                 (cond ((= "X" (strcase (substr (RFL:COLUMN NODE 2 ",") 1 1))) ; X = X offset
                        (progn
                         (setq DX (atof (substr (RFL:COLUMN NODE 2 ",") 2)))
                         (setq DY (atof (substr (RFL:COLUMN NODE 3 ",") 2)))
                        )
                       )
                       (T  ;  Cut/Fill only accept DX/DY offsets, if not will look for toe
                           ;  
                        (progn
                         (setq PA nil PB nil P nil)
                         (if (/= nil TLISTFL)
                          (setq PB (last TLISTFL))
                          (if (/= nil TLISTL)
                           (setq PB (last TLISTL))
                          )
                         )
                         (if PB
                          (progn
                           (setq P nil)
                           (setq PA (list (/ SWATH -2.0)
                                          (+ (cadr PB) (/ (- (car PB) (/ SWATH -2.0)) (atof (RFL:COLUMN NODE 2 ","))))
                                    )
                           )
                           (setq C 1)
                           (while (and (< C (length OSLIST))
                                       (/= "NewLine" (nth C OSLIST))
                                  )
                            (if (setq TMP (inters PA PB (nth (1- C) OSLIST) (nth C OSLIST)))
                             (setq P TMP)
                            )
                            (setq C (1+ C))
                           )
                           (if (= nil P)
                            (progn
                             (setq PA (list (/ SWATH -2.0)
                                            (+ (cadr PB) (/ (- (car PB) (/ SWATH -2.0)) (atof (RFL:COLUMN NODE 3 ","))))
                                      )
                             )
                             (setq C 1)
                             (while (and (< C (length OSLIST))
                                         (/= "NewLine" (nth C OSLIST))
                                    )
                              (if (setq TMP (inters PA PB (nth (1- C) OSLIST) (nth C OSLIST)))
                               (setq P TMP)
                              )
                              (setq C (1+ C))
                             )
                            )
                           )
                           (if P
                            (setq DX (- (car P) (car PB))
                                  DY (- (cadr P) (cadr PB))
                            )
                           )
                          )
                         )
                        )
                       )
                 )
                 (if (and DX DY)
                  (if (= nil TLISTFL)
                   (if (= nil TLISTL)
                    (setq TLISTFL (list (list DX (+ Z DY))))
                    (setq TLISTFL (append TLISTFL (list (list (+ (car (last TLISTL)) DX)  (+ (cadr (last TLISTL)) DY)))))
                   )
                   (setq TLISTFL (append TLISTFL (list (list (+ (car (last TLISTFL)) DX)  (+ (cadr (last TLISTFL)) DY)))))
                  )
                 )
                )
               )
               ((= "R" (strcase (RFL:COLUMN NODE 1 ","))) ; R = Right control
                (progn
                 (cond ((= "X" (strcase (substr (RFL:COLUMN NODE 2 ",") 1 1))) ; X = X offset
                        (setq DX (atof (substr (RFL:COLUMN NODE 2 ",") 2)))
                       )
                       ((= "A" (strcase (substr (RFL:COLUMN NODE 2 ",") 1 1))) ; A = Alignment offset
                        (progn
                         (setq A (atoi (substr (RFL:COLUMN NODE 2 ",") 2)))
                         (setq TMP nil)
                         (setq ALSAVE RFL:ALIGNLIST)
                         (foreach RFL:ALIGNLIST (cadr (assoc A RFL:QSECTIONALIGNLIST))
                          (if (setq P (RFL:ALINTERS P0 P2 RFL:ALIGNLIST))
                           (setq TMP (append TMP (list P)))
                          )
                         )
                         (setq RFL:ALIGNLIST ALSAVE)
                         (if TMP
                          (progn
                           (setq P (caadar (vl-sort (mapcar '(lambda (A) (list (distance P0 A) (list A)))  TMP) '(lambda (A B) (< (car A) (car B))))))
                           (if (last TLISTR)
                            (setq DX (- (distance P0 P) (car (last TLISTR))))
                            (setq DX 0.0)
                           )
                          )
                          (setq DX 0.0)
                         )
                        )
                       )
                       ((= "Y" (strcase (substr (RFL:COLUMN NODE 2 ",") 1 1))) ; Y = Y offset
                        (setq DY (atof (substr (RFL:COLUMN NODE 2 ",") 2)))
                       )
                 )
                 (cond ((= "Y" (strcase (substr (RFL:COLUMN NODE 3 ",") 1 1))) ; Y = Y offset
                        (setq DY (atof (substr (RFL:COLUMN NODE 3 ",") 2)))
                       )
                       ((= "DS-L" (strcase (substr (RFL:COLUMN NODE 3 ",") 1 3))) ; DS-L = Left depth super * -1.0
                        (setq DY (+ (* DX -0.01 (if SLIST (car SLIST) (car SLISTDEFAULT))) (atof (substr (RFL:COLUMN NODE 3 ",") 5))))
                       )
                       ((= "DSL" (strcase (substr (RFL:COLUMN NODE 3 ",") 1 3))) ; DSL = Left depth super
                        (setq DY (+ (* DX 0.01 (if SLIST (car SLIST) (car SLISTDEFAULT))) (atof (substr (RFL:COLUMN NODE 3 ",") 4))))
                       )
                       ((= "S-L" (strcase (substr (RFL:COLUMN NODE 3 ",") 1 3))) ; S-L = Left super * -1.0
                        (setq DY (* DX -0.01 (if SLIST (car SLIST) (car SLISTDEFAULT))))
                       )
                       ((= "SL" (strcase (substr (RFL:COLUMN NODE 3 ",") 1 2))) ; SL = Left Super
                        (setq DY (* DX 0.01 (if SLIST (car SLIST) (car SLISTDEFAULT))))
                       )
                       ((= "DS-R" (strcase (substr (RFL:COLUMN NODE 3 ",") 1 3))) ; DS-R = Right depth super * -1.0
                        (setq DY (+ (* DX -0.01 (if SLIST (cadr SLIST) (cadr SLISTDEFAULT))) (atof (substr (RFL:COLUMN NODE 3 ",") 5))))
                       )
                       ((= "DSR" (strcase (substr (RFL:COLUMN NODE 3 ",") 1 3))) ; DSR = Right depth super
                        (setq DY (+ (* DX 0.01 (if SLIST (cadr SLIST) (cadr SLISTDEFAULT))) (atof (substr (RFL:COLUMN NODE 3 ",") 4))))
                       )
                       ((= "S-R" (strcase (substr (RFL:COLUMN NODE 3 ",") 1 3))) ; S-R = Right super * -1.0
                        (setq DY (* DX -0.01 (if SLIST (cadr SLIST) (cadr SLISTDEFAULT))))
                       )
                       ((= "SR" (strcase (substr (RFL:COLUMN NODE 3 ",") 1 2))) ; SR = Right super
                        (setq DY (* DX 0.01 (if SLIST (cadr SLIST) (cadr SLISTDEFAULT))))
                       )
                       ((= "OX" (strcase (substr (RFL:COLUMN NODE 3 ",") 1 2))) ; OX = Left offset at X:1
                        (setq OX (atof (substr (RFL:COLUMN NODE 3 ",") 3))
                              DX (/ DY (* -1.0 (+ (/ 1.0 OX) (* 0.01 (if SLIST (cadr SLIST) (cadr SLISTDEFAULT))))))
                              DY (+ DY (* 0.01 (if SLIST (cadr SLIST) (cadr SLISTDEFAULT)) DX))
                        )
                       )
                 )
                 (if (= nil TLISTR)
                  (setq TLISTR (list (list DX (+ Z DY))))
                  (setq TLISTR (append TLISTR (list (list (+ (car (last TLISTR)) DX)  (+ (cadr (last TLISTR)) DY)))))
                 )
                )
               )
               ((= "CR" (strcase (RFL:COLUMN NODE 1 ","))) ; CR = Cut Slope Right
                (progn
                 (setq DX nil DY nil)
                 (cond ((= "X" (strcase (substr (RFL:COLUMN NODE 2 ",") 1 1))) ; X = X offset
                        (progn
                         (setq DX (atof (substr (RFL:COLUMN NODE 2 ",") 2)))
                         (setq DY (atof (substr (RFL:COLUMN NODE 3 ",") 2)))
                        )
                       )
                       (T  ;  Cut/Fill only accept DX/DY offsets, if not will look for toe
                           ;  
                        (progn
                         (setq PA nil PB nil P nil)
                         (if (/= nil TLISTCR)
                          (setq PA (last TLISTCR))
                          (if (/= nil TLISTR)
                           (setq PA (last TLISTR))
                          )
                         )
                         (if PA
                          (progn
                           (setq P nil)
                           (setq PB (list (/ SWATH 2.0)
                                          (+ (cadr PA) (/ (- (/ SWATH 2.0) (car PA)) (atof (RFL:COLUMN NODE 2 ","))))
                                    )
                           )
                           (setq C (1- (length OSLIST)))
                           (while (> C 0)
                            (if (and (/= "NewLine" (nth C OSLIST))
                                     (/= "NewLine" (nth (1- C) OSLIST))
                                     (setq TMP (inters PA PB (nth (1- C) OSLIST) (nth C OSLIST)))
                                )
                             (setq P TMP)
                            )
                            (setq C (1- C))
                           )
                           (if (= nil P)
                            (progn
                             (setq PB (list (/ SWATH 2.0)
                                            (+ (cadr PA) (/ (- (/ SWATH 2.0) (car PA)) (atof (RFL:COLUMN NODE 3 ","))))
                                      )
                             )
                             (setq C (1- (length OSLIST)))
                             (while (> C 0)
                              (if (and (/= "NewLine" (nth C OSLIST))
                                       (/= "NewLine" (nth (1- C) OSLIST))
                                       (setq TMP (inters PA PB (nth (1- C) OSLIST) (nth C OSLIST)))
                                  )
                               (setq P TMP)
                              )
                              (setq C (1- C))
                             )
                            )
                           )
                           (if P
                            (setq DX (- (car P) (car PA))  ;*****
                                  DY (- (cadr P) (cadr PA))  ;*****
                            )
                           )
                          )
                         )
                        )
                       )
                 )
                 (if (and DX DY)
                  (if (= nil TLISTCR)
                   (if (= nil TLISTR)
                    (setq TLISTCR (list (list DX (+ Z DY))))
                    (setq TLISTCR (append TLISTCR (list (list (+ (car (last TLISTR)) DX)  (+ (cadr (last TLISTR)) DY)))))
                   )
                   (setq TLISTCR (append TLISTCR (list (list (+ (car (last TLISTCR)) DX)  (+ (cadr (last TLISTCR)) DY)))))
                  )
                 )
                )
               )
               ((= "FR" (strcase (RFL:COLUMN NODE 1 ","))) ; FR = Fill Slope Right
                (progn
                 (setq DX nil DY nil)
                 (cond ((= "X" (strcase (substr (RFL:COLUMN NODE 2 ",") 1 1))) ; X = X offset
                        (progn
                         (setq DX (atof (substr (RFL:COLUMN NODE 2 ",") 2)))
                         (setq DY (atof (substr (RFL:COLUMN NODE 3 ",") 2)))
                        )
                       )
                       (T  ;  Cut/Fill only accept DX/DY offsets, if not will look for toe
                           ;  
                        (progn
                         (setq PA nil PB nil P nil)
                         (if (/= nil TLISTFR)
                          (setq PA (last TLISTFR))
                          (if (/= nil TLISTR)
                           (setq PA (last TLISTR))
                          )
                         )
                         (if PA
                          (progn
                           (setq P nil)
                           (setq PB (list (/ SWATH 2.0)
                                          (+ (cadr PA) (/ (- (/ SWATH 2.0) (car PA)) (atof (RFL:COLUMN NODE 2 ","))))
                                    )
                           )
                           (setq C (1- (length OSLIST)))
                           (while (> C 0)
                            (if (and (/= "NewLine" (nth C OSLIST))
                                     (/= "NewLine" (nth (1- C) OSLIST))
                                     (setq TMP (inters PA PB (nth (1- C) OSLIST) (nth C OSLIST)))
                                )
                             (setq P TMP)
                            )
                            (setq C (1- C))
                           )
                           (if (= nil P)
                            (progn
                             (setq PB (list (/ SWATH 2.0)
                                            (+ (cadr PA) (/ (- (/ SWATH 2.0) (car PA)) (atof (RFL:COLUMN NODE 3 ","))))
                                      )
                             )
                             (setq C (1- (length OSLIST)))
                             (while (> C 0)
                              (if (and (/= "NewLine" (nth C OSLIST))
                                       (/= "NewLine" (nth (1- C) OSLIST))
                                       (setq TMP (inters PA PB (nth (1- C) OSLIST) (nth C OSLIST)))
                                  )
                               (setq P TMP)
                              )
                              (setq C (1- C))
                             )
                            )
                           )
                           (if P
                            (setq DX (- (car P) (car PA))  ;*****
                                  DY (- (cadr P) (cadr PA))  ;*****
                            )
                           )
                          )
                         )
                        )
                       )
                 )
                 (if (and DX DY)
                  (if (= nil TLISTFR)
                   (if (= nil TLISTR)
                    (setq TLISTFR (list (list DX (+ Z DY))))
                    (setq TLISTFR (append TLISTFR (list (list (+ (car (last TLISTR)) DX)  (+ (cadr (last TLISTR)) DY)))))
                   )
                   (setq TLISTFR (append TLISTFR (list (list (+ (car (last TLISTFR)) DX)  (+ (cadr (last TLISTFR)) DY)))))
                  )
                 )
                )
               )
         )
        )
       )
       (if (ISABOVE (last TLISTL) OSLIST)
        (setq TLISTL (append TLISTL TLISTFL))
        (setq TLISTL (append TLISTL TLISTCL))
       )
       (if (ISABOVE (last TLISTR) OSLIST)
        (setq TLISTR (append TLISTR TLISTFR))
        (setq TLISTR (append TLISTR TLISTCR))
       )
;       (if (and (= nil TLISTCL) (/= nil TLISTFL))
;        (setq TLISTL (append TLISTL TLISTFL))
;        (if (and (/= nil TLISTCL) (= nil TLISTFL))
;         (setq TLISTL (append TLISTL TLISTCL))
;         (if (and (/= nil TLISTCL) (/= nil TLISTFL))
;          (if (> (car (last TLISTCL)) (car (last TLISTFL)))
;           (setq TLISTL (append TLISTL TLISTCL))
;           (setq TLISTL (append TLISTL TLISTFL))
;          )
;         )
;        )
;       )
;       (if (and (= nil TLISTCR) (/= nil TLISTFR))
;        (setq TLISTR (append TLISTR TLISTFR))
;        (if (and (/= nil TLISTCR) (= nil TLISTFR))
;         (setq TLISTR (append TLISTR TLISTCR))
;         (if (and (/= nil TLISTCR) (/= nil TLISTFR))
;          (if (< (car (last TLISTCR)) (car (last TLISTFR)))
;           (setq TLISTR (append TLISTR TLISTCR))
;           (setq TLISTR (append TLISTR TLISTFR))
;          )
;         )
;        )
;       )
       (if (and (= nil TLISTL) (= nil TLISTR))
        (setq TLIST nil)
        (if (= nil TLISTL)
         (setq TLIST TLISTR)
         (if (= nil TLISTR)
          (setq TLIST (reverse TLISTL))
          (if (< (distance (car TLISTL) (car TLISTR)) TOL)
           (setq TLIST (append (reverse TLISTL) (cdr TLISTR)))
           (setq TLIST (append (reverse TLISTL) TLISTR))
          )
         )
        )
       )
       (if (/= nil TLIST)
        (progn
         (command "._PLINE")
         (foreach P TLIST
          (command (list (+ (car PBASE) (car P))
                         (+ (cadr PBASE) (* VEXAG (- (cadr P) ZBASE)))
                   )
          )
         )
         (command "")
         (ADDHANDLE)
        )
       )
      )
     )
    )
    (if (= 1 DCIRCLE)
     (if (/= nil Z)
      (progn
       (command "._CIRCLE"
                (list (car PBASE) (+ (cadr PBASE) (* VEXAG (- Z ZBASE))))
                THEIGHT
       )
       (ADDHANDLE)
      )
     )
     (if (/= 0 DCIRCLE)
      (if (/= nil Z)
       (progn
        (entmake (list (cons 0 "INSERT")
                       (cons 2 DCIRCLE)
                       (list 10 (car PBASE) (+ (cadr PBASE) (* VEXAG (- Z ZBASE))) 0.0)
                       (cons 41 1.0)
                       (cons 42 VEXAG)
                       (cons 43 1.0)
                       (cons 50 0.0)
                       (cons 70 0)
                 )
        )
        (ADDHANDLE)
       )
      )
     )
    )
   )
   (if (= nil (assoc "ENTITIES" RFL:QSECTIONLIST))
    (setq RFL:QSECTIONLIST (append RFL:QSECTIONLIST (list (list "ENTITIES" HANDENTLIST))))
    (setq RFL:QSECTIONLIST (subst (list "ENTITIES" HANDENTLIST) (assoc "ENTITIES" RFL:QSECTIONLIST) RFL:QSECTIONLIST))
   )
   (if (= nil (assoc "PBASE" RFL:QSECTIONLIST))
    (setq RFL:QSECTIONLIST (append RFL:QSECTIONLIST (list (list "PBASE" PBASE))))
    (setq RFL:QSECTIONLIST (subst (list "PBASE" PBASE) (assoc "PBASE" RFL:QSECTIONLIST) RFL:QSECTIONLIST))
   )
   (if (= nil (assoc "STA" RFL:QSECTIONLIST))
    (setq RFL:QSECTIONLIST (append RFL:QSECTIONLIST (list (cons "STA" STA))))
    (setq RFL:QSECTIONLIST (subst (cons "STA" STA) (assoc "STA" RFL:QSECTIONLIST) RFL:QSECTIONLIST))
   )
  )
 )
)
(defun C:QS+ (/ ANGBASE ANGDIR CMDECHO INC NODE OSMODE ORTHOMODE REP)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0.0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 1)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq OSMODE (getvar "OSMODE"))
 (setvar "OSMODE" 0)
 (setq ORTHOMODE (getvar "ORTHOMODE"))
 (setvar "ORTHOMODE" 0)
 (if (/= nil RFL:QSECTIONLIST)
  (if (/= nil (assoc "PBASE" RFL:QSECTIONLIST))
   (progn
    (if (= nil (cdr (assoc "INC" RFL:QSECTIONLIST)))
     (progn
      (setq INC 20.0)
      (setq REP (getdist (strcat "\nStation increment <" (rtos INC) "> : ")))
      (if (/= nil REP) (setq INC REP))
      (setq RFL:QSECTIONLIST (append RFL:QSECTIONLIST (list (cons "INC" INC))))
     )
     (setq INC (cdr (assoc "INC" RFL:QSECTIONLIST)))
    )
    (foreach NODE (cadr (assoc "ENTITIES" RFL:QSECTIONLIST))
     (entdel (handent NODE))
    )
    (QSECTION (+ (cdr (assoc "STA" RFL:QSECTIONLIST)) INC)
              (cdr (assoc "SWATH" RFL:QSECTIONLIST))
              (cadr (assoc "PBASE" RFL:QSECTIONLIST))
              nil
              (cdr (assoc "VEXAG" RFL:QSECTIONLIST))
              (cdr (assoc "THEIGHT" RFL:QSECTIONLIST))
              (cdr (assoc "DCIRCLE" RFL:QSECTIONLIST))
              (cdr (assoc "OBSURFACE" RFL:QSECTIONLIST))
    )
   )
  )
 )
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
 (setvar "CMDECHO" CMDECHO)
 (setvar "OSMODE" OSMODE)
 (setvar "ORTHOMODE" ORTHOMODE)
 (eval nil)
)
(defun C:QS- (/ ANGBASE ANGDIR CMDECHO INC NODE OSMODE ORTHOMODE REP)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0.0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 1)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq OSMODE (getvar "OSMODE"))
 (setvar "OSMODE" 0)
 (setq ORTHOMODE (getvar "ORTHOMODE"))
 (setvar "ORTHOMODE" 0)
 (if (/= nil RFL:QSECTIONLIST)
  (if (/= nil (assoc "PBASE" RFL:QSECTIONLIST))
   (progn
    (if (= nil (cdr (assoc "INC" RFL:QSECTIONLIST)))
     (progn
      (setq INC 20.0)
      (setq REP (getdist (strcat "\nStation increment <" (rtos INC) "> : ")))
      (if (/= nil REP) (setq INC REP))
      (setq RFL:QSECTIONLIST (append RFL:QSECTIONLIST (list (cons "INC" INC))))
     )
     (setq INC (cdr (assoc "INC" RFL:QSECTIONLIST)))
    )
    (foreach NODE (cadr (assoc "ENTITIES" RFL:QSECTIONLIST))
     (entdel (handent NODE))
    )
    (QSECTION (- (cdr (assoc "STA" RFL:QSECTIONLIST)) INC)
              (cdr (assoc "SWATH" RFL:QSECTIONLIST))
              (cadr (assoc "PBASE" RFL:QSECTIONLIST))
              nil
              (cdr (assoc "VEXAG" RFL:QSECTIONLIST))
              (cdr (assoc "THEIGHT" RFL:QSECTIONLIST))
              (cdr (assoc "DCIRCLE" RFL:QSECTIONLIST))
              (cdr (assoc "OBSURFACE" RFL:QSECTIONLIST))
    )
   )
  )
 )
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
 (setvar "CMDECHO" CMDECHO)
 (setvar "OSMODE" OSMODE)
 (setvar "ORTHOMODE" ORTHOMODE)
 (eval nil)
)