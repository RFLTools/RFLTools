;
;
;     Program written by Robert Livingston, 2015-10-02
;
;     RFL:SIGHTDISTPROF is a utility for calculating the sight distance from a given Station along a profile
;
;     STA : Station of Eye
;     DIR : Direction, 1 = Up Chainage, -1 = Down Chainage
;     EYE : Eye Height
;     TARGET : Target Height
;     STEP : Increment
;     MAXDIST : Maximum distance to check
;
(defun RFL:SIGHTDISTPROF (STA DIR EYE TARGET STEP MAXDIST / CHECKSIGHT SIGHTDIST STASTART STAEND STOPFLAG ZLIST)
;(defun SIGHTDISTPROF (STA DIR EYE TARGET STEP MAXDIST)
 (defun CHECKSIGHT (/ C NODE S STA1 STA2 SLOPE Z1 Z2)
  (setq STA1 (caar ZLIST))
  (setq Z1 (+ (cadar ZLIST) EYE))
  (setq STA2 (car (last ZLIST)))
  (setq Z2 (+ (cadr (last ZLIST)) TARGET))
  (setq S (/ (- Z2 Z1) (- STA2 STA1)))
  (setq C 0)
  (while (and (< C (length ZLIST))
              (<= (cadr (nth C ZLIST)) (+ Z1 (* (- (car (nth C ZLIST)) STA1) S)))
         )
   (setq C (+ C 1))
  )
  (if (= C (length ZLIST))
   T
   nil
  )
 )
 (setq STA (float STA))
 (setq DIR (float DIR))
 (setq EYE (float EYE))
 (setq TARGET (float TARGET))
 (setq STEP (float STEP))
 (setq MAXDIST (float MAXDIST))
 (if (= nil RFL:PVILIST)
  (progn
   (princ "\n***** No Profile Defined! *****\n")
   nil
  )
  (if (or (< STA (caar RFL:PVILIST)) (> STA (car (last RFL:PVILIST))))
   (progn
    (princ "\n***** Station Outside Of Profile! *****\n")
    nil
   )
   (progn
    (setq STASTART STA)
    (setq ZLIST (list (list STA (RFL:ELEVATION STA))))
    (setq STA (+ STA (* DIR STEP)))
    (setq ZLIST (append ZLIST (list (list STA (RFL:ELEVATION STA)))))
    (while (and (CHECKSIGHT) 
                (<= (abs (- STA STASTART)) MAXDIST)
                (>= STA (+ (caar RFL:PVILIST) STEP))
                (<= STA (- (car (last RFL:PVILIST)) STEP))
           )
     
     (setq STA (+ STA (* DIR STEP)))
     (setq ZLIST (append ZLIST (list (list STA (RFL:ELEVATION STA)))))
    )
    (setq STA (- STA (* DIR STEP)))
    (abs (- STA STASTART))
   )
  )
 )  
)
