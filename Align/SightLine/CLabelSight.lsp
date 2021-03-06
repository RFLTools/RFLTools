;
;
;     Program written by Robert Livingston, 2015-10-02
;
;     C:LABELSIGHT is a utility for drawing the sight line from a given Station along a profile
;
(defun C:LABELSIGHT (/ D ENT PREVENT P ORTHOMODE OSMODE STA)
 (setq ORTHOMODE (getvar "ORTHOMODE"))
 (setq OSMODE (getvar "OSMODE"))
 (setq PREVENT nil)
 (if (= RFL:LABELSIGHTEYE nil)
  (progn
   (setq RFL:LABELSIGHTEYE (getdist "\nEye Height (1.05) : "))
   (if (= nil RFL:LABELSIGHTEYE) (setq RFL:LABELSIGHTEYE 1.05))
  )
 )
 (if (= RFL:LABELSIGHTTARGET nil)
  (progn
   (setq RFL:LABELSIGHTTARGET (getdist "\nTarget Height (0.38) : "))
   (if (= nil RFL:LABELSIGHTTARGET) (setq RFL:LABELSIGHTTARGET 0.38))
  )
 )
 (if (= RFL:LABELSIGHTMAX nil)
  (progn
   (setq RFL:LABELSIGHTMAX (getdist "\nMaximum Sight Check (250.0) : "))
   (if (= nil RFL:LABELSIGHTMAX) (setq RFL:LABELSIGHTMAX 250.0))
  )
 )
 (setq P (getpoint "\nSelect Profile Point (<return> to reset eye and target) : "))
 (if (= nil P)
  (progn
   (setq RFL:LABELSIGHTEYE nil)
   (setq RFL:LABELSIGHTTARGET nil)
   (setq RFL:LABELSIGHTMAX nil)
   (C:LABELSIGHT)
  )
  (progn
   (setvar "ORTHOMODE" 0)
   (setvar "OSMODE" 0)
   (setq STA (car (RFL:VPP P)))
   (if (or (= nil STA)
           (< STA (caar RFL:PVILIST))
           (> STA (car (last RFL:PVILIST)))
       )
    (progn
     (princ "\n*** Station out of range! ***\n")
     nil
    )
    (progn
     (setq D (RFL:SIGHTDISTPROF STA -1 RFL:LABELSIGHTEYE RFL:LABELSIGHTTARGET 1.0 RFL:LABELSIGHTMAX))
     (if (/= nil D)
      (progn
       (entmake)
       (entmake (list (cons 0 "LWPOLYLINE")
                      (cons 100 "AcDbEntity")
                      (cons 100 "AcDbPolyline")
                      (cons 90 4)
                      (append (list 10) (RFL:PROFPOINT (- STA D) (RFL:ELEVATION (- STA D))))
                      (append (list 10) (RFL:PROFPOINT (- STA D) (+ (RFL:ELEVATION (- STA D)) RFL:LABELSIGHTTARGET)))
                      (append (list 10) (RFL:PROFPOINT STA (+ (RFL:ELEVATION STA) RFL:LABELSIGHTEYE)))
                      (append (list 10) (RFL:PROFPOINT STA (RFL:ELEVATION STA)))
                )
       )
       (setq ENT (entlast))
       (RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
      )
     )
     (setq D (RFL:SIGHTDISTPROF STA 1 RFL:LABELSIGHTEYE RFL:LABELSIGHTTARGET 1.0 RFL:LABELSIGHTMAX))
     (if (/= nil D)
      (progn
       (entmake)
       (entmake (list (cons 0 "LWPOLYLINE")
                      (cons 100 "AcDbEntity")
                      (cons 100 "AcDbPolyline")
                      (cons 90 4)
                      (append (list 10) (RFL:PROFPOINT STA (RFL:ELEVATION STA)))
                      (append (list 10) (RFL:PROFPOINT STA (+ (RFL:ELEVATION STA) RFL:LABELSIGHTEYE)))
                      (append (list 10) (RFL:PROFPOINT (+ STA D) (+ (RFL:ELEVATION (+ STA D)) RFL:LABELSIGHTTARGET)))
                      (append (list 10) (RFL:PROFPOINT (+ STA D) (RFL:ELEVATION (+ STA D))))
                )
       )
       (setq ENT (entlast))
       (RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
      )
     )
    )
   )
  )
 )
 (setvar "ORTHOMODE" ORTHOMODE)
 (setvar "OSMODE" OSMODE)
)