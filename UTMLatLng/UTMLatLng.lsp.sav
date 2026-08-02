;
;
;     Program written by Robert Livingston, 2018-12-05
;
;     UTMLatLng.lsp is a collection of utilities for converting between UTM and Lattidude/Longitude
;
;     Derived from: https://github.com/shahid28/utm-latlng/blob/master/UTMLatLng.js
;
;
(setq RFL:LATLNGDATUMNAME "WGS 84")
(setq RFL:LATLNGSTATUS nil)
(setq RFL:LATLNGPRECISION 4)
; PLL = (list LATITUDE LONGITUDE)
; PUTM = (list EASTING NORTHING)
(defun RFL:LATLNG2UTM (PLL / A C EASTING ECCPRIMESQUARED LAT LATRAD LNG LNGORIGIN LNGORIGINRAD LNGRAD LNGTMP M N NORTHING PUTM TN UTMZONE ZONENUMBER)
 (setq PUTM nil)
 (setq LAT (car PLL) LNG (cadr PLL))
 (if (and LAT LNG (not (RFL:SETELLIPSOID RFL:LATLNGDATUMNAME)))
  (progn
   (setq LNGTMP LNG)
   (setq LATRAD (* LAT (/ pi 180.0)))
   (setq LNGRAD (* LNG (/ pi 180.0)))
   (if (and (>= LNGTMP 8) (<= LNGTMP 13) (> LAT 54.5) (< LAT 58))
    (setq ZONENUMBER 32)
    (if (and (>= LAT 56.0) (< LAT 64.0) (>= LNGTMP 3.0) (< LNGTMP 12.0))
     (setq ZONENUMBER 32)
     (progn
      (setq ZONENUMBER (+ (/ (+ LNGTMP 180.0) 6.0) 1.0))
      (if (and (>= LAT 72.0) (< LAT 84.0))
       (if (and (>= LNGTMP 0.0) (< LNGTMP 9.0))
        (setq ZONENUMBER 31)
        (if (and (>= LNGTMP 9.0) (< LNGTMP 21.0))
         (setq ZONENUMBER 33)
         (if (and (>= LNGTMP 21.0) (< LNGTMP 33.0))
          (setq ZONENUMBER 35)
          (if (and (>= LNGTMP 33.0) (< LNGTMP 42.0))
           (setq ZONENUMBER 37)
          )
         )
        )
       )
      )
     )
    )
   )
   (setq ZONENUMBER (fix ZONENUMBER))
   (setq LNGORIGIN (+ (- (* (- ZONENUMBER 1) 6) 180) 3))
   (setq LNGORIGINRAD (* LNGORIGIN (/ pi 180.0)))
   (setq UTMZONE (RFL:GETUTMLETTERDESIGNATOR LAT))
   (setq ECCPRIMESQUARED (/ RFL:LATLNGECCSQUARED (- 1.0 RFL:LATLNGECCSQUARED)))
   (setq N (/ RFL:LATLNGA (sqrt (- 1.0 (* RFL:LATLNGECCSQUARED (expt (sin LATRAD) 2))))))
   (setq TN (expt (RFL:TAN LATRAD) 2))
   (setq C (* ECCPRIMESQUARED (expt (cos LATRAD) 2)))
   (setq A (* (cos LATRAD) (- LNGRAD LNGORIGINRAD)))
   (setq M (* RFL:LATLNGA
              (- (+ (- (* (- 1.0 (/ RFL:LATLNGECCSQUARED 4.0) (/ (* 3.0 (expt RFL:LATLNGECCSQUARED 2)) 64.0) (/ (* 5.0 (expt RFL:LATLNGECCSQUARED 3)) 256.0)) LATRAD)
                       (* (+ (/ (* 3.0 RFL:LATLNGECCSQUARED) 8.0) (/ (* 3.0 (expt RFL:LATLNGECCSQUARED 2)) 32.0) (/ (* 45.0 (expt RFL:LATLNGECCSQUARED 3)) 1024.0)) (sin (* 2.0 LATRAD)))
                    )
                    (* (+ (/ (* 15.0 (expt RFL:LATLNGECCSQUARED 2)) 256.0) (/ (* 45.0 (expt RFL:LATLNGECCSQUARED 3)) 1024.0)) (sin (* 4.0 LATRAD)))
                 )
                 (* (/ (* 35.0 (expt RFL:LATLNGECCSQUARED 3)) 3072.0) (sin (* 6.0 LATRAD)))
              )
           )
   )
   (setq EASTING (+ (* 0.9996
                       N
                       (+ A
                          (* (+ (- 1.0 TN) C) (/ (expt A 3) 6.0))
                          (* (- (+ (- 5.0 (* 18.0 TN)) (expt TN 2) (* 72.0 C)) (* 58.0 ECCPRIMESQUARED)) (/ (expt A 5) 120.0))
                       )
                    )
                    500000.0
                 )
   )
   (setq NORTHING (* 0.9996
                     (+ M
                        (* N
                           (RFL:TAN LATRAD)
                           (+ (/ (expt A 2) 2.0)
                              (/ (* (+ (- 5.0 TN) (* 9.0 C) (* 4.0 (expt C 2))) (expt A 4)) 24.0)
                              (/ (* (- (+ (- 61.0 (* 58.0 TN)) (expt TN 2) (* 600.0 C)) (* 330.0 ECCPRIMESQUARED)) (expt A 6)) 720.0)
                           )
                        )
                     )
                  )
   )
   (if (< LAT 0.0) (setq NORTHING (+ NORTHING 10000000.0)))
   (setq NORTHING (/ (float (fix (+ (* NORTHING (expt 10 RFL:LATLNGPRECISION)) 0.5))) (expt 10 RFL:LATLNGPRECISION)))
   (setq EASTING (/ (float (fix (+ (* EASTING (expt 10 RFL:LATLNGPRECISION)) 0.5))) (expt 10 RFL:LATLNGPRECISION)))
   (setq PUTM (list EASTING NORTHING))
  )
 )
 (princ (strcat "\nUTM Zone : " (itoa ZONENUMBER) UTMZONE "\n"))
 (cons (strcat (itoa ZONENUMBER) UTMZONE) PUTM)
)
; PUTM = (list EASTING NORTHING)
; ZONENUMBER = string
; ZONELETTER = string
; PLL = (list LATITUDE LONGITUDE)
(defun RFL:UTM2LATLNG (PUTM ZONENUMBER ZONELETTER / C1 D E1 EASTING ECCPRIMESQUARED LAT LATRAD LNG LNGRAD LNGORIGIN M MU N1 NORTHING NORTHERN PHI1 PHI1RAD PLL R1 T1 X Y)
 (setq EASTING (car PUTM) NORTHING (cadr PUTM) ZONENUMBER (atoi ZONENUMBER) ZONELETTER (strcase ZONELETTER))
 (setq PLL nil)
 (if (and EASTING NORTHING ZONENUMBER ZONELETTER (not (RFL:SETELLIPSOID RFL:LATLNGDATUMNAME)))
  (progn
   (setq E1 (/ (- 1.0 (sqrt (- 1.0 RFL:LATLNGECCSQUARED))) (+ 1.0 (sqrt (- 1.0 RFL:LATLNGECCSQUARED)))))
   (setq X (- EASTING 500000.0))
   (setq Y NORTHING)
   (if (>= ZONELETTER "N")
    (setq NORTHERN 1)
    (setq NORTHERN 0 Y (- Y 10000000.0))
   )
   (setq LNGORIGIN (+ (- (* (- ZONENUMBER 1) 6) 180) 3))
   (setq ECCPRIMESQUARED (/ RFL:LATLNGECCSQUARED (- 1.0 RFL:LATLNGECCSQUARED)))
   (setq M (/ Y 0.9996))
   (setq MU (/ M (* RFL:LATLNGA (- 1.0 (/ RFL:LATLNGECCSQUARED 4.0) (/ (* 3.0 (expt RFL:LATLNGECCSQUARED 2)) 64.0) (/ (* 5.0 (expt RFL:LATLNGECCSQUARED 3)) 256.0)))))
   (setq PHI1RAD (+ MU
                    (* (- (/ (* 3.0 E1) 2.0) (/ (* 27.0 (expt E1 3)) 32.0)) (sin (* 2.0 MU)))
                    (* (- (/ (* 21.0 (expt E1 2)) 16.0) (/ (* 55.0 (expt E1 4)) 32.0)) (sin (* 4.0 MU)))
                    (* (/ (* 151.0 (expt E1 3)) 96.0) (sin (* 6.0 MU)))
                 )
   )
   (setq PHI1 (* PHI1RAD (/ 180.0 pi)))
   (setq N1 (/ RFL:LATLNGA (sqrt (- 1.0 (* RFL:LATLNGECCSQUARED (expt (sin PHI1RAD) 2))))))
   (setq T1 (expt (RFL:TAN PHI1RAD) 2))
   (setq C1 (* RFL:LATLNGECCSQUARED (expt (cos PHI1RAD) 2)))
   (setq R1 (/ (* RFL:LATLNGA (- 1.0 RFL:LATLNGECCSQUARED)) (expt (- 1.0 (* RFL:LATLNGECCSQUARED (expt (sin PHI1RAD) 2))) 1.5)))
   (setq D (/ X (* N1 0.9996)))
   (setq LATRAD (- PHI1RAD
                   (* (/ (* N1 (RFL:TAN PHI1RAD)) R1)
                      (+ (- (/ (expt D 2) 2.0)
                            (/ (* (- (+ 5.0 (* 3.0 T1) (* 10.0 C1)) (* 4.0 (expt C1 2)) (* 9.0 ECCPRIMESQUARED)) (expt D 4)) 24.0)
                         )
                         (/ (* (- (+ 61.0 (* 90.0 T1) (* 298.0 C1) (* 45.0 (expt T1 2))) (* 252.0 ECCPRIMESQUARED) (* 3.0 (expt C1 2))) (expt D 6)) 720.0)
                      )
                   )
                )
   )
   (setq LAT (* LATRAD (/ 180.0 pi)))
   (setq LNGRAD (/ (+ (- D
                         (/ (* (+ 1.0 (* 2.0 T1) C1) (expt D 3)) 6.0)
                      )
                      (/ (* (+ (- (+ (- 5.0 (* 2.0 C1)) (* 28.0 T1)) (* 3.0 (expt C1 2))) (* 8.0 ECCPRIMESQUARED) (* 24.0 (expt T1 2))) (expt D 5)) 120.0)
                   )
                   (cos PHI1RAD)
                )
   )
   (setq LNG (+ LNGORIGIN (* LNGRAD (/ 180.0 pi))))
   (setq PLL (list LAT LNG))
  )
 )
 PLL
)
(defun RFL:GETUTMLETTERDESIGNATOR (LAT)
 (cond ((and (>= 84 LAT) (>= LAT 72))
        "X"
       )
       ((and (> 72 LAT) (>= LAT 64))
        "W"
       )
       ((and (> 64 LAT) (>= LAT 56))
        "V"
       )
       ((and (> 56 LAT) (>= LAT 48))
        "U"
       )
       ((and (> 48 LAT) (>= LAT 40))
        "T"
       )
       ((and (> 40 LAT) (>= LAT 32))
        "S"
       )
       ((and (> 32 LAT) (>= LAT 24))
        "R"
       )
       ((and (> 24 LAT) (>= LAT 16))
        "Q"
       )
       ((and (> 16 LAT) (>= LAT 8))
        "P"
       )
       ((and (> 8 LAT) (>= LAT 0))
        "N"
       )
       ((and (> 0 LAT) (>= LAT -8))
        "M"
       )
       ((and (> -8 LAT) (>= LAT -16))
        "L"
       )
       ((and (> -16 LAT) (>= LAT -24))
        "K"
       )
       ((and (> -24 LAT) (>= LAT -32))
        "J"
       )
       ((and (> -32 LAT) (>= LAT -40))
        "H"
       )
       ((and (> -40 LAT) (>= LAT -48))
        "G"
       )
       ((and (> -48 LAT) (>= LAT -56))
        "F"
       )
       ((and (> -56 LAT) (>= LAT -64))
        "E"
       )
       ((and (> -64 LAT) (>= LAT -72))
        "D"
       )
       ((and (> -72 LAT) (>= LAT -80))
        "C"
       )
       (T
        "Z"
       )
 )
)
(defun RFL:SETELLIPSOID (NAME)
 (setq RFL:LATLNGSTATUS nil)
 (cond ((= NAME "Airy")
        (setq RFL:LATLNGA 6377563
              RFL:LATLNGECCSQUARED 0.00667054
        )
       )
       ((= NAME "Australian National")
        (setq RFL:LATLNGA 6378160
              RFL:LATLNGECCSQUARED 0.006694542
        )
       )
       ((= NAME "Bessel 1841")
        (setq RFL:LATLNGA 6377397
              RFL:LATLNGECCSQUARED 0.006674372
        )
       )
       ((= NAME "Bessel 1841 Nambia")
        (setq RFL:LATLNGA 6377484
              RFL:LATLNGECCSQUARED 0.006674372
        )
       )
       ((= NAME "Clarke 1866")
        (setq RFL:LATLNGA 
              RFL:LATLNGECCSQUARED 
        )
       )
       ((= NAME "")
        (setq RFL:LATLNGA 6378206
              RFL:LATLNGECCSQUARED 0.006768658
        )
       )
       ((= NAME "Clarke 1880")
        (setq RFL:LATLNGA 6378249
              RFL:LATLNGECCSQUARED 0.006803511
        )
       )
       ((= NAME "Everest")
        (setq RFL:LATLNGA 6377276
              RFL:LATLNGECCSQUARED 0.006637847
        )
       )
       ((= NAME "Fischer 1960 Mercury")
        (setq RFL:LATLNGA 6378166
              RFL:LATLNGECCSQUARED 0.006693422
        )
       )
       ((= NAME "Fischer 1968")
        (setq RFL:LATLNGA 6378150
              RFL:LATLNGECCSQUARED 0.006693422
        )
       )
       ((= NAME "GRS 1967")
        (setq RFL:LATLNGA 6378160
              RFL:LATLNGECCSQUARED 0.006694605
        )
       )
       ((= NAME "GRS 1980")
        (setq RFL:LATLNGA 6378137
              RFL:LATLNGECCSQUARED 0.00669438
        )
       )
       ((= NAME "Helmert 1906")
        (setq RFL:LATLNGA 6378200
              RFL:LATLNGECCSQUARED 0.006693422
        )
       )
       ((= NAME "Hough")
        (setq RFL:LATLNGA 6378270
              RFL:LATLNGECCSQUARED 0.00672267
        )
       )
       ((= NAME "International")
        (setq RFL:LATLNGA 6378388
              RFL:LATLNGECCSQUARED 0.00672267
        )
       )
       ((= NAME "Krassovsky")
        (setq RFL:LATLNGA 6378245
              RFL:LATLNGECCSQUARED 0.006693422
        )
       )
       ((= NAME "Modified Airy")
        (setq RFL:LATLNGA 6377340
              RFL:LATLNGECCSQUARED 0.00667054
        )
       )
       ((= NAME "Modified Everest")
        (setq RFL:LATLNGA 6377304
              RFL:LATLNGECCSQUARED 0.006637847
        )
       )
       ((= NAME "Modified Fischer 1960")
        (setq RFL:LATLNGA 6378155
              RFL:LATLNGECCSQUARED 0.006693422
        )
       )
       ((= NAME "South American 1969")
        (setq RFL:LATLNGA 6378160
              RFL:LATLNGECCSQUARED 0.006694542
        )
       )
       ((= NAME "WGS 60")
        (setq RFL:LATLNGA 6378165
              RFL:LATLNGECCSQUARED 0.006693422
        )
       )
       ((= NAME "WGS 66")
        (setq RFL:LATLNGA 6378145
              RFL:LATLNGECCSQUARED 0.006694542
        )
       )
       ((= NAME "WGS 72")
        (setq RFL:LATLNGA 6378135
              RFL:LATLNGECCSQUARED 0.006694318
        )
       )
       ((= NAME "ED50")
        (setq RFL:LATLNGA 6378388
              RFL:LATLNGECCSQUARED 0.00672267
        )
       )
       ((or (= NAME "WGS 84") (= NAME "EUREF89") (= NAME "ETRS89"))
        (setq RFL:LATLNGA 6378137
              RFL:LATLNGECCSQUARED 0.00669438
        )
       )
       (T
        (setq RFL:LATLNGSTATUS T)
       )
 )
 RFL:LATLNGSTATUS
)