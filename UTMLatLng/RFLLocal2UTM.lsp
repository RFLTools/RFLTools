;
;
;     Program written by Robert Livingston, 2021-06-11
;
;     RFL:LOCAL2UTM is a collection of routines for converting to/from project to UTM
;
;
;     RFL:SURVEY list:
;          Local Northing
;          Local Easting
;          Grid Northing
;          Grid Easting
;          Scale Factor
;          Zone LNG (i.e. "10", Vancouver)
;          Zone LAT (i.e. "U", Vancouver)
(defun RFL:SETSURVEY (/ ENT ENTLIST GRIDEASTING GRIDNORTHING LOCALEASTING LOCALNORTHING SCALEFACTOR ZONELAT ZONELNG)
 (setq RFL:SURVEY nil)
 (if (setq ENT (car (entsel "\nSelect RFL Survey blck (or <return> to enter manually) : ")))
  (progn
   (setq ENTLIST (entget ENT))
   (if (and (= (cdr (assoc 0 ENTLIST)) "INSERT") (= (cdr (assoc 66 ENTLIST)) 1))
    (progn
     (setq ENT (entnext ENT))
     (setq ENTLIST (entget ENT))
     (while (= (cdr (assoc 0 ENTLIST)) "ATTRIB")
      (cond ((= (cdr (assoc 2 ENTLIST)) "LOCALNORTHING")
             (setq LOCALNORTHING (atof (cdr (assoc 1 ENTLIST))))
            )
            ((= (cdr (assoc 2 ENTLIST)) "LOCALEASTING")
             (setq LOCALEASTING (atof (cdr (assoc 1 ENTLIST))))
            )
            ((= (cdr (assoc 2 ENTLIST)) "GRIDNORTHING")
             (setq GRIDNORTHING (atof (cdr (assoc 1 ENTLIST))))
            )
            ((= (cdr (assoc 2 ENTLIST)) "GRIDEASTING")
             (setq GRIDEASTING (atof (cdr (assoc 1 ENTLIST))))
            )
            ((= (cdr (assoc 2 ENTLIST)) "SCALEFACTOR")
             (setq SCALEFACTOR (atof (cdr (assoc 1 ENTLIST))))
            )
            ((= (cdr (assoc 2 ENTLIST)) "ZONELNG")
             (setq ZONELNG (itoa (atoi (cdr (assoc 1 ENTLIST)))))
            )
            ((= (cdr (assoc 2 ENTLIST)) "ZONELAT")
             (setq ZONELAT (strcase (cdr (assoc 1 ENTLIST))))
            )
      )
      (setq ENT (entnext ENT))
      (setq ENTLIST (entget ENT))
     )
     (setq RFL:SURVEY (list LOCALNORTHING LOCALEASTING GRIDNORTHING GRIDEASTING SCALEFACTOR ZONELNG ZONELAT))
    )
    (princ "\nNot a valid block!")
   )
  )
  (setq RFL:SURVEY (list (getreal "\nLocal Northing : ")
                         (getreal "\nLocal Easting : ")
                         (getreal "\nGrid Northing : ")
                         (getreal "\nGrid Easting : ")
                         (getreal "\nScale Factor : ")
                         (itoa (atoi (getstring "\nZone Longitude (i.e. \"10\", Vancouver) : ")))
                         (strcase (getstring "\nZone Latitude (i.e. \"U\", Vancouver) : "))
                    )
  )
 )
 RFL:SURVEY
)
(defun RFL:SURVEY2UTM (P)
 (if (not RFL:SURVEY) (RFL:SETSURVEY))
 (list (+ (/ (- (car P) (nth 1 RFL:SURVEY)) (nth 4 RFL:SURVEY)) (nth 3 RFL:SURVEY))
       (+ (/ (- (cadr P) (nth 0 RFL:SURVEY)) (nth 4 RFL:SURVEY)) (nth 2 RFL:SURVEY))
 )
)
(defun RFL:UTM2SURVEY (P)
 (if (not RFL:SURVEY) (RFL:SETSURVEY))
 (list (+ (* (- (car P) (nth 3 RFL:SURVEY)) (nth 4 RFL:SURVEY)) (nth 1 RFL:SURVEY))
       (+ (* (- (cadr P) (nth 2 RFL:SURVEY)) (nth 4 RFL:SURVEY)) (nth 0 RFL:SURVEY))
 )
)
(defun RFL:MAPLINKUTM (P / PTMP)
 (if (not RFL:SURVEY) (RFL:SETSURVEY))
 (setq PTMP (RFL:UTM2LATLNG P (nth 5 RFL:SURVEY) (nth 6 RFL:SURVEY)))
 (strcat "https://maps.google.com/?q=" (rtos (car PTMP) 2 8) "," (rtos (cadr PTMP) 2 8))
)
(defun RFL:MAPLINK (P / PTMP)
 (if (not RFL:SURVEY) (RFL:SETSURVEY))
 (setq PTMP (RFL:UTM2LATLNG (RFL:SURVEY2UTM P) (nth 5 RFL:SURVEY) (nth 6 RFL:SURVEY)))
 (strcat "https://maps.google.com/?q=" (rtos (car PTMP) 2 8) "," (rtos (cadr PTMP) 2 8))
)
(defun C:MAPLINK ()
 (command "_browser" (RFL:MAPLINK (getpoint "\nSelect point : ")))
)
(defun C:MAPLINKUTM ()
 (command "_browser" (RFL:MAPLINKUTM (getpoint "\nSelect point : ")))
)