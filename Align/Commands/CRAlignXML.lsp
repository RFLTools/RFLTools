;
;
;     Program written by Robert Livingston, 2021-06-09
;
;     RAlignXML is a collection of routines for reading xml files and converting to RFL Alignments
;
;
(defun RFL:STRLMATCHLAST (STRL1 STRL2 / MATCH)
 (setq MATCH T)
 (setq STRL1 (reverse STRL1))
 (setq STRL2 (reverse STRL2))
 (while (and MATCH STRL1)
  (if (/= (car STRL1) (car STRL2)) (setq MATCH nil))
  (setq STRL1 (cdr STRL1) STRL2 (cdr STRL2))
 )
 MATCH
)
(defun RFL:STRLMATCH (STRL1 STRL2 / MATCH)
 (setq MATCH T)
 (while (and MATCH (or STRL1 STRL2))
  (if (/= (car STRL1) (car STRL2)) (setq MATCH nil))
  (setq STRL1 (cdr STRL1) STRL2 (cdr STRL2))
 )
 MATCH
)
(defun RFL:GETXMLKEY (STRIN INFILE / CH STRINL STROUTL)
 (setq STROUTL nil)
 (setq CH T)
 (if (and STRIN (setq STRINL (vl-string->list STRIN)))
  (progn
   (setq STRINL (append (vl-string->list "<") STRINL (vl-string->list " ")))
   (while (and CH (not (RFL:STRLMATCH STRINL STROUTL)))
    (if (setq CH (read-char INFILE)) (setq STROUTL (append STROUTL (list CH))))
    (if (> (length STROUTL) (length STRINL)) (setq STROUTL (cdr STROUTL)))
   )
   (while (and CH (not (RFL:STRLMATCHLAST (append (vl-string->list "</") (vl-string->list STRIN) (vl-string->list ">")) STROUTL)))
    (if (setq CH (read-char INFILE)) (setq STROUTL (append STROUTL (list CH))))
   )
  )
 )
 (vl-list->string STROUTL)
)
(defun RFL:GETXMLVALUE (KEY STR / C)
 (if (setq C (vl-string-search KEY STR))
  (progn
   (setq STR (substr STR (+ C (strlen KEY))))
   (setq STR (substr STR (+ (vl-string-search "\"" STR) 2)))
   (setq STR (substr STR 1 (vl-string-search "\"" STR)))
  )
  nil
 )
)
(defun RFL:XMLGETALIGNNAMES (INFILENAME / *error* C INFILE INLINE NAME NAMELIST STR)
 (defun *error* (msg)
  (close INFILE)
  (print msg)
 )
 (setq NAMELIST nil)
 (if (setq INFILE (open INFILENAME "r"))
  (progn
   (while (setq INLINE (read-line INFILE))
    (if (vl-string-search "<Alignment " INLINE)
     (if (setq STR (RFL:GETXMLVALUE "name=" INLINE))
      (setq NAMELIST (append NAMELIST (list STR)))
     )
    )
   )
   (close INFILE)
  )
  (princ "\nProblem reading file!")
 )
 NAMELIST
)
(defun RFL:RALIGNXML (ALIGNNAME INFILENAME / *error* A2 ALIGNLIST ANG BULGE DIR L LTAN P1 P2 PLT PLTST PST R R1 R2 RSPACE STA STAN THETA X Y)
 (defun *error* (msg)
  (close INFILE)
  (print msg)
 )
 (defun RSPACE (INLINE)
  (while (or (= " " (substr INLINE 1 1))
             (= "\t" (substr INLINE 1 1))
         )
   (setq INLINE (substr INLINE 2))
  )
  INLINE
 )
 (setq ALIGNLIST nil)
 (if (setq INFILE (open INFILENAME "r"))
  (progn
   (while (setq INLINE (read-line INFILE))
    (if (and (vl-string-search "<Alignment " INLINE)
             (vl-string-search ALIGNNAME INLINE)
        )
     (progn
      (setq STA (atof (RFL:GETXMLVALUE "staStart=" INLINE)))
      (while (not (vl-string-search "</Alignment>" (setq INLINE (read-line INFILE))))
       (cond ((vl-string-search "<Line " INLINE)
              (progn
               (setq P1 nil P2 nil)
               (setq L (atof (RFL:GETXMLVALUE "length=" INLINE)))
               (while (not (vl-string-search "</Line>" (setq INLINE (RSPACE (read-line INFILE)))))
                (cond ((vl-string-search "<Start>" INLINE)
                       (setq P1 (list (atof (RFL:COLUMN INLINE 2 " "))
                                      (atof (substr INLINE 8))
                                )
                       )
                      )
                      ((vl-string-search "<End>" INLINE)
                       (setq P2 (list (atof (RFL:COLUMN INLINE 2 " "))
                                      (atof (substr INLINE 6))
                                )
                       )
                      )
                )
               )
               (if (and P1 P2)
                (setq ALIGNLIST (append ALIGNLIST (list (list STA P1 P2 0.0))) STA (+ STA L))
               )
              )
             )
             ((vl-string-search "<Curve " INLINE)
              (progn
               (setq P1 nil P2 nil)
               (setq L (atof (RFL:GETXMLVALUE "length=" INLINE)))
               (setq R (atof (RFL:GETXMLVALUE "radius=" INLINE)))
               (setq DIR (RFL:GETXMLVALUE "rot=" INLINE))
               (while (not (vl-string-search "</Curve>" (setq INLINE (RSPACE (read-line INFILE)))))
                (cond ((vl-string-search "<Start>" INLINE)
                       (setq P1 (list (atof (RFL:COLUMN INLINE 2 " "))
                                      (atof (substr INLINE 8))
                                )
                       )
                      )
                      ((vl-string-search "<End>" INLINE)
                       (setq P2 (list (atof (RFL:COLUMN INLINE 2 " "))
                                      (atof (substr INLINE 6))
                                )
                       )
                      )
                )
               )
               (if (and P1 P2)
                (if (= "ccw" DIR)
                 (setq ALIGNLIST (append ALIGNLIST (list (list STA P1 P2 (RFL:BULGE P1 P2 R)))) STA (+ STA L))
                 (setq ALIGNLIST (append ALIGNLIST (list (list STA P1 P2 (* -1.0 (RFL:BULGE P1 P2 R))))) STA (+ STA L))
                )
               )
              )
             )
             ((vl-string-search "<Spiral " INLINE)
              (progn
               (setq P1 nil P2 nil)
               (setq L (atof (RFL:GETXMLVALUE "length=" INLINE)))
               (setq R1 (atof (RFL:GETXMLVALUE "radiusStart=" INLINE)))
               (setq R2 (atof (RFL:GETXMLVALUE "radiusEnd=" INLINE)))
               (if (or (= R1 0.0) (= R2 0.0))
                (setq R (max R1 R2))
                (setq R (min R1 R2))
               )
               (setq DIR (RFL:GETXMLVALUE "rot=" INLINE))
               (while (not (vl-string-search "</Spiral>" (setq INLINE (RSPACE (read-line INFILE)))))
                (cond ((vl-string-search "<Start>" INLINE)
                       (setq P1 (list (atof (RFL:COLUMN INLINE 2 " "))
                                      (atof (substr INLINE 8))
                                )
                       )
                      )
                      ((vl-string-search "<End>" INLINE)
                       (setq P2 (list (atof (RFL:COLUMN INLINE 2 " "))
                                      (atof (substr INLINE 6))
                                )
                       )
                      )
                      ((vl-string-search "<PI>" INLINE)
                       (setq P (list (atof (RFL:COLUMN INLINE 2 " "))
                                     (atof (substr INLINE 5))
                               )
                       )
                      )
                )
               )
               (if (and P1 P2 P)
                (progn
                 (if (= R R1)
                  (setq PST P1)
                  (setq PST P2)
                 )
                 (if (or (= R1 0.0) (= R2 0.0))
                  (setq L0 0.0 A2 (* R L))
                  (setq L0 (/ L (- (/ (max R1 R2) (min R1 R2)) 1.0)) A2 (* R (+ L L0)))
                 )
                 (setq ANG (angle PST P))
                 (setq THETA (/ (+ L L0) (* 2.0 R)))
                 (setq STAN (/ (* R (RFL:SPIRALFYR THETA)) (sin THETA)))
                 (setq LTAN (- (* R (RFL:SPIRALFXR THETA)) (* STAN (cos THETA))))
                 (setq PLTST (list (+ (car PST) (* STAN (cos ANG)))
                                   (+ (cadr PST) (* STAN (sin ANG)))
                             )
                 )
                 (setq X (* R (RFL:SPIRALFXR THETA)))
                 (setq Y (* R (RFL:SPIRALFYR THETA)))
                  (if (< (distance P2 PST) (distance P1 PST))
                   (if (= DIR "ccw")
                    (setq ANG (- ANG THETA))
                    (setq ANG (+ ANG THETA))
                   )
                   (if (= DIR "ccw")
                    (setq ANG (+ ANG THETA))
                    (setq ANG (- ANG THETA))
                   )
                  )
                 (setq PLT (list (+ (car PLTST) (* LTAN (cos ANG)))
                                 (+ (cadr PLTST) (* LTAN (sin ANG)))
                           )
                 )
                 (setq ALIGNLIST (append ALIGNLIST (list (list STA P1 P2 (list PLT PLTST PST L0)))) STA (+ STA L))
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
 ALIGNLIST
)
(defun C:RALIGNXML (/ C INFILENAME ALIGNNAME)
 (setq INFILENAME (getfiled "Select XML file for input" "" "xml" 2))
 (if (setq RFL:ALIGNNAMELIST (RFL:XMLGETALIGNNAMES INFILENAME))
  (progn
   (setq C 0)
   (foreach ALIGNNAME RFL:ALIGNNAMELIST
    (setq C (1+ C))
    (princ (strcat (itoa C) " - " ALIGNNAME "\n"))
   )
   (setq C (getint "\nEnter alignment number : "))
   (setq ALIGNNAME (nth (1- C) RFL:ALIGNNAMELIST))
   (setq RFL:ALIGNLIST (RFL:RALIGNXML ALIGNNAME INFILENAME))
  )
 )
 nil
)