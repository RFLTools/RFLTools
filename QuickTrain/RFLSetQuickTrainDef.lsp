;
;
;     Program written by Robert Livingston, 2013-05-01
;
;     RFL:SETQUICKTRAINDEF sets the parameters for QuickTrain.
;
;
(defun RFL:SETQUICKTRAINDEF (/ ENT ENTLIST ENTSET ATT VAL TITLE
                               RAILCL GAUGE WB OH LABELIOX LABELIOY LABELSX LABELOSX LABELOSY
                               LTITLEH LTEXTH LLAYERS DLAYERS TLAYERS REFSECTION SECTION SECTINC
                               LSECTION RSECTION GROUPTOL)
 (setq RFL:QUICKTRAINDEF nil)
 (setq ENTSET (ssget "X" (list (cons 0 "INSERT") (cons 2 "RFLTRAIN"))))
 (if (> (sslength ENTSET) 1)
  (setq ENT (car (entsel "\nMore than one RFLTrain block exist in the drawing, please select : ")))
  (setq ENT (ssname ENTSET 0))
 )
 (if (/= nil ENT)
  (progn
   (setq ENTLIST (entget ENT))
   (if (and (= "RFLTRAIN" (strcase (cdr (assoc 2 ENTLIST)))) (= 1 (cdr (assoc 66 ENTLIST))))
    (progn
     (setq ENT (entnext ENT))
     (setq ENTLIST (entget ENT))
     (while (= "ATTRIB" (cdr (assoc 0 ENTLIST)))
      (setq ATT (cdr (assoc 2 ENTLIST)))
      (setq VAL (cdr (assoc 1 ENTLIST)))
      (cond ((= "TITLE" ATT)
             (setq TITLE VAL)
            )
            ((= "RAILCL" ATT)
             (setq RAILCL (atof VAL))
            )
            ((= "GAUGE" ATT)
             (setq GAUGE (atof VAL))
            )
            ((= "WB" ATT)
             (setq WB (atof VAL))
            )
            ((= "OH" ATT)
             (setq OH (atof VAL))
            )
            ((= "LABELIOX" ATT)
             (setq LABELIOX (atof VAL))
            )
            ((= "LABELIOY" ATT)
             (setq LABELIOY (atof VAL))
            )
            ((= "LABELSX" ATT)
             (setq LABELSX (atof VAL))
            )
            ((= "LABELOSX" ATT)
             (setq LABELOSX (atof VAL))
            )
            ((= "LABELOSY" ATT)
             (setq LABELOSY (atof VAL))
            )
            ((= "LTITLEH" ATT)
             (setq LTITLEH (atof VAL))
            )
            ((= "LTEXTH" ATT)
             (setq LTEXTH (atof VAL))
            )
            ((= "LLAYERS" ATT)
             (setq LLAYERS VAL)
            )
            ((= "DLAYERS" ATT)
             (setq DLAYERS VAL)
            )
            ((= "TLAYERS" ATT)
             (setq TLAYERS VAL)
            )
            ((= "REFSECTION" ATT)
             (setq REFSECTION VAL)
            )
            ((= "SECTION" ATT)
             (setq SECTION VAL)
            )
            ((= "LSECTION" ATT)
             (setq LSECTION VAL)
            )
            ((= "RSECTION" ATT)
             (setq RSECTION VAL)
            )
            ((= "GROUPTOL" ATT)
             (setq GROUPTOL (atof VAL))
            )
            ((= "SECTINC" ATT)
             (setq SECTINC (atof VAL))
            )
      )
      (setq ENT (entnext ENT))
      (setq ENTLIST (entget ENT))
     )
     (setq RFL:QUICKTRAINDEF
      (list
       (cons "TITLE" TITLE)
       (cons "RAILCL" RAILCL)
       (cons "GAUGE" GAUGE)
       (cons "WB" WB)
       (cons "OH" OH)
       (cons "LABELIOX" LABELIOX)
       (cons "LABELIOY" LABELIOY)
       (cons "LABELSX" LABELSX)
       (cons "LABELOSX" LABELOSX)
       (cons "LABELOSY" LABELOSY)
       (cons "LTITLEH" LTITLEH)
       (cons "LTEXTH" LTEXTH)
       (cons "LLAYERS" LLAYERS)
       (cons "DLAYERS" DLAYERS)
       (cons "TLAYERS" TLAYERS)
       (cons "REFSECTION" REFSECTION)
       (cons "SECTION" SECTION)
       (cons "LSECTION" LSECTION)
       (cons "RSECTION" RSECTION)
       (cons "GROUPTOL" GROUPTOL)
       (cons "SECTINC" SECTINC)
      )
     )
    )
   )
  )
 )
)