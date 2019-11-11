;
;
;     Program written by Robert Livingston, 2019-11-07
;
;     IMPORTLIDARPOINTS reads a .pts file and inserts points at the coordinates
;
;
(defun C:IMPORTLIDARPOINTS (/ C COLUMN INFILE INLINE N P X Y Z)
 (defun COLUMN (LINE COL DELIM)
  (if (= (vl-string-search DELIM LINE) nil)
   nil
   (progn
    (while (and (> (setq COL (1- COL)) 0)
                (/= (vl-string-search DELIM LINE) nil)
           )
     (setq LINE (substr LINE (+ (vl-string-search DELIM LINE) 2)))
    )
    (if (= COL 0)
     (if (/= (vl-string-search DELIM LINE) nil)
      (substr LINE 1 (vl-string-search DELIM LINE))
      LINE
     )
     nil
    )
   )
  )
 )
 (if (setq INFILE (open (getfiled "Select a .pts file" "" "pts" 2) "r"))
  (progn
   (setq C 1)
   (setq N (getint "\nPlot every n'th point.  N = "))
   (while (setq INLINE (read-line INFILE))
    (if (and (/= (setq X (COLUMN INLINE 1 " ")) "")
             (/= (setq Y (COLUMN INLINE 2 " ")) "")
             (/= (setq Z (COLUMN INLINE 3 " ")) "")
             (= (rem C N) 0)
	    )
     (progn
      (setq P (list (atof X) (atof Y) (atof Z)))
      (entmake (list (cons 0 "POINT")
                     (cons 10 P)
               )
      )
     )
    )
    (if (= (rem C 1000) 0) (princ (strcat "\r" (itoa C))))
    (setq C (1+ C))
   )
   (close INFILE)
  )
 )
)