;
;
;     Program written by Robert Livingston, 2014-11-20
;
;     RFL:COLUMN returns the nth column of a DELIM delimited string
;
;
(defun RFL:COLUMN (LINE COL DELIM)
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
