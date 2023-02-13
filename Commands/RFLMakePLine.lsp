;
;
;  Program written by Robert Livingston, 2023-02-13
;
;  RFL:MAKEPLINE makes and open LWPOLYLINE from the provided PLIST
;
;    Thanks CAB at http://www.theswamp.org/index.php?PHPSESSID=41892584edb89b892971f87e12ad920f&topic=45039.msg502825#msg502825
;
;
(defun RFL:MAKEPLINE (PLIST / ITM PLIST2)  ; clsd must be 1 or 0
   (foreach ITM (reverse PLIST)
      (setq PLIST2 (cons (cons 10 ITM) PLIST2))
   )
   (entmakex
     (append
       (list '(0 . "LWPOLYLINE")
           '(100 . "AcDbEntity")
           '(100 . "AcDbPolyline")
           ;  (cons 8 (getvar "CLAYER"))
           (cons 90 (length PLIST))
           (cons 70 0)                    ; 1 for closed 0 otherwise
           ; (cons 38 ELEV)
           ; (cons 210 ZDIR)
       )
       PLIST2
     )
   )
 )