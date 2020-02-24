;
;
;     Program written by Robert Livingston, 2015-11-13
;
;     RFLGETPLISTINC returns a list of points for a selected polyline such that
;                    the distance between each point is equally spaced, just
;                    less than the STEP and ordered closest to P
;
;
(defun RFL:GETPLISTINC (P STEP ENT / C CMAX ENTLIST ENTOBJ L P1 PLIST)
 (vl-load-com)
 (setq PLIST nil)
 (setq ENTLIST (entget ENT))
 (if (and (/= "POLYLINE" (cdr (assoc 0 ENTLIST)))
          (/= "LWPOLYLINE" (cdr (assoc 0 ENTLIST))))
  (progn
   (alert "!!!  Not a polyline  !!!")
   nil
  )
  (progn
   (setq ENTOBJ (vlax-ename->vla-object ENT))
   (setq L (vlax-get-property ENTOBJ "LENGTH"))
   (setq CMAX (fix (/ L STEP)))
   (setq C 0)
   (while (< C CMAX)
    (setq P1 (vlax-curve-getPointAtDist ENTOBJ (/ (* C L) CMAX)))
    (if (/= nil P1)
     (progn
      (setq P1 (list (car P1) (cadr P1)))
      (setq PLIST (append PLIST (list P1)))
     )
    )
    (setq C (1+ C))
   )
   (setq P1 (vlax-curve-getEndPoint ENTOBJ))
   (setq P1 (list (car P1) (cadr P1)))
   (setq PLIST (append PLIST (list P1)))
   (if (< (distance P (car PLIST)) (distance P (last PLIST)))
    PLIST
    (reverse PLIST)
   )
  )
 )
)