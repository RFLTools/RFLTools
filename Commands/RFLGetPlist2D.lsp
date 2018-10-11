;
;
;     Program written by Robert Livingston, 2018-10-11
;
;     RFL:GETPLIST2D returns a list of 2D points along a polyline
;
;
(defun RFL:GETPLIST2D (ENT / ENTLIST P PLIST TMPLIST)
 (setq PLIST nil)
 (if (setq TMPLIST (RFL:GETPLIST ENT))
  (foreach P TMPLIST (setq PLIST (append PLIST (list (list (car P) (cadr P))))))
 )
 PLIST
)
