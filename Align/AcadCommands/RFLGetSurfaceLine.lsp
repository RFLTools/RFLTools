;
;
;     Program written by Robert Livingston
;
;
(defun RFL:GETSURFACELINE (P1 P2 OBSURFACE / C CATCHERROR OGLINE OGLINELIST VARLIST)
 (setq OGLINE nil)
 (setq VARLIST (list OBSURFACE "SampleElevations" (car P1) (cadr p1) (car P2) (cadr p2)))
 (setq OGLINE (vl-catch-all-apply 'vlax-invoke-method VARLIST))
 (if (not (vl-catch-all-error-p OGLINE))
  (if (/= nil OGLINE)
   (if (/= 0 (vlax-variant-type OGLINE))
    (progn
     (setq OGLINELIST nil)
     (setq OGLINE (vlax-variant-value OGLINE))
     (setq C (vlax-safearray-get-l-bound OGLINE 1))
     (while (<= C (vlax-safearray-get-u-bound OGLINE 1))
      (setq OGLINELIST (append OGLINELIST (list (list (vlax-safearray-get-element OGLINE C)
                                                      (vlax-safearray-get-element OGLINE (+ C 1))
                                                      (vlax-safearray-get-element OGLINE (+ C 2))))))
      (setq C (+ C 3))
     )
    )
   )
  )
 )
 OGLINELIST
)
