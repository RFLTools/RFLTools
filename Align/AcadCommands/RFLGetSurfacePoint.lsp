;
;
;     Program written by Robert Livingston
;
;
(defun RFL:GETSURFACEPOINT (P OBSURFACE / VARLIST)
 (setq VARLIST (list OBSURFACE "FindElevationAtXY" (car P) (cadr P)))
 (setq Z (vl-catch-all-apply 'vlax-invoke-method VARLIST))
 (if (vl-catch-all-error-p Z)
  nil
  Z
 )
)
