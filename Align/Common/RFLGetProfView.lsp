;
;
;     Program written by Robert Livingston, 2024-10-11
;
;     RFL:GETPROFVIEW returns a profile view entity based on a profile entity and a point within the profile view
;
;
(defun RFL:GETPROFVIEW (ENT P / C CMAX ENTVIEW OBALIGNMENT OBPROFILE OBPROFILEVIEW P1 P2 PROFILEVIEWS SA1 SA2)
 (setq ENTVIEW nil)
 (setq OBPROFILE (vlax-ename->vla-object ENT))
 (setq OBALIGNMENT (vlax-get-property OBPROFILE "Alignment"))
 (setq OBPROFILEVIEWS (vlax-get-property OBALIGNMENT "ProfileViews"))
 (setq C 0)
 (setq CMAX (vlax-get-property OBPROFILEVIEWS "Count"))
 (if (and (= P nil) (= CMAX 1))
  (progn
   (setq OBPROFILEVIEW (vlax-invoke-method OBPROFILEVIEWS "Item" C))
   (setq ENTVIEW (vlax-vla-object->ename OBPROFILEVIEW))
  )
  (while (< C CMAX)
   (setq OBPROFILEVIEW (vlax-invoke-method OBPROFILEVIEWS "Item" C))
   (vlax-invoke-method OBPROFILEVIEW "GetBoundingBox" 'SA1 'SA2)
   (setq P1 (vlax-safearray->list SA1))
   (setq P2 (vlax-safearray->list SA2))
   (if (and (>= (car P) (car P1))
            (>= (cadr P) (cadr P1))
            (<= (car P) (car P2))
            (<= (cadr P) (cadr P2))
       )
    (progn
     (setq ENTVIEW (vlax-vla-object->ename OBPROFILEVIEW))
    )
   )
   (setq C (1+ C))
  )
 )
 ENTVIEW
)
