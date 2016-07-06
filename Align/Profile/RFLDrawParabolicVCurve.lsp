;
;
;     Program written by Robert Livingston, 2016/07/06
;
;     RFL:DRAWPARABOLICVCURVE draws a parabolic vertical curve through three input points.
;         Note that P2 must be precisely between P1 and P3 for this to be a valid alignment curve
;
;
(defun RFL:DRAWPARABOLICVCURVE (P1 P2 P3 / ENT ENTOB SPLINESEGS SPLINETYPE)
 (setq SPLINESEGS (getvar "SPLINESEGS"))
 (setq SPLINETYPE (getvar "SPLINETYPE"))
 
 (setq P1 (list (car P1) (cadr P1) 0.0)
       P2 (list (car P2) (cadr P2) 0.0)
       P3 (list (car P3) (cadr P3) 0.0)
 )

 (entmake (list (cons 0 "POLYLINE")
                (list 10 0.0 0.0 0.0)
                (cons 66 1)
          )
 )
 (entmake (list (cons 0 "VERTEX")
                (cons 10 P1)
          )
 )
 (entmake (list (cons 0 "VERTEX")
                (cons 10 P2)
          )
 )
 (entmake (list (cons 0 "VERTEX")
                (cons 10 P3)
          )
 )
 (setq ENT (entmake (list (cons 0 "SEQEND")
                    )
           )
 )
 (if ENT
  (progn
   (setvar "SPLINESEGS" 65)
   (setvar "SPLINETYPE" 5)
   (setq ENTOB (vlax-ename->vla-object (entlast)))
   (vlax-put-property ENTOB "Type" 2)
  )
 )
 
 (setvar "SPLINESEGS" SPLINESEGS)
 (setvar "SPLINETYPE" SPLINETYPE)
)