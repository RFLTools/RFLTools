;
;
;     Program written by Robert Livingston, 2013-05-01
;
;     RFL:INSERTTRAINENT updates a train at P and rotations RX, RY and RZ
;
;
(defun RFL:UPDATETRAINENT (ENT P RX RY RZ / ENTLIST)
 (setq ENTLIST (entget ENT))
 (setq ENTLIST (subst (append (list 10) P) (assoc 10 ENTLIST) ENTLIST))
 (setq ENTLIST (subst (cons 50 0.0) (assoc 50 ENTLIST) ENTLIST))
 (setq ENTLIST (subst (list 210 0.0 0.0 1.0) (assoc 210 ENTLIST) ENTLIST))
 (entmod ENTLIST)
 (if (> (abs RX) TOL) (vla-rotate3d (vlax-ename->vla-object ENT) (vlax-3D-point P) (vlax-3D-point (list (+ (car P) 1.0) (cadr P) (caddr P))) RX))
 (if (> (abs RY) TOL) (vla-rotate3d (vlax-ename->vla-object ENT) (vlax-3D-point P) (vlax-3D-point (list (car P) (+ (cadr P) 1.0) (caddr P))) RY))
 (if (> (abs RZ) TOL) (vla-rotate3d (vlax-ename->vla-object ENT) (vlax-3D-point P) (vlax-3D-point (list (car P) (cadr P) (+ (caddr P) 1.0))) RZ))
)
