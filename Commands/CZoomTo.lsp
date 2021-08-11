;
;
;     Program written by Robert Livingston, 2021-08-10
;
;     ZOOMTO is a utility to assist with zooming to blocks in an xref
;
;
(setq RFL:ZOOMTOC 0)
(setq RFL:ZOOMTOLIST nil)
(defun C:GETZ2 (/ C ENT ENTLIST REFENT P R S Z)
 (setq RFL:ZOOMTOLIST nil)
 (setq ENT (nentsel))
 (setq REFENT (cadr (last ENT)))
 (setq REFNAME (cdr (assoc 2 (entget REFENT))))
 (setq ENT (car (last ENT)))
 (setq BLKNAME (cdr (assoc 2 (entget ENT))))

 (setq ENTLIST (tblsearch "BLOCK" REFNAME))
 (setq P (cdr (assoc 10 ENTLIST)))
 (setq ENT (cdr (assoc -2 ENTLIST)))

 (setq C 0)
 (while ENT
  (if (= (cdr (assoc 2 (entget ENT))) BLKNAME)
   (setq RFL:ZOOMTOLIST (append RFL:ZOOMTOLIST (list ENT))
         C (1+ C)
   )
  )
  (setq ENT (entnext ENT))
 )
 (princ (strcat "\n" (itoa C) " \"" BLKNAME "\" blocks found.\n"))
 (setq RFL:ZOOMTOC (1- (length RFL:ZOOMTOLIST)))
 C
)
(defun C:Z2N (/ ENT ENTLIST P Z Z2)
 (setq Z 25.0 Z2 5.0)
 (if RFL:ZOOMTOLIST
  (progn
   (setq RFL:ZOOMTOC (1+ RFL:ZOOMTOC))
   (if (= RFL:ZOOMTOC (length RFL:ZOOMTOLIST)) (setq RFL:ZOOMTOC 0))
   (setq P (cdr (assoc 10 (entget (nth RFL:ZOOMTOC RFL:ZOOMTOLIST)))))
   (command-s "._ZOOM" "W" "NON" (list (- (car P) Z) (- (cadr P) Z)) "NON" (list (+ (car P) Z) (+ (cadr P) Z)))
   (grdraw (list (- (car P) Z2) (- (cadr P) Z2)) (list (+ (car P) Z2) (- (cadr P) Z2)) 7)
   (grdraw (list (+ (car P) Z2) (- (cadr P) Z2)) (list (+ (car P) Z2) (+ (cadr P) Z2)) 7)
   (grdraw (list (+ (car P) Z2) (+ (cadr P) Z2)) (list (- (car P) Z2) (+ (cadr P) Z2)) 7)
   (grdraw (list (- (car P) Z2) (+ (cadr P) Z2)) (list (- (car P) Z2) (- (cadr P) Z2)) 7)
  )
 )
 RFL:ZOOMTOC
)
(defun C:Z2P (/ ENT ENTLIST P Z Z2)
 (setq Z 25.0 Z2 5.0)
 (if RFL:ZOOMTOLIST
  (progn
   (setq RFL:ZOOMTOC (1- RFL:ZOOMTOC))
   (if (= RFL:ZOOMTOC -1) (setq RFL:ZOOMTOC (1- (length RFL:ZOOMTOLIST))))
   (setq P (cdr (assoc 10 (entget (nth RFL:ZOOMTOC RFL:ZOOMTOLIST)))))
   (command-s "._ZOOM" "W" "NON" (list (- (car P) Z) (- (cadr P) Z)) "NON" (list (+ (car P) Z) (+ (cadr P) Z)))
   (grdraw (list (- (car P) Z2) (- (cadr P) Z2)) (list (+ (car P) Z2) (- (cadr P) Z2)) 7)
   (grdraw (list (+ (car P) Z2) (- (cadr P) Z2)) (list (+ (car P) Z2) (+ (cadr P) Z2)) 7)
   (grdraw (list (+ (car P) Z2) (+ (cadr P) Z2)) (list (- (car P) Z2) (+ (cadr P) Z2)) 7)
   (grdraw (list (- (car P) Z2) (+ (cadr P) Z2)) (list (- (car P) Z2) (- (cadr P) Z2)) 7)
  )
 )
 RFL:ZOOMTOC
)