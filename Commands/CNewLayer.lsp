;
;
;     Program written by Robert Livingston, 01-01-09
;
;     C:NEWLAYER takes a set of entities and changes their layer
;
;
(defun C:NEWLAYER (/ C CMDECHO ENT ENTLIST ENTSET L NEWLAYER PS STR)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (setq ENTSET (ssget))
 (setq C 0)
 (initget 1 "Prefix Suffix")
 (setq PS (getkword "\nPrefix or Suffix : "))
 (setq STR (getstring "\nEnter string to front/rear of layer name : "))
 (setq L -1)
 (while (< L 0)
  (setq L (getint "\nEnter number of characters to remove of front/rear of layer name : "))
 )
 (while (< C (sslength ENTSET))
  (setq ENT (ssname ENTSET C))
  (setq ENTLIST (entget ENT))
  (if (= PS "Prefix")
   (progn
    (setq NEWLAYER (strcat STR (substr (cdr (assoc 8 ENTLIST)) (+ L 1))))
   )
   (progn
    (setq NEWLAYER (strcat (substr (cdr (assoc 8 ENTLIST)) 1 (max 0 (- (strlen (cdr (assoc 8 ENTLIST))) L))) STR))
   )
  )
  (setq ENTLIST (subst (cons 8 NEWLAYER) (assoc 8 ENTLIST) ENTLIST))
  (entmod ENTLIST)
  (entupd ENT)
  (setq C (+ C 1))
 )

 (setvar "CMDECHO" 0)
 T
)