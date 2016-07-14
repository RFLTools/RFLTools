;
;
;   Program written by Robert Livingston, 99/10/08
;
;   RFL:DSUPER inserts SUPER blocks along the current alignment
;
;
(defun RFL:DSUPER (/ ACTIVEDOC ACTIVESPACE ANGBASE ANGDIR ATTREQ DIMZIN ENT ENTLIST NODE OSMODE P1 P2 PREVENT SL)
 (setq OSMODE (getvar "OSMODE"))
 (setvar "OSMODE" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 0)
 (setq DIMZIN (getvar "DIMZIN"))
 (setvar "DIMZIN" 8)
 (setq ATTREQ (getvar "ATTREQ"))

 (setq PREVENT nil)

 (vl-load-com)
 (setq ACTIVEDOC (vla-get-activedocument (vlax-get-acad-object)))
 (setq ACTIVESPC
       (vlax-get-property ACTIVEDOC
        (if (or (eq acmodelspace (vla-get-activespace ACTIVEDOC)) (eq :vlax-true (vla-get-mspace ACTIVEDOC)))
         'modelspace
         'paperspace
        )
       )
 )

 (command "._UNDO" "M")

 (if (and (/= nil RFL:ALIGNLIST) (/= RFL:XY nil) (/= RFL:SUPERLIST nil))
  (progn
   (setq SL RFL:SUPERLIST)
   (while (/= SL nil)
    (setq NODE (car SL))
    (setq SL (cdr SL))
    (setq P1 (RFL:XY (list (nth 0 NODE) 0.0)))
    (if (/= P1 nil)
     (progn
      (setq P2 (RFL:XY (list (nth 0 NODE) 10.0)))
      (if (= nil (tblsearch "BLOCK" "SUPER")) (RFL:MAKEENT "SUPER"))
      (vla-insertblock ACTIVESPC
                       (vlax-3D-point P1)
                       "SUPER"
                       1.0
                       1.0
                       1.0
                       (- (angle P1 P2) (/ pi 2.0))
      )
      (setq ENT (entlast))(RFL:PUTPREVENT ENT PREVENT)(RFL:PUTNEXTENT PREVENT ENT)(setq PREVENT ENT)
      (setq ENT (entnext ENT))
      (setq ENTLIST (entget ENT))
      (while (/= (cdr (assoc 0 ENTLIST)) "SEQEND")
       (if (= (cdr (assoc 2 ENTLIST)) "LEFT")
        (progn
         (setq ENTLIST (subst (cons 1 (rtos (nth 1 NODE) 2 8)) (assoc 1 ENTLIST) ENTLIST))
         (entmod ENTLIST)
         (entupd ENT)
        )
       )
       (if (= (cdr (assoc 2 ENTLIST)) "RIGHT")
        (progn
         (setq ENTLIST (subst (cons 1 (rtos (nth 2 NODE) 2 8)) (assoc 1 ENTLIST) ENTLIST))
         (entmod ENTLIST)
         (entupd ENT)
        )
       )
       (setq ENT (entnext ENT))
       (setq ENTLIST (entget ENT))
      )
     )
    )
   )
  )
  (princ "\n*** SUPERELEVATION NOT SET ***\n")
 )

 (setvar "OSMODE" OSMODE)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
 (setvar "DIMZIN" DIMZIN)
 (setvar "ANGDIR" ATTREQ)
)
