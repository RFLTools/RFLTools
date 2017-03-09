;
;
;     Program written by Robert Livingston, 99/05/28
;
;     C:SELEV inserts an elevation block as read from the alighment definition
;
;
(defun C:SELEV (/ ANG ANGBASE ANGDIR ATTREQ BLOCKNAME CMDECHO COFFSET DIMZIN ENT ENT2 ENTLIST INFILE INITDIRECTION INLINE PT
                  STATION
                  GETPT S STARTSTATION ENDSTATION OFFSET STEP USESUPER Z)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq ATTREQ (getvar "ATTREQ"))
 (setq DIMZIN (getvar "DIMZIN"))
 (setvar "DIMZIN" 0)
 (setq OSMODE (getvar "OSMODE"))
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0.0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 0)

 (setq STARTSTATION 1.0)
 (setq ENDSTATION 0.0)

 (defun C:SELEV2DIRECTION ()
  (initget "Forward Reverse")
  (setq SELEV2DIRECTION (getkword "\nEnter direction (<Forward>/Reverse) :"))
  (if (= SELEV2DIRECTION nil) (setq SELEV2DIRECTION "Forward"))
  (if (= SELEV2DIRECTION "Forward")
   (setq SELEV2DIRECTION -100.0)
   (setq SELEV2DIRECTION 100.0)
  )
 )

 (if (= RFL:SUPERLIST nil)
  (progn
   (setq USESUPER 0)
  )
  (progn
   (initget "Yes No")
   (setq USESUPER (getkword "\nUse superelevations (Yes/<No>) :"))
   (if (= USESUPER nil) (setq USESUPER "No"))
   (if (= USESUPER "No")
    (setq USESUPER 0)
    (progn
     (setq USESUPER 1)
     (setq COFFSET (getreal (strcat "\nEnter offset from control line <" (rtos 0.0) "> : ")))
     (if (= COFFSET nil) (setq COFFSET 0.0))
    )
   )
  )
 )

 (defun GETPT ()
  (setq PT (getpoint "\nSelect point (<return> for range of stations) :"))
  (setq OFFSET nil)
  (if (= PT nil)
   (progn
    (setq STARTSTATION (getreal "\nStart station (<return> to quit) :"))
    (if (/= STARTSTATION nil)
     (progn
      (setq ENDSTATION (getreal "\nEnd station (<return> to quit) :"))
      (if (/= ENDSTATION nil)
       (progn
        (setq STEP (abs (getreal "\nStep size (<return> to quit) :")))
        (if (and (/= STEP nil) (> ENDSTATION STARTSTATION))
         (progn
          (setq OFFSET (getreal "\nOffset (<return> to quit) :"))
          (if (/= OFFSET nil)
           (progn
            (setq PT (RFL:XY (list STARTSTATION OFFSET)))
           )
          )
         )
        )
       )
      )
     )
    )
   )
  )
 )

 (if (= SELEVBLOCK nil) (setq SELEVBLOCK "SPOTELEVATION"))
 (if (= SELEVSCALE nil) (setq SELEVSCALE 1.0))

 (princ "\nSelect spot elevation block (<return> for previous block) :")
 (setq ENT (car (entsel)))
 (if (/= ENT nil)
  (progn
   (setq ENTLIST (entget ENT))
   (setq SELEVBLOCK (cdr (assoc 2 ENTLIST)))
   (setq SELEVSCALE (cdr (assoc 41 ENTLIST)))
  )
 )

 (setq DELTA (getreal "\nEnter delta elevation <0.000> :"))
 (if (= DELTA nil) (setq DELTA 0.0))

 (if (= SELEV2FR nil)
  (progn
   (C:SELEV2DIRECTION)
   (princ "\nEnter SELEV2DIRECTION to reset this value.")
  )
 )

 (GETPT)
 (while (/= nil PT)
;  (setq ANG (getangle PT "\nReference angle :"))
;  (setq STATION (getreal "\nStation :"))
  (setq STATION (nth 0 (RFL:STAOFF PT)))
  (if (= OFFSET nil) (setq OFFSET (nth 1 (RFL:STAOFF PT))))
  (if (= USESUPER 1)
   (progn
    (setq S (RFL:SUPER STATION))
    (if (= S nil)
     (progn
      (princ "\n***  SUPER OUT OR RANGE - USING CONTROL ELEVATION ***")
      (setq S (cons 0.0 0.0))
     )
    )
    (if (< (- OFFSET COFFSET) 0.0)
     (setq Z (+ (* (abs (- OFFSET COFFSET)) (nth 0 S) 0.01) (RFL:ELEVATION STATION)))
     (setq Z (+ (* (abs (- OFFSET COFFSET)) (nth 1 S) 0.01) (RFL:ELEVATION STATION)))
    )
   )
   (progn
    (setq Z (RFL:ELEVATION STATION))
   )
  )
  (setq ANG (angle PT (RFL:XY (list STATION SELEV2DIRECTION))))
  (setvar "ATTREQ" 0)
  (setvar "OSMODE" 0)
  (command "INSERT" SELEVBLOCK "_NON" PT SELEVSCALE SELEVSCALE (angtos (- ANG (/ pi 2.0)) 0 16))
  (setvar "ATTREQ" 1)
  (setvar "OSMODE" OSMODE)
  (setq ENT (entlast))
  (setq ENTLIST (entget ENT))
  (if (= 1 (cdr (assoc 66 ENTLIST)))
   (progn
    (setq ENT2 (entnext ENT))
    (setq ENTLIST (entget ENT2))
    (while (/= "SEQEND" (cdr (assoc 0 ENTLIST)))
     (if (or (= "ELEV" (cdr (assoc 2 ENTLIST))) (= "ELEVATION" (cdr (assoc 2 ENTLIST))))
      (setq ENTLIST (subst (cons 1 (rtos (+ Z DELTA))) (assoc 1 ENTLIST) ENTLIST))
     )
     (entmod ENTLIST)
     (setq ENT2 (entnext ENT2))
     (setq ENTLIST (entget ENT2))
    )
    (entupd ENT)
   )
  )
  (if (< STARTSTATION ENDSTATION)
   (progn
    (setq STARTSTATION (+ STARTSTATION STEP))
    (setq PT (RFL:XY (list STARTSTATION OFFSET)))
   )
   (GETPT)
  )
 )

 (setvar "CMDECHO" CMDECHO)
 (setvar "ATTREQ" ATTREQ)
 (setvar "DIMZIN" DIMZIN)
 (setvar "OSMODE" OSMODE)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
)