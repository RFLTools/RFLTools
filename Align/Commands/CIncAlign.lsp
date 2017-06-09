;
;
;     Program written by Robert Livingston, 03-02-18
;
;     C:INCALIGN incruments the RFL alignment and profile by an entered amount
;
;
(defun C:INCALIGN (/ *error* ALSAVE CMDECHO INC NODE PVISAVE SUPERSAVE)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (defun *error* (msg)
  (setvar "CMDECHO" CMDECHO)
  (princ msg)
  (setq *error* nil)
 )

 (setq INC (getreal "\nEnter incrament to add to horizontal and vertical alignments : "))

 (setq ALSAVE RFL:ALIGNLIST)
 (setq RFL:ALIGNLIST nil)

 (setq NODE (car ALSAVE))
 (setq ALSAVE (cdr ALSAVE))

 (while (/= NODE nil)
  (setq NODE (append (list (+ INC (car NODE))) (cdr NODE)))
  (setq RFL:ALIGNLIST (append RFL:ALIGNLIST (list NODE)))
  (setq NODE (car ALSAVE))
  (setq ALSAVE (cdr ALSAVE))
 )

 (setq PVISAVE RFL:PVILIST)
 (setq RFL:PVILIST nil)

 (setq NODE (car PVISAVE))
 (setq PVISAVE (cdr PVISAVE))

 (while (/= NODE nil)
  (setq NODE (append (list (+ INC (car NODE))) (cdr NODE)))
  (setq RFL:PVILIST (append RFL:PVILIST (list NODE)))
  (setq NODE (car PVISAVE))
  (setq PVISAVE (cdr PVISAVE))
 )

 (setq SUPERSAVE RFL:SUPERLIST)
 (setq RFL:SUPERLIST nil)

 (setq NODE (car SUPERSAVE))
 (setq SUPERSAVE (cdr SUPERSAVE))

 (while (/= NODE nil)
  (setq NODE (append (list (+ INC (car NODE))) (cdr NODE)))
  (setq RFL:SUPERLIST (append RFL:SUPERLIST (list NODE)))
  (setq NODE (car SUPERSAVE))
  (setq SUPERSAVE (cdr SUPERSAVE))
 )

 (setvar "CMDECHO" CMDECHO)
 T
)