;
;
;     Program written by Robert Livingston, 2013-05-01
;
;     RFL:INSERTTRAIN Inserts a train at STA
;
;
(defun RFL:INSERTTRAIN (STA NAMECAR NAMETRUCK1 NAMETRUCK2 / *error* ANGBASE ANGDIR ATTREQ CMDECHO ENTC ENTT1 ENTT2 ENTSET OSMODE ORTHOMODE)
 (setq ATTREQ (getvar "ATTREQ"))
 (setvar "ATTREQ" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0.0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 1)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq OSMODE (getvar "OSMODE"))
 (setvar "OSMODE" 0)
 (setq ORTHOMODE (getvar "ORTHOMODE"))
 (setvar "ORTHOMODE" 0)

 (defun *error* (msg)
  (setvar "ATTREQ" ATTREQ)
  (setvar "ANGBASE" ANGBASE)
  (setvar "ANGDIR" ANGDIR)
  (setvar "CMDECHO" CMDECHO)
  (setvar "OSMODE" OSMODE)
  (setvar "ORTHOMODE" ORTHOMODE)
  (print msg)
 )

 (command "._INSERT" NAMETRUCK1 (list 0.0 0.0 0.0) "" "" "")
 (setq ENTT1 (entlast))
 (command "._INSERT" NAMETRUCK2 (list 0.0 0.0 0.0) "" "" "")
 (setq ENTT2 (entlast))
 (command "._INSERT" NAMECAR (list 0.0 0.0 0.0) "" "" "")
 (setq ENTC (entlast))

 (setvar "ATTREQ" ATTREQ)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
 (setvar "CMDECHO" CMDECHO)
 (setvar "OSMODE" OSMODE)
 (setvar "ORTHOMODE" ORTHOMODE)

 (setq ENTSET (ssadd))
 (setq ENTSET (ssadd ENTC ENTSET))
 (setq ENTSET (ssadd ENTT1 ENTSET))
 (setq ENTSET (ssadd ENTT2 ENTSET))
 
 (if (= nil (RFL:UPDATETRAIN STA ENTSET))
  (progn
   (command "._ERASE" ENTSET "")
   nil
  )
  (progn
   ENTSET
  )
 )
)
