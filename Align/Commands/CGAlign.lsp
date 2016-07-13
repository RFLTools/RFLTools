;
;
;   Program written by Robert Livingston, 98/06/11
;
;   GALIGN extracts a horizontal alignment from the current drawing
;
;
(defun C:GALIGN (/ ALIGNENT ALIGNENTLIST ANGBASE ANGDIR CMDECHO PSTART STASTART)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 0)

 (command "._UNDO" "M")
 (command "._UCS" "W")

 (if (/= RFL:ALIGNDEF nil)
  (progn
   (setq RFL:ALIGNLIST nil)
   (setq PSTART (getpoint "\nStart point:"))
   (if (/= PSTART nil)
    (progn
     (setq STASTART (getreal "\nStart chainage:"))
     (if (/= STASTART nil)
      (progn
       (princ "\nSelect R14 polyline (<return> to select SoftDesk entities):")
       (setq ALIGNENT (car (entsel)))
       (if (= ALIGNENT nil)
        (progn
         (setq ALIGNENT (ssget))
         (setq RFL:ALIGNLIST (RFL:ALIGNDEF (list ALIGNENT) PSTART STASTART))
        )
        (progn
         (setq ALIGNENTLIST (entget ALIGNENT))
         (if (= (cdr (assoc 0 ALIGNENTLIST)) "POLYLINE")
          (progn
           (command "._CONVERT" "P" "S" ALIGNENT "")
           (setq ALIGNENTLIST (entget ALIGNENT))
          )
         )
         (if (= (cdr (assoc 0 ALIGNENTLIST)) "LWPOLYLINE")
          (progn
           (setq RFL:ALIGNLIST (RFL:ALIGNDEF ALIGNENT PSTART STASTART))
          )
          (princ "\n**** NOT A POLYLINE ****")
         )
        )
       )
      )
     )
    )
   )
  )
  (progn
   (princ "\n!!!!! ALIGNMENT UTILITIES NOT LOADED !!!!!\n")
  )
 )

 (command "._UCS" "P")
 (setvar "CMDECHO" CMDECHO)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
)