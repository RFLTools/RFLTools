;
;
;    Program Written by Robert Livingston 99/08/10
;    PJOIN is a utility for joining entities to a LWPolyline
;
;
(defun C:PJOIN (/ CMDECHO ENTSET OSMODE PLINEENT PLINEENTLIST TOL)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq OSMODE (getvar "OSMODE"))
 (setvar "OSMODE" 0)
 (princ "\nSelect LWPOLYLINE :")
 (setq PLINEENT (car (entsel)))
 (if (= nil PLINEENT)
  (progn
   (princ "\n*** Nothing selected ***")
  )
  (progn
   (setq PLINEENTLIST (entget PLINEENT))
   (if (= (cdr (assoc 0 PLINEENTLIST)) "LWPOLYLINE")
    (progn
     (if (/= (float (/ (cdr (assoc 70 PLINEENTLIST)) 2))
             (/ (cdr (assoc 70 PLINEENTLIST)) 2.0))
      (progn
       (princ "\n*** Entity is a closed lwpolyline ***")
      )
      (progn
       (setq ENTSET (ssget))
       (if (/= (ssmemb PLINEENT ENTSET) nil)
        (setq ENTSET (ssdel PLINEENT ENTSET))
       )
       (setq TOL (getdist (strcat "\nEnter distance tolerance <" (rtos RFL:TOL) "> :")))
       (if (= TOL nil) (setq TOL RFL:TOL))
       (princ "\nFiltering out non-joinable entities ...")
       (RFL:PJOIN PLINEENT ENTSET TOL)
      )
     )
    )
    (progn
     (princ "\n*** Entity not a LWPOLYLINE ***")
    )
   )
  )
 )
 (setvar "CMDECHO" CMDECHO)
 (setvar "OSMODE" OSMODE)
 T
)
