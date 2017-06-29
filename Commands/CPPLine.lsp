;
;
;     Program written by Robert Livingston, 2000/01/11
;
;     PPLINE is a routine for creating 3D polylines from point block to point block
;
;
(setq RFL:PPLINEDELTA 0.0)
(defun C:PPLINE (/ CMDECHO ELEV ENT ENTLIST PT TMP Z)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (setq TMP (getreal (strcat "\nEnter delta elevation <" (rtos RFL:PPLINEDELTA) "> :")))
 (if (/= TMP nil)
  (progn
   (setq RFL:PPLINEDELTA TMP)
  )
 )

 (command "._3DPOLY")
 (while (/= (setq ENT (car (entsel "\nSelect spot elevation block :"))) nil)
  (setq Z nil)
  (setq ENTLIST (entget ENT))
  (if (= (cdr (assoc 0 ENTLIST)) "AECC_POINT")
   (progn
    (setq PT (cdr (assoc 11 ENTLIST)))
    (setq Z (nth 2 PT))
    (setq PT (list (nth 0 PT) (nth 1 PT)))
   )
   (if (= (cdr (assoc 0 ENTLIST)) "AECC_COGO_POINT")
    (progn
     (setq PT (vlax-get-property (vlax-ename->vla-object ENT) "Location"))
     (setq PT (vlax-variant-value PT))
     (setq Z (vlax-safearray-get-element PT 2))
     (setq PT (list (vlax-safearray-get-element PT 0) (vlax-safearray-get-element PT 1)))
    )
    (progn
     (if (= "INSERT" (cdr (assoc 0 ENTLIST)))
      (progn
       (setq PT (cdr (assoc 10 ENTLIST)))
       (setq PT (list (nth 0 PT) (nth 1 PT)))
       (if (= 1 (cdr (assoc 66 ENTLIST)))
        (progn
         (setq ENT (entnext ENT))
         (setq ENTLIST (entget ENT))
         (while (/= (cdr (assoc 0 ENTLIST)) "SEQEND")
          (if (or (= (cdr (assoc 2 ENTLIST)) "ELEV")
                  (= (cdr (assoc 2 ENTLIST)) "ELEVATION")
                  (= (cdr (assoc 2 ENTLIST)) "Z"))
           (progn
            (if (= (substr (cdr (assoc 1 ENTLIST)) 1 1) "(")
             (progn
              (setq Z (atof (substr (cdr (assoc 1 ENTLIST)) 2)))
             )
             (progn
              (setq Z (atof (cdr (assoc 1 ENTLIST))))
             )
            )
           )
          )
          (setq ENT (entnext ENT))
          (setq ENTLIST (entget ENT))
         )
         (if (= Z nil)
          (progn
           (setq Z (getreal "\nNo elevation found, enter elevation (<return> to disregard point) :"))
          )
         )
        )
       )
      )
     )
    )
   )
  )
  (if (/= Z nil)
   (progn
    (command "_NON" (list (nth 0 PT) (nth 1 PT) (+ Z RFL:PPLINEDELTA)))
   )
  )
 )
 (command "")

 (setvar "CMDECHO" CMDECHO)
 T
)
