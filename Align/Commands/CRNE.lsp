;
;
;    Program written by Robert Livingston, 95/04/25
;                                 Revised, 98/05/12
;
;    RNE labels the Northing and Easting
;
;
(setq RFL:RNELIST (list (cons "NE" 1)   ;  Label Northing and Easting
                        (cons "SO" 1)   ;  Label Station and Offset
                        (cons "Z" 1)    ;  Label Control Elevations
                        (cons "G" 1)    ;  Label Control Grades
                        (cons "SE" 1)   ;  Label Superelevations
                 )
)
(defun C:RNE (/ P1 P2)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 (setq P1 (getpoint "\nEnter point :"))
 (setq P2 (getpoint P1 "Second point for leader :"))
 (RFL:RNE P1 P2)

 (setvar "CMDECHO" CMDECHO)
)
(defun RFL:RNE (P1 P2 / *error* ANGBASE ANGDIR CMDECHO ENT ENTLIST G OFFSET P3
                        S STA STR STRLIST TMP Z)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq ANGBASE (getvar "ANGBASE"))
 (setvar "ANGBASE" 0)
 (setq ANGDIR (getvar "ANGDIR"))
 (setvar "ANGDIR" 0)

 (defun *error* (msg)
  (setvar "CMDECHO" CMDECHO)
  (setvar "ANGBASE" ANGBASE)
  (setvar "ANGDIR" ANGDIR)
  (alert msg)
  (setq *error* nil)
 )

 (setq STRLIST nil
       STA nil
       OFFSET nil
       G nil
       Z nil
 )
 (if (setq STA (RFL:STAOFF P1))
  (setq OFFSET (cadr STA)
        STA (car STA)
        Z (RFL:ELEVATION STA)
        G (RFL:SLOPE STA)
        S (RFL:SUPER STA)
  )
 )
 (if (and (= (cdr (assoc "NE" RFL:RNELIST)) 1)
          P1
     )
  (setq STRLIST (append STRLIST (list (strcat "N " (rtos (cadr P1)) ", E " (rtos (car P1))))))
 )
 (if (and (= (cdr (assoc "SO" RFL:RNELIST)) 1)
          STA
          OFFSET
     )
  (setq STRLIST (append STRLIST (list (strcat "Sta." (RFL:STATXT STA) ", O/S " (rtos OFFSET)))))
 )
 (if (and (= (cdr (assoc "Z" RFL:RNELIST)) 1)
          Z
     )
  (setq STRLIST (append STRLIST (list (strcat "Ctrl Elev " (rtos Z)))))
 )
 (if (and (= (cdr (assoc "G" RFL:RNELIST)) 1)
          G
     )
  (setq STRLIST (append STRLIST (list (strcat "Ctrl grade " (rtos (* 100.0 G)) "%"))))
 )
 (if (and (= (cdr (assoc "SE" RFL:RNELIST)) 1)
          S
     )
  (setq STRLIST (append STRLIST (list (strcat "Ctrl Super: L:"
                                              (rtos (abs (nth 0 S)))
                                              "%"
                                              (if (= (RFL:SIGN (nth 0 S)) 1) " up" " down")
                                              ", R:"
                                              (rtos (abs (nth 1 S)))
                                              "%"
                                              (if (= (RFL:SIGN (nth 1 S)) 1) " up" " down")
                                      )
                                )
                )
  )
 )
 (if STRLIST
  (progn
   (command "LEADER" "_NON" P1 "_NON" P2 "")
   (foreach STR STRLIST
    (command STR)
   )
   (command "")
  )
 )

 (setvar "CMDECHO" CMDECHO)
 (setvar "ANGBASE" ANGBASE)
 (setvar "ANGDIR" ANGDIR)
)
