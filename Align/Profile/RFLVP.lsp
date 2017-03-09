;
;
;   Program written by Robert Livingston, 99/12/03
;
;   (RFL:VP) returns the X,Y point for a station and elevation of the currently defined vertical alignment
;
(defun RFL:VP (/ ACCEPTVP CANCEL CANCELVP DCL_ID FIXSTA FIXZ P VPPICK)
 (defun ACCEPTVP ()
  (setq CANCEL 0)
  (setq VPSTA (atof (get_tile "STATION")))
  (setq VPELEV (atof (get_tile "ELEV")))
  (done_dialog)
 )

 (defun CANCELVP ()
  (setq CANCEL 1)
  (done_dialog)
 )

 (defun VPPICK (/ P)
  (setq CANCEL -1)
  (done_dialog)
 )

 (defun FIXSTA (/ TMP)
  (set_tile "STATION" (rtos (atof (get_tile "STATION"))))
 )

 (defun FIXZ ()
  (set_tile "ELEV" (rtos (atof (get_tile "ELEV"))))
 )

 (setq CANCEL -1)

 (if (or (= RFL:PROFDEFLIST nil) (= RFL:PROFPOINT nil))
  (princ "\n*** No vertical alignment defined or utilities not loaded ***")
  (progn
   (while (= CANCEL -1)

    (if (= VPDCLNAME nil)
     (progn
      (setq VPDCLNAME (vl-filename-mktemp "rfl.dcl"))
      (RFL:MAKEDCL VPDCLNAME "VP")
     )
     (if (= nil (findfile VPDCLNAME))
      (progn
       (setq VPDCLNAME (vl-filename-mktemp "rfl.dcl"))
       (RFL:MAKEDCL VPDCLNAME "VP")
      )
     )
    )

    (setq DCL_ID (load_dialog VPDCLNAME))
    (if (not (new_dialog "VP" DCL_ID)) (exit))

    (if (= nil VPSTA) (setq VPSTA 0.0))
    (if (= nil VPELEV) (setq VPELEV 0.0))

    (set_tile "STATION" (rtos VPSTA))
    (set_tile "ELEV" (rtos VPELEV))

    (FIXSTA)
    (FIXZ)

    (action_tile "STATION" "(FIXSTA)")
    (action_tile "ELEV" "(FIXZ)")
    (action_tile "PICK" "(VPPICK)")
    (action_tile "OK" "(ACCEPTVP)")
    (action_tile "CANCEL" "(CANCELVP)")

    (start_dialog)
    (unload_dialog DCL_ID)

    (if (= CANCEL 0)
     (progn
      (command "_NON" (RFL:PROFPOINT VPSTA VPELEV))
     )
     (if (= CANCEL -1)
      (progn
       (setq P (getpoint "\nProfile point : "))
       (if (/= nil P)
        (progn
         (setq VPSTA (+ (* (cdr (assoc "DIRECTION" RFL:PROFDEFLIST))
                           (- (nth 0 P)
                              (nth 0 (cdr (assoc "BPOINT" RFL:PROFDEFLIST)))
                           )
                        )
                        (cdr (assoc "STA" RFL:PROFDEFLIST))
                     )
         )
         (setq VPELEV (+ (/ (- (nth 1 P)
                               (nth 1 (cdr (assoc "BPOINT" RFL:PROFDEFLIST)))
                            )
                            (cdr (assoc "VEXAG" RFL:PROFDEFLIST))
                         )
                         (cdr (assoc "ELEV" RFL:PROFDEFLIST))
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
