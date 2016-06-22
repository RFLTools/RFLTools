;
;
;     Program written by Robert Livingston, 02/10/18
;
;     WBLOCKLIST writes lisp code to create a selected block
;
;
(defun C:WBLOCKLIST (/ *error* BLOCKNAME CMDECHO CODE DIMZIN ENT ENTLIST NODE OUTFILE VAL)
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (setq DIMZIN (getvar "DIMZIN"))
 (setvar "DIMZIN" 8)

 (defun *error* (msg)
  (if (/= nil OUTFILE) (close OUTFILE))
  (setvar "CMDECHO" CMDECHO)
  (setvar "DIMZIN" DIMZIN)
  (setq *error* nil)
  (princ msg)
 )

 (if (/= nil (setq ENT (car (entsel "\nSelect block : "))))
  (if (= "INSERT" (cdr (assoc 0 (setq ENTLIST (entget ENT)))))
   (progn
    (princ (strcat "\nFile written to: " (getenv "UserProfile") "\\Documents\\" (strcase (cdr (assoc 2 ENTLIST))) ".lsp"))
    (setq OUTFILE (open (strcat (getenv "UserProfile") "\\Documents\\" (strcase (cdr (assoc 2 ENTLIST))) ".lsp") "w"))
    (princ (strcat "       ((= (strcase BLKNAME) \"" (strcase (cdr (assoc 2 ENTLIST))) "\")\n") OUTFILE)
    (princ "        (progn\n" OUTFILE)
    (princ "         (entmake)\n" OUTFILE)
    (princ "         (entmake\n" OUTFILE)
    (princ "          (list\n" OUTFILE)
    (setq ENTLIST (tblsearch "BLOCK" (cdr (assoc 2 ENTLIST))))
    (setq ENT (cdr (assoc -2 ENTLIST)))
    (while (/= nil ENTLIST)
     (setq NODE (car ENTLIST))
     (setq ENTLIST (cdr ENTLIST))
     (setq CODE (car NODE))
     (if (and (>= CODE 0) (<= CODE 100) (/= CODE 5))
      (if (= (vl-list-length NODE) nil)
       (progn
        (princ
         (strcat "           (cons "
                 (itoa CODE)
                 " "
                 (if (numberp (cdr NODE))
                  (rtos (cdr NODE) 2 8)
                  (strcat "\"" (cdr NODE) "\"")
                 )
                 ")\n"
         )
         OUTFILE
        )
       )
       (progn
        (princ "           (list" OUTFILE)
        (foreach VAL NODE (princ " " OUTFILE)
                          (if (numberp VAL)
                              (princ (rtos VAL 2 8) OUTFILE)
                              (princ (strcat "\"" VAL "\"") OUTFILE)
                          )
        )
        (princ ")\n" OUTFILE)
       )
      )
     )
    )

    (princ "          )\n" OUTFILE)
    (princ "         )\n" OUTFILE)

    (while (/= ENT nil)
     (setq ENTLIST (entget ENT))
     (setq ENT (entnext ENT))
     (princ "         (entmake\n" OUTFILE)
     (princ "          (list\n" OUTFILE)
     (while (/= ENTLIST nil)
      (setq NODE (car ENTLIST))
      (setq ENTLIST (cdr ENTLIST))
      (setq CODE (car NODE))
      (if (and (>= CODE 0) (<= CODE 100) (/= CODE 5))
       (if (= (vl-list-length NODE) nil)
        (progn
         (princ
          (strcat "           (cons "
                  (itoa CODE)
                  " "
                  (if (numberp (cdr NODE))
                   (rtos (cdr NODE) 2 8)
                   (strcat "\"" (cdr NODE) "\"")
                  )
                  ")\n"
          )
          OUTFILE
         )
        )
        (progn
         (princ "           (list" OUTFILE)
         (foreach VAL NODE (princ " " OUTFILE)
                           (if (numberp VAL)
                               (princ (rtos VAL 2 8) OUTFILE)
                               (princ (strcat "\"" VAL "\"") OUTFILE)
                           )
         )
         (princ ")\n" OUTFILE)
        )
       )
      )
     )
     (princ "          )\n" OUTFILE)
     (princ "         )\n" OUTFILE)
    )
    (princ "         (entmake (list (cons 0 \"ENDBLK\")))\n" OUTFILE)
    (princ "        )\n" OUTFILE)
    (princ "       )\n" OUTFILE)

    (close OUTFILE)
   )
   (princ "\n*** Entity not a block ***")
  )
 )

 (setvar "CMDECHO" CMDECHO)
 (setvar "DIMZIN" DIMZIN)
)