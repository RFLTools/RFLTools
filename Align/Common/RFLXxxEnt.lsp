;
;
;     Program written by Robert Livingston, 2016/07/06
;
;     RFL:xxxENT is a collection of routines for adding extended data for linking entities
;
;     (RFL:PUTENT E1 E2 E3)   :  Adds handle of E2 as the next entity, E3 as the previous entity to E1
;     (RFL:PUTNEXTENT E1 E2)  :  Adds handle of E2 as the next entity to E1
;     (RFL:PUTPREVENT E1 E2)  :  Adds handle of E2 as the previous entity to E1
;     (RFL:GETNEXTENT E1)     :  Returns the next entity of E1
;     (RFL:GETPREVENT E1)     :  Returns the previous entity of E1
;     (RFL:GETALLNEXTENT E1)  :  Returns all the next entities of E1
;     (RFL:GETALLPREVENT E1)  :  Returns all the previous entities of E1
;     (RFL:GETALLENT E1)      :  Returns all the entities linked to E1 (including E1)
;     (RFL:BREAKENT E1)       :  Removes all the links to E1 and relinks the previous and next to eachother
;
(defun RFL:PUTENT (ENT NEXTENT PREVENT / ENTLIST)
 (vl-load-com)
 (if (not (tblsearch "APPID" "RFLTOOLS_XENT"))
  (regapp "RFLTOOLS_XENT")
 )
 (setq ENTLIST nil)
 (if (= (type ENT) 'ENAME)
  (progn
   (cond ((and (= (type NEXTENT) 'ENAME)
               (= (type PREVENT) 'ENAME)
          )
          (setq ENTLIST (append (entget ENT)
                                (list
                                      (list -3 
                                            (list "RFLTOOLS_XENT"
                                                  (cons 1000 "RFLTOOLS_NEXTENT")
                                                  (cons 1005 (cdr (assoc 5 (entget NEXTENT))))
                                                  (cons 1000 "RFLTOOLS_PREVENT")
                                                  (cons 1005 (cdr (assoc 5 (entget PREVENT))))
                                            )
                                      )
                                )
                        )
          )
         )
         ((and (= (type NEXTENT) 'ENAME)
               (= PREVENT nil)
          )
          (setq ENTLIST (append (entget ENT)
                                (list
                                      (list -3 
                                            (list "RFLTOOLS_XENT"
                                                  (cons 1000 "RFLTOOLS_NEXTENT")
                                                  (cons 1005 (cdr (assoc 5 (entget NEXTENT))))
                                            )
                                      )
                                )
                        )
          )
         )
         ((and (= NEXTENT nil)
               (= (type PREVENT) 'ENAME)
          )
          (setq ENTLIST (append (entget ENT)
                                (list
                                      (list -3 
                                            (list "RFLTOOLS_XENT"
                                                  (cons 1000 "RFLTOOLS_PREVENT")
                                                  (cons 1005 (cdr (assoc 5 (entget PREVENT))))
                                            )
                                      )
                                )
                        )
          )
         )
         ((and (= NEXTENT nil)
               (= PREVENT nil)
          )
          (setq ENTLIST (list (cons -1 ENT) (list -3 (list "RFLTOOLS_XENT"))))
         )
   )
  )
 )
 (if ENTLIST
  (progn
   (entmod ENTLIST)
   ENT
  )
  nil
 )
)
(defun RFL:PUTNEXTENT (ENT NEXTENT / ENTLIST PREVENT)
 (if (= (type ENT) 'ENAME)
  (progn
   (setq PREVENT (RFL:GETPREVENT ENT))
   (RFL:PUTENT ENT NEXTENT PREVENT)
   ENT
  )
  nil
 )
)
(defun RFL:PUTPREVENT (ENT PREVENT / ENTLIST NEXTENT)
 (if (= (type ENT) 'ENAME)
  (progn
   (setq NEXTENT (RFL:GETNEXTENT ENT))
   (RFL:PUTENT ENT NEXTENT PREVENT)
   ENT
  )
  nil
 )
)
(defun RFL:GETNEXTENT (ENT / ENTLIST)
 (if (= (type ENT) 'ENAME)
  (progn
   (if (/= nil (setq ENTLIST (cdadr (assoc -3 (entget ENT (list "RFLTOOLS_XENT"))))))
    (while (and ENTLIST (/= (cdar ENTLIST) "RFLTOOLS_NEXTENT"))
     (setq ENTLIST (cdr ENTLIST))
    )
   )
   (setq ENTLIST (cdr ENTLIST))
   (if ENTLIST
    (handent (cdar ENTLIST))
    nil
   )
  )
  nil
 )
)
(defun RFL:GETPREVENT (ENT / ENTLIST)
 (if (= (type ENT) 'ENAME)
  (progn
   (if (/= nil (setq ENTLIST (cdadr (assoc -3 (entget ENT (list "RFLTOOLS_XENT"))))))
    (while (and ENTLIST (/= (cdar ENTLIST) "RFLTOOLS_PREVENT"))
     (setq ENTLIST (cdr ENTLIST))
    )
   )
   (setq ENTLIST (cdr ENTLIST))
   (if ENTLIST
    (handent (cdar ENTLIST))
    nil
   )
  )
  nil
 )
)
(defun RFL:GETALLNEXTENT (ENT / ENT2 ENTSET)
 (if (= (type ENT) 'ENAME)
  (progn
   (setq ENTSET (ssadd)
         ENT2 ENT
   )
   (while (setq ENT2 (RFL:GETNEXTENT ENT2))
    (ssadd ENT2 ENTSET)
   )
   ENTSET
  )
  nil
 )
)
(defun RFL:GETALLPREVENT (ENT / ENT2 ENTSET)
 (if (= (type ENT) 'ENAME)
  (progn
   (setq ENTSET (ssadd)
         ENT2 ENT
   )
   (while (setq ENT2 (RFL:GETPREVENT ENT2))
    (ssadd ENT2 ENTSET)
   )
   ENTSET
  )
  nil
 )
)
(defun RFL:GETALLENT (ENT / ENT2 ENTSET)
 (if (= (type ENT) 'ENAME)
  (progn
   (setq ENTSET (ssadd ENT)
         ENT2 ENT
   )
   (while (setq ENT2 (RFL:GETNEXTENT ENT2))
    (ssadd ENT2 ENTSET)
   )
   (setq ENT2 ENT)
   (while (setq ENT2 (RFL:GETPREVENT ENT2))
    (ssadd ENT2 ENTSET)
   )
   ENTSET
  )
 )
)
(defun RFL:BREAKENT (ENT / NEXTENT PREVENT)
 (if (= (type ENT) 'ENAME)
  (progn
   (setq NEXTENT (RFL:GETNEXTENT ENT))
   (setq PREVENT (RFL:GETPREVENT ENT))
   (RFL:PUTNEXTENT PREVENT NEXTENT)
   (RFL:PUTPREVENT NEXTENT PREVENT)
   (entmod (list (cons -1 ENT) (list -3 (list "RFLTOOLS_XENT"))))
   ENT
  )
  nil
 )
)