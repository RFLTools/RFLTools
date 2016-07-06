;
;
;     Program written by Robert Livingston, 2016/07/06
;
;     RFL:xxxENT is a collection of routines for adding extended data for linking entities
;
;     (RFL:PUTNEXTENT E1 E2)  :  Adds handle of E2 as the next entity of E1
;     (RFL:PUTPREVENT E1 E2)  :  Adds handle of E2 as the previous entity of E1
;     (RFL:GETNEXTENT E1)     :  Returns the next entity of E1
;     (RFL:GETPREVENT E1)     :  Returns the previous entity of E1
;     (RFL:GETALLNEXTENT E1)  :  Returns all the next entities of E1
;     (RFL:GETALLPREVENT E1)  :  Returns all the previous entities of E1
;     (RFL:GETALLENT E1)      :  Returns all the entities linked to E1 (including E1)
;
(defun RFL:PUTNEXTENT (ENT NEXTENT / ENTLIST PREVENT)
 (vl-load-com)
 (if (not (tblsearch "APPID" "RFLTOOLS_XENT"))
  (regapp "RFLTOOLS_XENT")
 )
 (if (and (= (type ENT) 'ENAME)
          (= (type NEXTENT) 'ENAME)
     )
  (progn
   (if (setq PREVENT (RFL:GETPREVENT ENT))
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
   (entmod ENTLIST)
   ENT
  )
  nil
 )
)
(defun RFL:PUTPREVENT (ENT PREVENT / ENTLIST NEXTENT)
 (vl-load-com)
 (if (not (tblsearch "APPID" "RFLTOOLS_XENT"))
  (regapp "RFLTOOLS_XENT")
 )
 (if (and (= (type ENT) 'ENAME)
          (= (type PREVENT) 'ENAME)
     )
  (progn
   (if (setq NEXTENT (RFL:GETNEXTENT ENT))
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
   (entmod ENTLIST)
   ENT
  )
  nil
 )
)
(defun RFL:GETNEXTENT (ENT / ENTLIST)
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
(defun RFL:GETPREVENT (ENT / ENTLIST)
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
(defun RFL:GETALLNEXTENT (ENT / ENT2 ENTSET)
 (setq ENTSET (ssadd)
       ENT2 ENT
 )
 (while (setq ENT2 (RFL:GETNEXTENT ENT2))
  (ssadd ENT2 ENTSET)
 )
 ENTSET
)
(defun RFL:GETALLPREVENT (ENT / ENT2 ENTSET)
 (setq ENTSET (ssadd)
       ENT2 ENT
 )
 (while (setq ENT2 (RFL:GETPREVENT ENT2))
  (ssadd ENT2 ENTSET)
 )
 ENTSET
)
(defun RFL:GETALLENT (ENT / ENT2 ENTSET)
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