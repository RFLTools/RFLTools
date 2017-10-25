;
;
;     Program written by Robert Livingston 2017-05-22
;
;     C:HOR2TABLE is a wrapper for alignment table creation
;
;          RFL:HOR2TABLE  - Alberta Infrastructure
;          RFL:HOR2TABLE2 - BC Ministry
;
;
(defun C:HOR2TABLE (/ *error* CMDECHO REP)
 (defun *error* (msg)
  (setvar "CMDECHO" CMDECHO)
  (print msg)
  ;(setq *error* nil)
 )
 (setq CMDECHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 
 (initget "Ai Bc")
 (setq REP (getkword "\nAlberta Infrastructure / BC Ministry (AI/<BC>) : "))
 (if (= nil REP) (setq REP "BC"))
 (setq REP (strcase REP))
 (cond ((= REP "AI")
        (RFL:HOR2TABLE)
       )
       ((= REP "BC")
        (RFL:HOR2TABLE2)
       )
 )
 
 (setvar "CMDECHO" CMDECHO)
 T
)