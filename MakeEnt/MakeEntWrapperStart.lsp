;
;
;     Program written by Robert Livingston, 02-10-21
;
;     MAKEENT is a utility for creating RFL blocks within lisp
;
;
;     Blocks included to date:
;                              ALTABLE01
;                              ALTABLE01DATA
;                              BCCURVETABLE
;                              BCCURVETABLEDATA
;                              CIRC
;                              CURVETABLE
;                              CURVETABLEDATA
;                              DRAWGRIDDEF
;                              FORCE
;                              POINT
;                              PR-CIRCLE
;                              PVI2
;                              RFLALIGN
;                              RFLPROF
;                              SLOPE
;                              SPOTELEVATION
;                              SPOTELEVATION2
;                              STALBL
;                              STATICK
;                              SUPER
;                              SURVEYSPOT
;
(defun C:MAKEENT (/ DCL_ID ACCEPTMAKEENT BLOCKINDEX BLOCKLIST CANCELMAKEENT UPDATEBLOCK)
 (defun ACCEPTMAKEENT (/ C)
  (RFL:MAKEENT (nth BLOCKINDEX BLOCKLIST))
  (setq BLOCKINDEX nil)
  (done_dialog)
  (unload_dialog DCL_ID)
 )
 (defun CANCELMAKEENT ()
  (setq BLOCKINDEX nil)
  (done_dialog)
  (unload_dialog DCL_ID)
 )

 (defun UPDATEBLOCK ()
  (setq BLOCKINDEX (atoi (get_tile "BLOCKNAME")))
  (done_dialog)
  (unload_dialog DCL_ID)
 )

 (setq BLOCKLIST (list "ALTABLE01"
                       "ALTABLE01DATA"
                       "BCCURVETABLE"
                       "BCCURVETABLEDATA"
                       "CIRC"
                       "CURVETABLE"
                       "CURVETABLEDATA"
                       "FORCE"
                       "POINT"
                       "PR-CIRCLE"
                       "PVI2"
                       "RFLALIGN"
                       "RFLPROF"
                       "SLOPE"
                       "SPOTELEVATION"
                       "SPOTELEVATION2"
                       "STALBL"
                       "STATICK"
                       "SUPER"
                       "SURVEYSPOT"
                 )
 )

 (setq BLOCKINDEX 0)

 (while (/= nil BLOCKINDEX)
  (if (= MAKEENTDCLNAME nil)
   (progn
    (setq MAKEENTDCLNAME (vl-filename-mktemp "rfl.dcl"))
    (MAKEDCL MAKEENTDCLNAME "MAKEENT")
   )
   (if (= nil (findfile MAKEENTDCLNAME))
    (progn
     (setq MAKEENTDCLNAME (vl-filename-mktemp "rfl.dcl"))
     (MAKEDCL MAKEENTDCLNAME "MAKEENT")
    )
   )
  )
  (setq DCL_ID (load_dialog MAKEENTDCLNAME))
  (if (not (new_dialog "MAKEENT" DCL_ID)) (exit))

  (start_list "BLOCKNAME")
  (mapcar 'add_list BLOCKLIST)
  (end_list)

  (set_tile "BLOCKNAME" (itoa BLOCKINDEX))

  (setq RFLALIGNSLBNAME "rflAlign.slb")
  (if (= nil (findfile RFLALIGNSLBNAME))
   (progn
    (setq RFLALIGNSLBNAME (vl-filename-mktemp "rfl.slb"))
    (MAKERFLSLB RFLALIGNSLBNAME)
   )
  )
  (start_image "IMAGE")
  (slide_image 0 0 (- (dimx_tile "IMAGE") 1)
                   (- (dimy_tile "IMAGE") 1)
                   (strcat RFLALIGNSLBNAME "(" (nth BLOCKINDEX BLOCKLIST) ")")
  )
  (end_image)

  (action_tile "BLOCKNAME" "(UPDATEBLOCK)")
  (action_tile "OK" "(ACCEPTMAKEENT)")
  (action_tile "CANCEL" "(CANCELMAKEENT)")

  (start_dialog)
 )
)
(defun RFL:MAKEENT (BLKNAME / BLOCKLIST NODE)
 (setq BLOCKLIST (RFL:GETBLOCKLIST BLKNAME))
 (if (/= nil BLOCKLIST)
  (progn
   (entmake)
   (foreach NODE BLOCKLIST
    (entmake NODE)
   )
  )
  nil
 )
)
(defun RFL:GETBLOCKLIST (BLKNAME)
 (cond
