;
;
;     Program written by Robert Livingston, 02-10-21
;
;     MAKEENT is a utility for creating RFL blocks within lisp
;
;
(defun C:MAKEENT (/ DCL_ID ACCEPTMAKEENT BLOCKINDEX CANCELMAKEENT UPDATEBLOCK)
 (defun ACCEPTMAKEENT (/ C)
  (RFL:MAKEENT (nth BLOCKINDEX RFL:MAKEENTBLOCKLIST))
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

 (setq BLOCKINDEX 0)

 (while (/= nil BLOCKINDEX)
  (if (= MAKEENTDCLNAME nil)
   (progn
    (setq MAKEENTDCLNAME (vl-filename-mktemp "rfl.dcl"))
    (RFL:MAKEDCL MAKEENTDCLNAME "MAKEENT")
   )
   (if (= nil (findfile MAKEENTDCLNAME))
    (progn
     (setq MAKEENTDCLNAME (vl-filename-mktemp "rfl.dcl"))
     (RFL:MAKEDCL MAKEENTDCLNAME "MAKEENT")
    )
   )
  )
  (setq DCL_ID (load_dialog MAKEENTDCLNAME))
  (if (not (new_dialog "MAKEENT" DCL_ID)) (exit))

  (start_list "BLOCKNAME")
  (mapcar 'add_list RFL:MAKEENTBLOCKLIST)
  (end_list)

  (set_tile "BLOCKNAME" (itoa BLOCKINDEX))

  (setq RFLALIGNSLBNAME "rflAlign.slb")
  (if (= nil (findfile RFLALIGNSLBNAME))
   (progn
    (setq RFLALIGNSLBNAME (vl-filename-mktemp "rfl.slb"))
    (RFL:MAKERFLSLB RFLALIGNSLBNAME)
   )
  )
  (start_image "IMAGE")
  (slide_image 0 0 (- (dimx_tile "IMAGE") 1)
                   (- (dimy_tile "IMAGE") 1)
                   (strcat RFLALIGNSLBNAME "(" (nth BLOCKINDEX RFL:MAKEENTBLOCKLIST) ")")
  )
  (end_image)

  (action_tile "BLOCKNAME" "(UPDATEBLOCK)")
  (action_tile "OK" "(ACCEPTMAKEENT)")
  (action_tile "CANCEL" "(CANCELMAKEENT)")

  (start_dialog)
 )
)
