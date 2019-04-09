;
;
;    Program written by Robert Livingston, 2019-04-09
;
;    Routine to get text from clipboard.
;
;    Code by Patrick_35 at https://www.theswamp.org/index.php?topic=21764.0
;
;
(defun RFL:SETCLIPBOARDTEXT ( text / htmlfile result )
 (setq result
  (vlax-invoke
   (vlax-get
    (vlax-get
     (setq htmlfile (vlax-create-object "htmlfile"))
     'ParentWindow
    )
    'ClipBoardData
   )
   'SetData
   "Text"
   text
  )
 )
 (vlax-release-object htmlfile)
 text
)