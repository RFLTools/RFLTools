;
;
;    Program written by Robert Livingston, 2019-04-09
;
;    Routine to get text from clipboard.
;
;    Code by Patrick_35 at https://www.theswamp.org/index.php?topic=21764.0
;
;
(defun RFL:GETCLIPBOARDTEXT ( / htmlfile result )
 (setq result
  (vlax-invoke
   (vlax-get
    (vlax-get
     (setq htmlfile (vlax-create-object "htmlfile"))
     'ParentWindow
    )
    'ClipBoardData
   )
   'GetData
   "Text"
  )
 )
 (vlax-release-object htmlfile)
 result
)