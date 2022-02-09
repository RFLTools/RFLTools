;
;
;    Program written by Robert Livingston, 2019-04-09
;
;    Routine to get and set text from and to clipboard.
;
;    Code by Patrick_35 and xshrimp at https://www.theswamp.org/index.php?topic=21764.0
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
(defun RFL:SETCLIPBOARDTEXT (STR / htmlfile result )
 (if (= 'STR (type STR))
  (progn
   (setq html   (vlax-create-object "htmlfile")
         result (vlax-invoke (vlax-get (vlax-get html 'ParentWindow) 'ClipBoardData) 'setData "Text" STR)
   )
   (vlax-release-object html)
   STR
  )
 );end if
)