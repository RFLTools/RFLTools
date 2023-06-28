;
;
;     Program written by Robert Livingston, 2015-07-21
;     Updated 2023-06-28 revised MP tl LM versions of read and write streams
;
;     FILE2BINARY is a routine to create hex codes from a file to be used in MAKExxx.lsp
;
;
(defun C:FILE2BINARY (/ *error* C1 C2 CODE FILE FILEBINARY OUTFILE LM:ReadBinaryStream)
 (defun *error* (msg)
  (close OUTFILE)
  ;(setq *error* nil)
  (print msg)
 )
  ;;-----------------=={ Read Binary Stream }==-----------------;;
  ;;                                                            ;;
  ;;  Uses the ADO Stream Object to read a supplied file and    ;;
  ;;  returns a variant of bytes.                               ;;
  ;;------------------------------------------------------------;;
  ;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
  ;;------------------------------------------------------------;;
  ;;  Arguments:                                                ;;
  ;;  filename - filename of file to read.                      ;;
  ;;  len      - number of bytes to read                        ;;
  ;;  (if non-numerical, less than 1, or greater than the size  ;;
  ;;   of the file, everything is returned).                    ;;
  ;;------------------------------------------------------------;;
  ;;  Returns:                                                  ;;
  ;;  Variant of Binary data which may be converted to a list   ;;
  ;;  bytes using the relevant VL Variant functions or used     ;;
  ;;  with LM:WriteBinaryStream.                                ;;
  ;;------------------------------------------------------------;;
 (defun LM:ReadBinaryStream ( filename len / ADOStream result ) (vl-load-com)
  
  (setq result
    (vl-catch-all-apply
      (function
        (lambda ( / size )
          (setq ADOStream (vlax-create-object "ADODB.Stream"))
          (vlax-invoke ADOStream 'Open)
          (vlax-put-property   ADOStream 'type 1)
          (vlax-invoke-method  ADOStream 'loadfromfile filename)
          (vlax-put-property   ADOStream 'position 0)
          (setq size (vlax-get ADOStream 'size))
          (vlax-invoke-method  ADOStream 'read (if (and (numberp len) (< 0 len size)) (fix len) -1))
        )
      )
    )
  )
  (if ADOStream (vlax-release-object ADOStream))

  (if (not (vl-catch-all-error-p result))
    result
  )
 )
 (setq FILE (getfiled "Select a file" "" "" 4))
 ;(setq FILEBINARY (vl-string->list (_ReadStream FILE T)))
 (setq FILEBINARY (vlax-safearray->list (vlax-variant-value (LM:ReadBinaryStream FILE 0))))
 (setq OUTFILE (open (getfiled "Select a file to write" "" "txt" 1) "w"))
 (while (/= nil FILEBINARY)
  (princ " (setq FILEBINARY (append FILEBINARY (list " OUTFILE)
  (setq C1 0)
  (while (and (/= nil FILEBINARY) (< C1 64))
   (setq C1 (+ C1 1))
   (setq C2 0)
   (while (and (/= nil FILEBINARY) (< C2 64))
    (setq C2 (+ C2 1))
    (setq CODE (car FILEBINARY))
    (setq FILEBINARY (cdr FILEBINARY))
    (princ (itoa CODE) OUTFILE)
    (if (>= CODE 100)
     (princ " " OUTFILE)
     (if (>= CODE 10)
      (princ "  " OUTFILE)
      (princ "   " OUTFILE)
     )
    )
   )
   (princ "\n                                           " OUTFILE)
   (setq C2 0)
  )
  (princ (strcat ")))\n") OUTFILE)
 )
 (close OUTFILE)
)