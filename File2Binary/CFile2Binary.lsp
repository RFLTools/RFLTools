;
;
;     Program written by Robert Livingston, 2015-07-21
;
;     FILE2BINARY is a routine to create hex codes from a file to be used in MAKExxx.lsp
;
;
(defun C:FILE2BINARY (/ *error* C1 C2 CODE FILE FILEBINARY FLAG OUTFILE _ReadStream)
 (defun *error* (msg)
  (close OUTFILE)
  (setq *error* nil)
  (print msg)
 )
 ;;  Thanks to MP @ TheSwamp for his _ReadStream and WriteStream code:
 ;;  https://www.theswamp.org/index.php?topic=17465.msg210396
 (defun _ReadStream ( path len / fso file stream result )

     ;;  If the file is successful read the data is returned as 
     ;;  a string. Won't be tripped up by nulls, control chars
     ;;  including ctrl z (eof marker). Pretty fast (feel free 
     ;;  to bench mark / compare to alternates).
     ;;
     ;;  If the caller wants the result as a list of byte values 
     ;;  simply use vl-string->list on the result:
     ;;
     ;;      (setq bytes
     ;;          (if (setq stream (_ReadStream path len))
     ;;              (vl-string->list stream)
     ;;          )
     ;;      )            
     ;;
     ;;  Arguments:
     ;;
     ;;      path  <duh>
     ;;      len   Number of bytes to read. If non numeric, less 
     ;;            than 1 or greater than the number of bytes in 
     ;;            the file everything is returned.
    
     (vl-catch-all-apply
        '(lambda ( / iomode format size )
             (setq 
                 iomode   1 ;; 1 = read, 2 = write, 8 = append
                 format   0 ;; 0 = ascii, -1 = unicode, -2 = system default
                 fso      (vlax-create-object "Scripting.FileSystemObject")
                 file     (vlax-invoke fso 'GetFile path)
                 stream   (vlax-invoke fso 'OpenTextFile path iomode format)
                 size     (vlax-get file 'Size)
                 len      (if (and (numberp len) (< 0 len size)) (fix len) size)
                 result   (vlax-invoke stream 'read len)
             )
             (vlax-invoke stream 'Close)
         )
     )
    
     (if stream (vlax-release-object stream))
     (if file (vlax-release-object file))
     (if fso (vlax-release-object fso))
    
     result

 )
 (setq FLAG "w")
 (setq FILE (getfiled "Select a file" "" "" 4))
 (setq FILEBINARY (vl-string->list (_ReadStream FILE T)))
 (setq OUTFILE (open (getfiled "Select a file to write" "" "txt" 1) "w"))
 (while (/= nil FILEBINARY)
  (princ " (_WriteStream FILE (vl-list->string (list " OUTFILE)
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
  (princ (strcat ")) \"" FLAG "\")\n") OUTFILE)
  (setq FLAG "a")
 )
 (close OUTFILE)
)