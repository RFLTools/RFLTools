;
;
;     Program written by Robert Livingston, 2021-08-05
;
;     SCOPY extracts a section surface line from a section view
;
;          Credit to jeff_M at theswamp.org for some code and ideas (http://www.theswamp.org/index.php?topic=45401.msg505671#msg505671)
;
(defun C:SCOPY ( / DATUMPTS DOC FLATTENLIST GRNDPTS GRND MSPACE PLIST SCTNVW VL-SEL)
 ;;select vla-object
 (defun VL-SEL (msg / ent)
  (if (setq ent (car (entsel msg)))
   (vlax-ename->vla-object ent)
  )
 )
 (defun FLATTENLIST (LST / L RESULT)
  (foreach L LST
   (setq RESULT (cons (car L) RESULT)
         RESULT (cons (cdr L) RESULT)
   )
  )
  (reverse RESULT)
 )
 ;;selct the ground section, then the datum section, then the sectionview
 (setq SCTNVW (VL-SEL "\nSelect section view: "))
 (while (setq GRND   (VL-SEL "\nSelect ground section: "))
 ;;get the points, all start from left and go to the right
  (setq GRNDPTS  (RFL:GETSECTIONPOINTLIST GRND SCTNVW))

  (setq DOC    (vla-get-activedocument (vlax-get-acad-object))
        MSPACE (vla-get-modelspace DOC)
  )
  (setq PLIST (vlax-invoke MSPACE 'addlightweightpolyline (FLATTENLIST GRNDPTS)))
  (vla-put-closed PLIST :vlax-false)
 )
 nil
)