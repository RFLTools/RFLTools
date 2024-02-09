;
;
;     Program written by Robert Livingston, 2024-02-09
;
;     IMPORTPLACEMARK.LSP is a routine to insert KML placemarks into the drawing at the current survey defined by RFLSURVEY
;
;
(defun C:IMPORTPLACEMARK (/ ACTIVEDOC ACTIVESPC ENT ENT2 ENTLIST ENTLIST2 ID INFILE INFILENAME NAME P PLACEMARKLIST)
 (setq INFILENAME (getfiled "Select a KML file" "" "kml" 2))
 (setq INFILE (open INFILENAME "r"))
 (setq NAME "Placemark")
 
 (command-s "._UNDO" "M")
 (setq ACTIVEDOC (vla-get-activedocument (vlax-get-acad-object)))
 (setq ACTIVESPC
       (vlax-get-property ACTIVEDOC
        (if (or (eq acmodelspace (vla-get-activespace ACTIVEDOC)) (eq :vlax-true (vla-get-mspace ACTIVEDOC)))
         'modelspace
         'paperspace
        )
       )
 )

 (while (setq PLACEMARKLIST (RFL:GETNEXTELEMENT INFILE NAME))
  (if (setq P (RFL:LATLNG2UTM (RFL:POINTELEMENT2POINT (RFL:GETELEMENT PLACEMARKLIST "coordinates"))))
   (progn
    (print P)
    (if (= (car P) (strcat (nth 5 RFL:SURVEY) (nth 6 RFL:SURVEY)))
     (progn
      (setq ID (vl-list->string (RFL:GETELEMENT PLACEMARKLIST "name")))
      (setq P (RFL:UTM2SURVEY (cdr P)))
      (if (= nil (tblsearch "BLOCK" "PlacemarkPoint")) (RFL:MAKEENT "PlacemarkPoint"))
      (vla-insertblock ACTIVESPC
                       (vlax-3D-point P)
                       "PlacemarkPoint"
                       1.0
                       1.0
                       1.0
                       0.0
      )
      (setq ENT (entlast))
      (setq ENTLIST (entget ENT))
      (if (= 1 (cdr (assoc 66 ENTLIST)))
       (progn
        (setq ENT2 (entnext ENT))
        (setq ENTLIST (entget ENT2))
        (while (/= "SEQEND" (cdr (assoc 0 ENTLIST)))
         (if (= "SEGMENT" (cdr (assoc 2 ENTLIST)))
          (setq ENTLIST (subst (cons 1 SEGMENT) (assoc 1 ENTLIST) ENTLIST))
         )
         (if (= "NAME" (cdr (assoc 2 ENTLIST)))
          (setq ENTLIST (subst (cons 1 ID) (assoc 1 ENTLIST) ENTLIST))
         )
         (entmod ENTLIST)
         (setq ENT2 (entnext ENT2))
         (setq ENTLIST (entget ENT2))
        )
        (entupd ENT)
       )
      )
     )
     (progn
      (print "Not in correct ZONE!")
     )
    )
   )
  )
 )
 
 (close INFILE)
)