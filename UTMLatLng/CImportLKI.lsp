;
;
;     Program written by Robert Livingston, 2021-09-01
;
;     RFL:GETNEXTELEMENT returns the next NAME element in the file INFILE
;
;
(defun RFL:GETNEXTELEMENT (INFILE NAME / C CH MATCHLIST NAMELIST NAMELISTIN NAMELISTOUT OUTLIST)
 (setq OUTLIST nil)
 (setq NAMELIST nil)
 (defun MATCHLIST (LIST1 LIST2 / MATCH)
  (setq MATCH T)
  (while (and MATCH LIST1)
   (if (/= (car LIST1) (car LIST2)) (setq MATCH nil))
   (setq LIST1 (cdr LIST1) LIST2 (cdr LIST2))
  )
  MATCH
 )
 (if INFILE
  (progn
   (setq NAMELISTIN (vl-string->list (strcat "<" NAME ">")))
   (setq NAMELISTOUT (vl-string->list (strcat "</" NAME ">")))
   (setq C 0)
   (setq CH "")
   (while (and CH (not (MATCHLIST NAMELISTIN NAMELIST)))
    (setq CH (read-char INFILE))
    (setq NAMELIST (append NAMELIST (list CH)))
    (if (> (length NAMELIST) (length NAMELISTIN)) (setq NAMELIST (cdr NAMELIST)))
   )
   (while (and CH (not (MATCHLIST NAMELISTOUT NAMELIST)))
    (setq CH (read-char INFILE))
    (setq NAMELIST (append NAMELIST (list CH)))
    (setq OUTLIST (append OUTLIST (list CH)))
    (if (> (length NAMELIST) (length NAMELISTOUT)) (setq NAMELIST (cdr NAMELIST)))
   )
   (if OUTLIST
    (progn
     (setq OUTLIST (reverse OUTLIST))
     (foreach CH NAMELISTOUT (setq OUTLIST (cdr OUTLIST)))
     (setq OUTLIST (reverse OUTLIST))
    )
   )
   OUTLIST
  )
  nil
 )
)
;
;     RFL:GETELEMENT returns the next first element in the input list
;
(defun RFL:GETELEMENT (INLIST NAME / C CH GETFIRST MATCHLIST NAMELIST NAMELISTIN NAMELISTOUT OUTLIST)
 (setq OUTLIST nil)
 (setq NAMELIST nil)
 (defun MATCHLIST (LIST1 LIST2 / MATCH)
  (setq MATCH T)
  (while (and MATCH LIST1)
   (if (/= (car LIST1) (car LIST2)) (setq MATCH nil))
   (setq LIST1 (cdr LIST1) LIST2 (cdr LIST2))
  )
  MATCH
 )
 (defun GETFIRST (/ CH)
  (setq CH (car INLIST))
  (setq INLIST (cdr INLIST))
  CH
 )
 (if INFILE
  (progn
   (setq NAMELISTIN (vl-string->list (strcat "<" NAME ">")))
   (setq NAMELISTOUT (vl-string->list (strcat "</" NAME ">")))
   (setq C 0)
   (setq CH "")
   (while (and CH (not (MATCHLIST NAMELISTIN NAMELIST)))
    (setq CH (GETFIRST))
    (setq NAMELIST (append NAMELIST (list CH)))
    (if (> (length NAMELIST) (length NAMELISTIN)) (setq NAMELIST (cdr NAMELIST)))
   )
   (while (and CH (not (MATCHLIST NAMELISTOUT NAMELIST)))
    (setq CH (GETFIRST))
    (setq NAMELIST (append NAMELIST (list CH)))
    (setq OUTLIST (append OUTLIST (list CH)))
    (if (> (length NAMELIST) (length NAMELISTOUT)) (setq NAMELIST (cdr NAMELIST)))
   )
   (if OUTLIST
    (progn
     (setq OUTLIST (reverse OUTLIST))
     (foreach CH NAMELISTOUT (setq OUTLIST (cdr OUTLIST)))
     (setq OUTLIST (reverse OUTLIST))
    )
   )
   OUTLIST
  )
  nil
 )
)
;
;     RFL:POINTELEMENT2POINT converts an XML point element to a LAT LNG Point
;
(defun RFL:POINTELEMENT2POINT (INLIST / CH CHECKLIST NUMBERS OUTLIST)
 (setq NUMBERS (list (ascii "-")
                     (ascii ".")
                     (ascii ",")
                     (ascii "0")
                     (ascii "1")
                     (ascii "2")
                     (ascii "3")
                     (ascii "4")
                     (ascii "5")
                     (ascii "6")
                     (ascii "7")
                     (ascii "8")
                     (ascii "9")
               )
 )
 (defun CHECKLIST (CH CHLIST / CH2 MATCH)
  (setq MATCH nil)
  (foreach CH2 CHLIST (if (= CH CH2) (setq MATCH T)))
  MATCH
 )
 (foreach CH INLIST
  (if (CHECKLIST CH NUMBERS) (setq OUTLIST (append OUTLIST (list CH))))
 )
 (if OUTLIST
  (list (atof (RFL:COLUMN (vl-list->string OUTLIST) 2 ","))
        (atof (RFL:COLUMN (vl-list->string OUTLIST) 1 ","))
  )
  nil
 )
)
;
;     C:IMPORTLKI draws points at all the "Placemerk" elements of the inputted kml file
;
(defun C:IMPORTLKI (/ ACTIVEDOC ACTIVESPC ENT ENT2 ENTLIST INFILE INFILENAME KM NAME P PLACEMARKLIST SEGMENT)
 (setq ACTIVEDOC (vla-get-activedocument (vlax-get-acad-object)))
 (setq ACTIVESPC
       (vlax-get-property ACTIVEDOC
        (if (or (eq acmodelspace (vla-get-activespace ACTIVEDOC)) (eq :vlax-true (vla-get-mspace ACTIVEDOC)))
         'modelspace
         'paperspace
        )
       )
 )
 (setq INFILENAME (getfiled "Select a KML file" "" "kml" 2))
 (setq INFILE (open INFILENAME "r"))
 (setq NAME "Placemark")
 
 (while (setq PLACEMARKLIST (RFL:GETNEXTELEMENT INFILE NAME))
  (if (setq P (RFL:UTM2SURVEY (RFL:LATLNG2UTM (RFL:POINTELEMENT2POINT (RFL:GETELEMENT PLACEMARKLIST "Point")))))
   (progn
    (print P)
    (setq C (vl-string-search "<td>Segment<td>" (vl-list->string PLACEMARKLIST)))
    (setq SEGMENT (substr (vl-list->string PLACEMARKLIST) (+ C 16)))
    (setq C (vl-string-search "</tr>" SEGMENT))
    (setq SEGMENT (substr SEGMENT 1 C))
    (setq C (vl-string-search "<td>Km<td>" (vl-list->string PLACEMARKLIST)))
    (setq KM (substr (vl-list->string PLACEMARKLIST) (+ C 11)))
    (setq C (vl-string-search "</tr>" KM))
    (setq KM (substr KM 1 C))
    (if (= nil (tblsearch "BLOCK" "LKI")) (RFL:MAKEENT "LKI"))
    (vla-insertblock ACTIVESPC
                     (vlax-3D-point P)
                     "LKI"
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
       (if (= "KM" (cdr (assoc 2 ENTLIST)))
        (setq ENTLIST (subst (cons 1 KM) (assoc 1 ENTLIST) ENTLIST))
       )
       (entmod ENTLIST)
       (setq ENT2 (entnext ENT2))
       (setq ENTLIST (entget ENT2))
      )
      (entupd ENT)
     )
    )
   )
  )
 )
 
 (close INFILE)
 nil
)