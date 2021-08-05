;
;
;     Program written by Robert Livingston, 2021-08-05
;
;     RFL:GETSECTIONPOINTLIST extracts a list of points from a C3D section surface line
;
;          Credit to jeff_M at theswamp.org for some code and ideas (http://www.theswamp.org/index.php?topic=45401.msg505671#msg505671)
;
(defun RFL:GETSECTIONPOINTLIST (SCTN SCTNVW / LINKS IDX LINK START END PTLIST X Y)
 (setq LINKS (vlax-get-property SCTN 'links))
 (setq IDX -1)
 (while (< (setq IDX (1+ IDX)) (vlax-get LINKS 'count))
  (setq LINK (vlax-invoke LINKS 'item IDX))
  (vlax-invoke-method SCTNVW 'FindXYAtStationOffsetAndElevation 0 (vlax-get LINK 'startpointx) (vlax-get LINK 'startpointy) 'x 'y)
  (setq START (cons X Y))
  (vlax-invoke-method SCTNVW 'FindXYAtStationOffsetAndElevation 0 (vlax-get LINK 'endpointx) (vlax-get LINK 'endpointy) 'x 'y)
  (setq END (cons X Y))
  (if (= (vlax-get LINK 'type) 0)
   ;;this link is displayed
   (progn
    (if (not (member START PTLIST))
     (setq PTLIST (cons START PTLIST))
    )
    (if (not (member END PTLIST))
     (setq PTLIST (cons END PTLIST))
    )
   )
  )
 )
 (setq PTLIST (reverse PTLIST))
)