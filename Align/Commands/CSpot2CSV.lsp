;
;
;     Program written by Robert Livingston, 2026-03-02
;
;     Spot2CSV reads the ELEV attribute from a selected set of entities, calcualtes their STA/OFFSET and calculates their Design Elevation
;              and writes the set, ordered by station, to "SPOT2CSV.CSV" in the users Documents folder.
;
;
;(defun C:SPOT2CSV (/ EASTING ENT ENTLIST ENTSET ELEVDESIGN ELEVOG LAYER N N1 N2 NODE NORTHING OFFSET OUTFILE OUTFILENAME OUTLIST P STA)
(defun C:SPOT2CSV ()
 (if (or (= RFL:ALIGNLIST nil) (= RFL:PVILIST nil))
  (princ "\n*** ALIGNMENT AND/OR PROFILE NOT LOADED ***\n")
  (progn
   (setq OUTFILENAME (strcat (getenv "UserProfile") "\\Documents\\" "SPOT2CSV.CSV"))
   (setq OUTLIST nil)
   (princ "\nSelect spotelevation blocks :\n")
   (setq ENTSET (ssget))
   (setq N 0)
   (while (< N (sslength ENTSET))
    (setq ENT (ssname ENTSET N))
	(setq ENTLIST (entget ENT))
	(if (setq ELEVOG (RFL:GETATTVALUE ENT "ELEV"))
	 (progn
	  (setq ELEVOG (atof ELEVOG))
	  (setq LAYER (cdr (assoc 8 ENTLIST)))
	  (setq P (cdr (assoc 10 ENTLIST)))
	  (setq EASTING (car P))
	  (setq NORTHING (cadr P))
	  (setq P (RFL:STAOFF P))
	  (setq STA (car P))
	  (setq OFFSET (cadr P))
	  (setq ELEVDESIGN (RFL:ELEVATION STA))
	  (setq OUTLIST (append OUTLIST (list (list LAYER NORTHING EASTING STA OFFSET ELEVOG ELEVDESIGN))))
	 )
	)
	(setq N (1+ N))
   )
   (setq OUTLIST (vl-sort OUTLIST (function (lambda (N1 N2) (< (nth 3 N1) (nth 3 N2))))))
   (setq OUTFILE (open OUTFILENAME "w"))
   (princ "LAYER,NORTHING,EASTING,STATION,OFFSET,OGELEVATION,DESIGNELEVATION\n" OUTFILE)
   (princ "LAYER,NORTHING,EASTING,STATION,OFFSET,OGELEVATION,DESIGNELEVATION\n")
   (foreach NODE OUTLIST
    (progn
	 (princ (strcat (nth 0 NODE) ","
	                (rtos (nth 1 NODE) 2 3) ","
					(rtos (nth 2 NODE) 2 3) ","
					(rtos (nth 3 NODE) 2 3) ","
					(rtos (nth 4 NODE) 2 3) ","
					(rtos (nth 5 NODE) 2 3) ","
					(rtos (nth 6 NODE) 2 3) "\n"
		    )
	        OUTFILE
	 )
	 (princ (strcat (nth 0 NODE) ","
	                (rtos (nth 1 NODE) 2 3) ","
					(rtos (nth 2 NODE) 2 3) ","
					(rtos (nth 3 NODE) 2 3) ","
					(rtos (nth 4 NODE) 2 3) ","
					(rtos (nth 5 NODE) 2 3) ","
					(rtos (nth 6 NODE) 2 3) "\n"
		    )
	 )
	)
   )
   (close OUTFILE)
   (princ (strcat "\nData written to: " OUTFILENAME))
  )
 )
 nil
)