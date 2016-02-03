       ((= (strcase BLKNAME) "STATICK")
        (progn
         (entmake)
         (entmake
          (list
           (cons 0 "BLOCK")
           (cons 2 "STATICK")
           (cons 70 0)
           (list 10 0 0 0)
          )
         )
         (entmake
          (list
           (cons 0 "LINE")
           (cons 100 "AcDbEntity")
           (cons 67 0)
           (cons 8 "0")
           (cons 100 "AcDbLine")
           (list 10 0 -0.75 0)
           (list 11 0 0.75 0)
          )
         )
         (entmake (list (cons 0 "ENDBLK")))
        )
       )
