       ((= (strcase BLKNAME) "CIRC")
        (progn
         (entmake)
         (entmake
          (list
           (cons 0 "BLOCK")
           (cons 2 "CIRC")
           (cons 70 0)
           (cons 4 "")
           (list 10 0 0 0)
          )
         )
         (entmake
          (list
           (cons 0 "CIRCLE")
           (cons 67 0)
           (cons 8 "0")
           (list 10 0 0 0)
           (cons 40 5)
          )
         )
         (entmake (list (cons 0 "ENDBLK")))
        )
       )
