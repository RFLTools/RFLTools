;     Store this code in .lsp files based on different standards, drag/drop desired .lsp onto your drawing session to set
;
;     NODEMODE = 0  :  LEFT
;     NODEMODE = 1  :  RIGHT
;     NODEMODE = 2  :  INSIDE
;     NODEMODE = 3  :  OUTSIDE
;
;     xxxLAYER  :  '*' concatinates current layer
;
(setq RFL:LALIGNLIST (list (cons "LABELBLOCK" "STALBL")            ;  Station label block name
                           (cons "LABEL" 1)                        ;  Insert station labels yes/no
                           (cons "LABELLAYER" "C-ALGN-ANNO")             ;  Station label insertion layer
                           (cons "LABELINC" 100.0)                 ;  Station label insertion increment
                           (cons "LABELSCALE" 1.0)                 ;  Station label insertion scale
                           (cons "LABELOFFSET" -2.0)               ;  Station label relative offset
                           (cons "LABELROTATE" 0.0)                ;  Station label relative rotation
                           (cons "TICKBLOCK" "STATICK")            ;  Station tick block name
                           (cons "TICK" 1)                         ;  Insert station ticks yes/no
                           (cons "TICKLAYER" "C-ALGN-ANNO")              ;  Station tick insertion layer
                           (cons "TICKINC" 20.0)                   ;  Station tick insertion increment
                           (cons "TICKSCALE" 1.0)                  ;  Station tick insertion scale
                           (cons "TICKOFFSET" 0.0)                 ;  Station tick relative offset
                           (cons "TICKROTATE" 0.0)                 ;  Station tick relative rotation
                           (cons "NODELEFTBLOCK" "STANODELEFT")    ;  Block name for left side labelling
                           (cons "NODERIGHTBLOCK" "STANODERIGHT")  ;  Block name for right side labelling
                           (cons "NODE" 1)                         ;  Alignment node insertion yes/no
                           (cons "NODELAYER" "C-ALGN-ANNO")              ;  Alignment node insertion layer
                           (cons "NODEMODE" 0)                     ;  Alignment node mode (see above)
                           (cons "NODESCALE" 1.0)                  ;  Alignment node insertion scale
                           (cons "NODEOFFSET" 0.0)                 ;  Alignment node relative offset
                           (cons "NODEROTATE" 0.0)                 ;  Alignment node relative rotation
                     )
)