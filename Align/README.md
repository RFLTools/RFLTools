# RFLAlign
AutoLisp Alignment Tools

Purpose:  To provide tools for analyzing civil alignments

Notes:  Use "RunMe.cmd" to combine all necessary files to single "LoadRFLAlign.lsp"

Status:

Work in progress!

All you need to run is "LoadRFLAlign.lsp" unless you want to eval code.  If so copy the entire project and use "RunMe.cmd"

Commands currently working:

C:AArc                       Draws an arc at the end of a selected entity

C:ALine                      Draws a line at the end of a selected entity

C:ASpiral                    Draws a spiral at the end of a selected entity

C:DAlign                     Draws the current alignment

C:DAlignOS                   Draws the current alignment at a specified offset

C:DProf                      Draws the current profile

C:DProfOG                    Draws the current OG profile

C:DSuper                     Draws the superelevation blocks along the alignmnet

C:FitSpiral                  Draws a clothoid spiral between a line and an arc or a spiral/curve/spiral between two lines

C:GAlign                     Defines an alignment (Get) based on a polyline of a selected set of independent alignment entities

C:GProfOG                    Defines a profile (Get) based on PVI blocks and a selected profile grid (see my Grid Tools for explanation)

C:GSuper                     Defines the superelevation by selecting super blocks along an alignment

C:LAlign                     Labels an alignment

C:LProf                      Labels a profile

C:RAB                        Reads the horizontal, vertical, super, OG from a selected alignment block

C:RAB2C3D                    Reads the horizontal/vertical and creates a Civil 3D alignment and profile (does not create the profile view)

C:RABKill                    Clears the alignment data from a selected alignment block

C:RABN                       Reads the horizontal, vertical, super, OG from a selected nested alignment block

C:RAlign                     Reads an alignment from file

C:RAlignB                    Reads an alignment from a selected alignment block

C:RAlignBN                   Reads an alignment from a selected nested alignment block

C:RAlignC3D                  Reads a Civil 3D alignment entity

C:RProf                      Reads a profile from file

C:RProfB                     Reads a profile from a selected alignment block

C:RProfBN                    Reads a profile from a selected nested alignment block

C:RProfC3D                   Reads a Civil 3D profile

C:RProfOG                    Reads an OG profile from file

C:RProfOGB                   Reads an OG profile from a selected alignment block

C:RProfOGBN                  Reads an OG profile from a selected nested alignment block

C:RSuper                     Reads the superelevation from file

C:RSuperB                    Reads the superelevation from a selected alignment block

C:RSuperBN                   Reads the superelevation from a selected nested alignment block

C:VCurve                     Draws a vertical curve (and PVI block) between two selected lines

C:WAlign                     Writes an alignment to file

C:WAlignB                    Writes an alignment to a selected alignment block

C:WProf                      Writes a profile to file

C:WProfB                     Writes a profile to a selected alignment block

C:WProfOG                    Writes an OG profile to file

C:WProfOGB                   Writes an OG profile to a selected alignment block

C:WSuper                     Writes the superelevation to file

C:WSuperB                    Writes the superelevation to a selected alignment block

(RFL:Elevation Sta)          Returns the elevation at station Sta

(RFL:ProfDef)                Will prompt for the user to select a grid and will define RFL:PROFDEFLIST

(RFL:ProfPoint Sta Elev)     Returns the point based on RFL:PROFDEFLIST at Sta and Elev

(RFL:Slope Sta)              Returns the slope at station Sta

(RFL:STAOFF p)               Returns a list of (Station Offset) for a provided (X Y) point

(RFL:XY s)                   Returns a point list (X Y) for a provided list of (Station Offset)

(RFL:XYP)                    A collection of routines for plotting XP points along an alignment

Global Variables:

RFL:ALIGNLIST                Horizontal Alignment

RFL:OGLIST                   OG vertical Profile

RFL:PVILIST                  Vertical Profile

RFL:LALIGNLIST               List describing alignment labeling variables

;     NODEMODE = 0  :  LEFT

;     NODEMODE = 1  :  RIGHT

;     NODEMODE = 2  :  INSIDE

;     NODEMODE = 3  :  OUTSIDE

;

;     xxxLAYER  :  '*' concatinates current layer

;

(setq RFL:LALIGNLIST (list (cons "LABELBLOCK" "STALBL")            ;  Station label block name

                           (cons "LABEL" 1)                        ;  Insert station labels yes/no
                           
                           (cons "LABELLAYER" "*-LBL")             ;  Station label insertion layer
                           
                           (cons "LABELINC" 100.0)                 ;  Station label insertion increment
                           
                           (cons "LABELSCALE" 1.0)                 ;  Station label insertion scale
                           
                           (cons "LABELOFFSET" 4.0)                ;  Station label relative offset
                           
                           (cons "LABELROTATE" 0.0)                ;  Station label relative rotation
                           
                           (cons "TICKBLOCK" "STATICK")            ;  Station tick block name
                           
                           (cons "TICK" 1)                         ;  Insert station ticks yes/no
                           
                           (cons "TICKLAYER" "*-LBL")              ;  Station tick insertion layer
                           
                           (cons "TICKINC" 20.0)                   ;  Station tick insertion increment
                           
                           (cons "TICKSCALE" 1.0)                  ;  Station tick insertion scale
                           
                           (cons "TICKOFFSET" 0.0)                 ;  Station tick relative offset
                           
                           (cons "TICKROTATE" 0.0)                 ;  Station tick relative rotation
                           
                           (cons "NODELEFTBLOCK" "STANODELEFT")    ;  Block name for left side labelling
                           
                           (cons "NODERIGHTBLOCK" "STANODERIGHT")  ;  Block name for right side labelling
                           
                           (cons "NODE" 1)                         ;  Alignment node insertion yes/no
                           
                           (cons "NODELAYER" "*-LBL")              ;  Alignment node insertion layer
                           
                           (cons "NODEMODE" 3)                     ;  Alignment node mode (see above)
                           
                           (cons "NODESCALE" 1.0)                  ;  Alignment node insertion scale
                           
                           (cons "NODEOFFSET" 0.0)                 ;  Alignment node relative offset
                           
                           (cons "NODEROTATE" 0.0)                 ;  Alignment node relative rotation
                           
                     )
                     
)

RFL:LPROFLIST                List describing profile labeling variables

(setq RFL:LPROFLIST (list (cons "PROFDRAW" 0)           ;  Draw profile

                          (cons "PROFLAYER" "*")        ;  Profile Layer
                          
                          (cons "TEXTLAYER" "*-LBL")    ;  Text Layer
                          
                          (cons "TEXTHEIGHT" 3.5)       ;  Text Height
                          
                          (cons "UNITS" "m")            ;  Linear Units
                          
                          (cons "LSLOPE" 1)             ;  Label Slope
                          
                          (cons "LL" 1)                 ;  Label 'L'
                          
                          (cons "LK" 1)                 ;  Label 'K'
                          
                          (cons "CNODES" 1)             ;  Circle Nodes
                          
                          (cons "CNODERAD" 1.0)         ;  Node Circle Radius
                          
                          (cons "CNODELENGTH" 10.0)     ;  Node Line Length
                          
                          (cons "CNODEVOFFSET" 1.0)     ;  Node Text Vertical Offset
                          
                          (cons "CNODEHOFFSET" 0.875)   ;  Node Text Horisontal Offset
                          
                          (cons "DPVI" 1)               ;  Draw PVI
                          
                          (cons "LPVI" 1)               ;  Label PVI
                          
                          (cons "LBVC" 1)               ;  Label BVC and EVC
                          
                          (cons "LHIGH" 0)              ;  Label 'high' Chainage (if not stations are labelled as '+234.567'
                          
                          (cons "LELEVATIONSC" 1)       ;  Label Elevations Curves
                          
                          (cons "LELEVATIONST" 1)       ;  Label Elevations Tangents
                          
                          (cons "ELEVTEXTHEIGHT" 2.5)   ;  Elevation Text Height
                          
                          (cons "ELEVTEXTINC" 20.0)     ;  Elevation Text Increment
                          
                          (cons "ELEVTEXTOS" -20.0)     ;  Elevation Text Offset (from grid basepoint)
                          
                          (cons "RAB" 0)                ;  Reverse Above/Below flag (1 = labels above with K/L below)
                          
                          (cons "DIRECTION" "DIRRIGHT") ;  Label Direction (DIRRIGHT / DIRLEFT / DIRUP / DIRDOWN)
                          
                          (cons "KPREC" 1)              ;  'K' Precision
                          
                          (cons "LPREC" 0)              ;  'L' Precision
                          
                          (cons "SLOPEPREC" 3)          ;  Slope Precision
                          
                          (cons "STAPREC" 3)            ;  Station Precision
                          
                          (cons "ELEVPREC" 3)           ;  Elevation Precision
                          
                    )
                    
)

RFL:PROFDEFLIST              List describing location of profile (or section) grid

RFL:RNELIST                  List describing Northing/Easting labelling

(setq RFL:RNELIST (list (cons "NE" 1)   ;  Label Northing and Easting

                        (cons "SO" 1)   ;  Label Station and Offset
                        
                        (cons "Z" 1)    ;  Label Control Elevations
                        
                        (cons "G" 1)    ;  Label Control Grades
                        
                        (cons "SE" 1)   ;  Label Superelevations
                        
                 )
                 
)
 



A registry value is written to "HKEY_CURRENT_USER\\rflAlignDirectory" for saving the last folder used.  This was added to simplify the searching for alignments
