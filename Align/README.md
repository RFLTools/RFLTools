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

Global Variables:

RFL:ALIGNLIST                Horizontal Alignment

RFL:OGLIST                   OG vertical Profile

RFL:PVILIST                  Vertical Profile

RFL:LALIGNLIST               List describing alignment labeling variables

RFL:LPROFLIST                List describing profile labeling variables

RFL:PROFDEFLIST              List describing location of profile (or section) grid



A registry value is written to "HKEY_CURRENT_USER\\rflAlignDirectory" for saving the last folder used.  This was added to simplify the searching for alignments
