# RFLAlign
AutoLisp Alignment Tools

Purpose:  To provide tools for analyzing civil alignments

Notes:  Use "RunMe.cmd" to combine all necessary files to single "LoadRFLAlign.lsp"

Status:

Work in progress!

All you need to run is "LoadRFLAlign.lsp" unless you want to eval code.  If so copy the entire project and use "RunMe.cmd"

Commands currently working:

C:DAlign        Draws the current alignment

C:DAlignOS      Draws the current alignment at a specified offset

C:FitSpiral     Draws a clothoid spiral between a line and an arc or a spiral/curve/spiral between two lines

C:GAlign        Defines an alignment (Get) based on a polyline of a selected set of independent alignment entities

C:RAlign        Reads an alignment from file

C:LAlign        Labels an alignment from file

C:WAlign        Writes an alignment to file

(RFL:STAOFF p)  Returns a list of (Station Offset) for a provided (X Y) point

(RFL:XY s)      Returns a point list (X Y) for a provided list of (Station Offset)

A registry value is written to "HKEY_CURRENT_USER\\rflAlignDirectory" for saving the last folder used.  This was added to simplify the searching for alignments
