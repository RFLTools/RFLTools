# MakeEnt

AutoLisp Block Creation Tool

Purpose:  Create RFLTools blocks

Run RunMe.cmd to create/update LoadMakeEnt.lsp including all the blocks found in the ./Blocks folder

C:MAKEENT : Command with dialog.

(RFL:MAKEENT blockname) : Creates blockname or nil if it does not exist.

(RFL:GETBLOCKLIST blockname) : Returns a list that can be called with entmake (see RFL:MAKEENT)

C:WBLOCKLIST : Writes the selected blocks entitity information to the users documents folder under the name <blockname>.LSP.  This file can be placed into the ./Blocks folder

Note :

The textstyle used in the block definition will be as defined by (getvar "textstyle")

Nested blocks will not be written and must be defined prior