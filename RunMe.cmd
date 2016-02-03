@echo off
rem
rem Simple copy batch to create combined lsp file
rem
if exist RFLTools.lsp del RFLTools.lsp
rem
COPY ".\MakeEnt\MakeEnt.lsp" + ^
     ".\DrawGrid\DrawGrid.lsp" ^
     ".\RFLTools.lsp"
pause
