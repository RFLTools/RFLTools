@echo off
rem
rem Simple copy batch to create combined lsp file
rem
if exist LoadRFLTools.lsp del LoadRFLTools.lsp
rem
COPY ".\MakeEnt\MakeEnt.lsp" + ^
     ".\DrawGrid\DrawGrid.lsp" ^
     ".\LoadRFLTools.lsp"
pause
