@echo off
rem
rem Simple copy batch to create combined lsp file
rem
if exist MakeEnt.lsp del MakeEnt.lsp
rem
COPY ".\MakeEnt-Front.lsp" + ^
     ".\Blocks\*.lsp" + ^
     ".\MakeEnt-Rear.lsp" ^
     ".\MakeEnt.lsp"
pause
