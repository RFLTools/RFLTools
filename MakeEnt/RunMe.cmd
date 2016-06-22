@echo off
rem
rem Simple copy batch to create combined lsp file
rem
if exist MakeEnt.lsp del MakeEnt.lsp
rem
COPY ".\MakeEntWrapperStart.lsp" + ^
     ".\Blocks\*.lsp" + ^
     ".\MakeEntWrapperEnd.lsp" ^
     ".\MakeEnt.lsp"
pause
