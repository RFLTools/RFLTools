@echo off
rem
rem Simple copy batch to create combined lsp file
rem
if exist LoadMakeEnt.lsp del MakeEnt.lsp
rem
COPY ".\MakeEntCmd.lsp" + ^
     ".\MakeEntWrapperStart.lsp" + ^
     ".\Blocks\*.lsp" + ^
     ".\MakeEntWrapperEnd.lsp" ^
     ".\LoadMakeEnt.lsp"
