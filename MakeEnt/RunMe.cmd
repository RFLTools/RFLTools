@echo off
rem
rem Simple copy batch to create combined lsp file
rem
if exist LoadMakeEnt.lsp del LoadMakeEnt.lsp
rem
CALL .\BuildBlockList.cmd
COPY /B ".\RFLBlockList.lsp" + ^
        ".\MakeEntCmd.lsp" + ^
        ".\MakeEntWrapperStart.lsp" + ^
        ".\Blocks\*.lsp" + ^
        ".\MakeEntWrapperEnd.lsp" ^
        ".\LoadMakeEnt.lsp"
