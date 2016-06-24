@echo off
rem
rem Simple copy batch to create RFLBlockList.lsp file
rem
rem -------------------------------------------------
rem
rem Delete the file is it exists
rem
if exist RFLBlockList.lsp del RFLBlockList.lsp
rem
rem -------------------------------------------------
rem
rem Start building the file
rem
echo.(setq RFL:MAKEENTBLOCKLIST (list > RFLBlockList.lsp
rem
rem -------------------------------------------------
rem
rem Cycle through the file names
rem
for /f %%f in ('dir /b /on .\Blocks\*.lsp') do echo.                                 "%%~nf" >> RFLBlockList.lsp
rem
rem -------------------------------------------------
rem
rem End the file
rem
echo.                           ) >> RFLBlockList.lsp
echo.) >> RFLBlockList.lsp
rem
