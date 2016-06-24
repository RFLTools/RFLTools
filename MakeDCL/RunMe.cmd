@echo off
setlocal enableDelayedExpansion
rem
rem Simple copy batch to create combined lsp file
rem
rem -------------------------------------------------
rem
rem Delete the file is it exists
rem
if exist LoadMakeDCL.lsp del LoadMakeDCL.lsp
rem
rem -------------------------------------------------
rem
rem Start building the file
rem
echo.(defun RFL:MAKEDCL (OUTFILENAME DCLNAME / OUTFILE) > LoadMakeDCL.lsp
echo. (cond >> LoadMakeDCL.lsp
rem
rem -------------------------------------------------
rem
rem Cycle through the file names
rem
for /f %%f in ('dir /b /on .\DCL\*.DCL') do CALL :DIRLOOPBODY %%f
GOTO :END
:DIRLOOPBODY
echo %1
set BRO=(
set BRC=)
set QUOTE="
set REPSTR=\"
echo.       ((= (strcase DCLNAME) (strcase "%~n1")) >> LoadMakeDCL.lsp
echo.        (progn >> LoadMakeDCL.lsp
echo.         (setq OUTFILE (open OUTFILENAME "w")) >> LoadMakeDCL.lsp
for /f "delims=" %%a in (./DCL/%1) do (
set MYSTR=%%a
set MYSTR=!MYSTR:%QUOTE%=%REPSTR%!
echo.         !BRO!princ !QUOTE!!MYSTR!\n!QUOTE! OUTFILE!BRC! >> LoadMakeDCL.lsp
)
echo.         (close OUTFILE) >> LoadMakeDCL.lsp
echo.        ) >> LoadMakeDCL.lsp
echo.       ) >> LoadMakeDCL.lsp
GOTO:eof
rem
rem -------------------------------------------------
rem
:END
echo. ) >> LoadMakeDCL.lsp
echo.) >> LoadMakeDCL.lsp
