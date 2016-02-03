@echo off
rem
rem Simple copy batch to create combined lsp file
rem
if exist LoadRFLTools.lsp del LoadRFLTools.lsp
rem
COPY ".\Common\Common.lsp" + ^
     ".\Math\Trig.lsp" + ^
     ".\Align\Align.lsp" + ^
     ".\MakeEnt\MakeEnt.lsp" + ^
     ".\Best\BestLine.lsp" + ^
     ".\Best\BestCircle.lsp" + ^
     ".\Best\BestVCurve.lsp" + ^
     ".\DrawGrid\DrawGrid.lsp" ^
     ".\LoadRFLTools.lsp"
pause
