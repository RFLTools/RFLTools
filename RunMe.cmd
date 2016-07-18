@echo off
rem
rem Simple copy batch to create combined lsp file
rem
rem ---------------------------------------------
rem
rem Build Align
rem
CD .\Align
CALL .\RunMe.cmd
CD ..
rem
rem ---------------------------------------------
rem
rem Build MakeEnt
rem
CD .\MakeEnt
CALL .\RunMe.cmd
CD ..
rem
rem ---------------------------------------------
rem
rem Build MakeDCL
rem
CD .\MakeDCL
CALL .\RunMe.cmd
CD ..
rem
rem ---------------------------------------------
rem
rem Build RFLTools
rem
if exist LoadRFLTools.lsp del LoadRFLTools.lsp
rem
COPY ".\Common\RFLLoadRFLDLL.lsp" + ^
     ".\Common\Common.lsp" + ^
     ".\Common\C3DCommon.lsp" + ^
     ".\Math\Trig.lsp" + ^
     ".\Align\LoadRFLALign.lsp" + ^
     ".\MakeEnt\LoadMakeEnt.lsp" + ^
     ".\MakeDCL\LoadMakeDCL.lsp" + ^
     ".\DrawGrid\DrawGrid.lsp" + ^
     ".\Align\Align.lsp" + ^
     ".\Best\BestLine.lsp" + ^
     ".\Best\BestCircle.lsp" + ^
     ".\Best\BestVCurve.lsp" + ^
     ".\Common\Loaded.lsp" ^
     ".\LoadRFLTools.lsp"
rem
rem ---------------------------------------------
