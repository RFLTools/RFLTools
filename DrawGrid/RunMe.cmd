@echo off
rem
rem Simple copy batch to create combined lsp file
rem
if exist LoadDrawGrid.lsp del LoadDrawGrid.lsp
rem
COPY /B ".\Common.lsp" + ^
        ".\DrawGrid.lsp" ^
        ".\LoadDrawGrid.lsp"
