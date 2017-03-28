rem
rem Simple copy batch to create combined lsp file
rem
rem -------------------------------------------------
rem
rem Delete the file is it exists
rem
if exist LoadQuickTurn.lsp del LoadQuickTurn.lsp
rem
rem -------------------------------------------------
rem
rem Check if QuickTurn found off same directory as RFLTools
rem
if exist ..\..\Quickturn\LoadQuickTurn.lsp goto :FOUND
goto :NOTFOUND
rem
rem -------------------------------------------------
rem
rem QuickTurn Found
rem
:FOUND
copy ..\..\Quickturn\LoadQuickTurn.lsp
goto :END
rem
rem -------------------------------------------------
rem
rem QuickTurn Not Found
rem
:NOTFOUND
echo.; QuickTurn not located >> LoadQuickTurn.lsp
goto :END
rem
rem -------------------------------------------------
rem
rem End
rem
:END
