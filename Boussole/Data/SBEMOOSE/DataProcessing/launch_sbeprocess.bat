@ECHO off

FOR %%M IN ( Work ) DO (
ECHO %%M

REM profils CTD
  FOR /F "tokens=1 delims=." %%A IN ('dir /B ..\%%M\raw\*.hex') DO (
    echo       %%A
    sbebatch traitementctd.txt %%M %%A #m
  )

)
:fin_boucle