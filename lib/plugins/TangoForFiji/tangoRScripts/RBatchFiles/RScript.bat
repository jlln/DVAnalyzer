@echo off
rem if /i "%1"==path (path %2) && goto:eof

setlocal
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
:: Placing this file in your path will allow rcmd to be run anywhere
:: without changing your path environment variable.  See comments
:: below on how it finds where R is.  Your path can be listed by
:: the Windows console command:  path
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

rem at one tine this script had only been tested on XP
rem recent tests have only been on Vista
rem ver | findstr XP >NUL
rem if errorlevel 1 echo Warning: This script has only been tested on Windows XP.

set scriptdir_=%~dp0
set lookin=.;%userprofile%;%scriptdir_%
if not defined R_BATCHFILES_RC (
	for %%f in ("rbatchfilesrc.bat") do set "R_BATCHFILES_RC=%%~$lookin:f"
)
if defined R_BATCHFILES_RC (
	if exist "%R_BATCHFILES_RC%" call %R_BATCHFILES_RC%
)

:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
:: use environment variable R_HOME if defined
:: else current folder if bin\rcmd.exe exists 
:: else most current R as determined by registry entry
:: else error
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

if not defined R_HOME if exist bin\r.exe set R_HOME=%CD%
if not defined R_HOME for /f "tokens=2*" %%a in (
 'reg query hklm\software\R-core\R /v InstallPath 2^>NUL ^| findstr InstallPath'
  ) do set R_HOME=%%~b
if not defined R_HOME for /f "tokens=2*" %%a in (
 'reg query hklm\software\wow6432Node\r-core\r /v InstallPath 2^>NUL ^| findstr InstallPath'
  ) do set R_HOME=%%~b
if not defined R_HOME echo "Error: R not found" & goto:eof

:: look for architecture in these places in this order:
:: - environment variable R_ARCH
:: - first arg if its --arch
:: - check if R_HOME\bin\i386 exists
:: - if R_HOME\bin\x64 exists
:: - if none of the above then use i386

if not defined R_ARCH if /i "%1"=="--arch=x64" set R_ARCH=x64
if not defined R_ARCH if /i "%1"=="--arch=64" set R_ARCH=x64
if not defined R_ARCH if /i "%1"=="--arch=i386" set R_ARCH=i386
if not defined R_ARCH if /i "%1"=="--arch=32" set R_ARCH=i386
if defined R_ARCH goto:archdefined
if exist %R_HOME%\bin\i386 (set R_ARCH=i386) & goto:arch_defined
if exist %R_HOME%\bin\x64 (set R_ARCH=x64) & goto:arch_defined
set R_ARCH=i386
:arch_defined

:: add R_MIKTEX to PATH if defined.  Otherwise if its not 
:: in the PATH already then check \Program Files\miktex* or \miktex* 
:: and if found add that to PATH.

:: if miktex found in PATH skip searching for it
PATH | findstr /i miktex > nul
if not errorlevel 1 goto:end_miktex

:: check for presence of %ProgramFiles%\miktex* or \miktex*

if not defined R_MIKTEX for /f "delims=" %%a in (
    'dir /b /on "%ProgramFiles%"\miktex* 2^>NUL'
) do set R_MIKTEX=%ProgramFiles%\%%a

if not defined R_MIKTEX for /f "delims=" %%a in (
    'dir /b /on %SystemDrive%:\miktex* 2^>NUL'
) do set R_MIKTEX=%SystemDrive%:\miktex\%%a

:end_miktex
if defined R_MIKTEX PATH %R_MIKTEX%\miktex\bin;%PATH%

if not defined R_TOOLS for /f "tokens=2*" %%a in (
 'reg query hklm\software\R-core\Rtools /v InstallPath 2^>NUL ^| findstr InstallPath'
 ) do set R_TOOLS=%%~b
if not defined R_TOOLS for /f "tokens=2*" %%a in (
 'reg query hklm\software\wow6432Node\Rtools /v InstallPath 2^>NUL ^| findstr InstallPath'
  ) do set R_TOOLS=%%~b

set PATHQ=%PATH%
:WHILE
    if "%PATHQ%"=="" goto WEND
    for /F "delims=;" %%i in ("%PATHQ%") do if exist "%%~sfi" set PATH2=%PATH2%;%%~sfi
    for /F "delims=; tokens=1,*" %%i in ("%PATHQ%") do set PATHQ=%%j
    goto WHILE 
:WEND

set path2=%path2:~1%

if defined R_TOOLS (
    set path2=%R_TOOLS%\bin;%R_TOOLS%\perl\bin;%R_TOOLS%\MinGW\bin;%PATH2%
)

path %path2%

set here=%CD%
set args=%*

:: get name by which this command was called
:: this allows same file to be used for Rgui, Rterm, etc. by just renaming it
for %%i in (%0) do set cmd=%%~ni.exe

if /i %cmd%==rtools.exe (endlocal & set path=%path2%) && goto:eof

cd %R_HOME%\bin
if /i not %cmd%==rguistart.exe goto:notRguiStart
  set cmd=rgui.exe
  set firstArgument=%1
  if defined firstArgument (
    dir %1 | findstr "<DIR>" > nul
    if errorlevel 1 goto:notRguiStart
    set here=%~1
    set firstArgument=
  )
  set args=
  shift
  :startloop
  set firstArgument=%1
  if defined firstArgument (
     set args=%args% "%~1" 
     shift
     goto:startloop
  )
:notRguiStart

set st=
if /i %cmd%==rgui.exe set st=start

if /i not %cmd%==#Rscript.exe goto:not#Rscript
set cmd=Rscript.exe
if [%1]==[] goto:help#Rscript
call :rsarg1 %*
goto:not#Rscript
:rsarg1
set args=%*
set arg1=%~1
set arg1=%arg1:.bat.bat=.bat%
set last4=%arg1:~-4%
if /i not "%last4%"==".bat" set arg1=%arg1%.bat
for %%a in ("%R_HOME%\bin\Rscript.exe") do set RSCRIPT=%%~sfa
call set args=%%args:%1="%arg1%"%%
rem call set args=%%args:%1=%%
goto:eof
:not#Rscript

cd %here%

:: Look in architecture specific subdirectory of bin. If not there look in bin.
set cmdpath=%R_HOME%\bin\%R_ARCH%\%cmd%
if exist "%cmdpath%" goto:cmdpathfound
set cmdpath=%R_HOME%\bin\%cmd%
if exist "%cmdpath%" goto:cmdpathfound
echo "Error: %cmd% not found" & goto:eof
:cmdpathfound

:: if called as jgr.bat locate the JGR package to find jgr.exe
if /i not %cmd%==jgr.exe goto:notJGR
  set st=start
  set cmdpath=jgr.exe
  if not defined JGR_LIBS set JGR_LIBS=%R_LIBS%
  for %%a in ("%R_HOME%\bin\Rscript.exe") do set RSCRIPT=%%~sfa
  if not defined JGR_LIBS for /f "usebackq delims=" %%a in (
		`%RSCRIPT% -e "cat(.libPaths(),sep=';')"`
  ) do set JGR_LIBS=%%~a
  if not defined JGR_LIBS (
	echo "Error: JGR package not found in R library" & goto:eof
  )
  for %%f in ("JGR") do set "jgrpkg=%%~$JGR_LIBS:f"
  set JGR_LIB=%jgrpkg:~0,-4%
  for %%a in ("%JGR_LIB%") do set JGR_LIB_SHORT=%%~sfa
  for %%a in ("%R_HOME%") do set R_HOME_SHORT=%%~sfa
  set args=--libpath=%JGR_LIB_SHORT% --rhome=%R_HOME_SHORT%

:notJGR

if defined st (start "" "%cmdpath%" %args%) else "%cmdpath%" %args%
goto:eof

:help#Rscript
echo Usage: #Rscript %%0 %%*
echo If the above is the first line in a file 
echo containing only R code and the file is 
echo given a .bat extension then it can be 
echo run as a batch file.



endlocal