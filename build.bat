echo Restoring dotnet tools...
dotnet tool restore
echo Restoring dotnet project references...
dotnet restore
echo Prebuilding...
dotnet build

SET PAKET_TOOL_PATH=.paket

IF NOT EXIST "%PAKET_TOOL_PATH%\paket.exe" (
  dotnet tool install paket --tool-path ./%PAKET_TOOL_PATH%
)

dotnet fake build -t %*
