REM Creates a symbolic link between the file in the git repository and the
REM startup folder in the start menu, which means that the file will be opened by
REM the default application when Windows is started.

REM mklink <target> <source>
mklink "c:\Users\eojebo\appdata\roaming\Microsoft\Windows\Start Menu\Programs\Startup\emacsify.ahkl" c:\code\configuration\autohotkey\emacsify.ahkl 