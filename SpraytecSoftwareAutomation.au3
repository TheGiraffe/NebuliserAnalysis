#cs ----------------------------------------------------------------------------

 AutoIt Version: 3.3.16.1
 Author:         myName

 Script Function:
	Template AutoIt script.

#ce ----------------------------------------------------------------------------

; Script Start - Add your code below here
Run("C:\Program Files (x86)\Malvern Instruments\Spraytec\RTSizer.exe")
WinWaitActive("User name")
ControlClick("User name", "", 1)
WinWaitActive("ISAC Communications Package")
ControlClick("ISAC Communications Package", "", 2)