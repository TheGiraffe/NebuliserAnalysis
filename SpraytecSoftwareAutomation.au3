#cs ----------------------------------------------------------------------------

 AutoIt Version: 3.3.16.1
 Author:         Sophia Davis

 Script Function:
	Template AutoIt script.

#ce ----------------------------------------------------------------------------

; Script Start - Add your code below here

#include <Array.au3>
#include <File.au3>

Func EditDetectorRange($First, $DistanceFromLast)
	MouseClick("left", 412, 274) ;~ Click Data Handling
	Sleep(500)
	MouseClick("left", 600, 230) ;~ Click the First dropdown menu
	Sleep(500)
	MouseClick("left", 600, (245 + 12.5*($First - 1)))
	Sleep(500)
	MouseClick("left", 600, 260) ;~ Click the Last dropdown menu
	Sleep(500)
	MouseClick("left", 600, (274 + 12.5*($DistanceFromLast)))
	Sleep(500)
EndFunc

Func EditAlarms($MinScattering)
	MouseClick("left", 411, 295) ;~ Click Alarms
	Sleep(500)
	;~ MouseClick("left", 497, 205) ;~ Uncheck Use default values
	;~ Sleep(100)
	MouseMove(600, 400)
	ControlSetText("Edit result","","[CLASSNN:Edit4]", $MinScattering)
	Sleep(500)
EndFunc

Func EditParticleDiameter($Min, $Max)
	MouseClick("left", 413, 363) ;~ Click Advanced
	Sleep(500)
	MouseMove(600, 400)
	ControlSetText("Edit result","","[CLASSNN:Edit1]", $Min)
	ControlSetText("Edit result","","[CLASSNN:Edit2]", $Max)
	Sleep(500)
EndFunc

Func TestDetectorDropdownLocations()
	;~ ;~  Used to test the full range of detector dropdowns
	for $i = 0 To 25
		If $i = 25 Then
			EditDetectorRange($i, $i)
		Else 
			EditDetectorRange($i+1, $i)
		EndIf
		Sleep(250)
	Next
EndFunc

Func EditRecordAndExportData($pshName, $pshLocation, $exportfilename, $detectorFirst, $detectorDistanceFromLast, $MinScatteringError, $particleDiameterMin, $particleDiameterMax)
	WinWaitActive($pshName)
	MouseClick("left", $pshLocation[0], $pshLocation[1]) ;~ Click on the list item
	MouseClick("left", 69, 33) ;~ Click on Edit
	MouseClick("left", 108, 57) ;~ Click on Records

	WinWaitActive("Select Range of Records to Edit")
	ControlClick("Select Range of Records to Edit", "", 1108) ;~ Select the All radio button
	ControlClick("Select Range of Records to Edit", "", 1220) ;~ Press Settings...

	WinWaitActive("Edit result")
	MouseClick("left", 379, 232) ;~ Click Analysis
	MouseClick("left", 367, 271) ;~ Press the + next to Data Handling to expand the list
	MouseClick("left", 367, 345) ;~ Press the + next to Additional properties to expand this list

	EditDetectorRange($detectorFirst, $detectorDistanceFromLast)
	EditAlarms($MinScatteringError)
	EditParticleDiameter($particleDiameterMin, $particleDiameterMax)

	;~ ControlClick("Edit result", "Cancel", 2)
	;~ MouseClick("left", 810, 550) ;~ Cancel Button

	MouseClick("left", 730, 550) ;~ Press OK Button
	WinWaitActive("Select Range of Records to Edit")
	MouseClick("left", 655, 300) ;~ Press next OK Button
	Sleep(1500)

	WinWaitActive($pshName)
	MouseClick("left", 36, 32) ;~ Go to File
	Sleep(500)
	MouseClick("left", 75, 125) ;~ Go to Export...
	Sleep(100)
	WinWaitActive("Select Range of Records to Save")
	ControlClick("Select Range of Records to Save", "", 1108) ;~ Select the All radio button
	ControlClick("Select Range of Records to Save", "", 1134) ;~ Select the All radio button
	MouseClick("left", 655, 300) ;~ Press the OK Button

	WinWaitActive("Save As")
	ControlSend("Save As", "", 1001, $exportfilename)
	Sleep(500)
	MouseClick("left", 780, 555) ;~ Press the Save Button

	WinWaitActive($pshName)
	Send("#^{RIGHT}")
	Sleep(500)
	WinWaitActive("AnalysisParameters - Excel")
	;~ MouseClick("left", 750, 265)
	;~ Sleep(100)
	;~ Send( "{BACKSPACE}" )
	MouseClick("left", 400, 200)
	Sleep(100)
	Send($exportfilename)
	Sleep(500)
	Send("{Enter}")
	Sleep(500)
	Send("#^{LEFT}")
EndFunc

global $AnalysisParameters[1]
$AnalysisParameterFile = "C:\Users\sodav\Desktop\BEng\NebuliserAnalysis\AnalysisParametersList.csv"
_FileReadToArray($AnalysisParameterFile, $AnalysisParameters, Default, ",")
$intLineCount = _FileCountLines($AnalysisParameterFile) - 2
ConsoleWrite($intLineCount & @CRLF)
ConsoleWrite($AnalysisParameters[2][0] & @CRLF)

;~ $pshName = "Spraytec - [Exp20-Water.psh]"
;~ global $pshLoc[2] = [90, 609]
;~ global $exp = "Exp20"

$pshName = "Spraytec - [ Exp029-5pcEthanol.psh]"
global $pshLoc[2] = [90, 460]
global $exp = "Exp29"

For $i=0 To $intLineCount
	$fn = $exp & "-" & String(1000 + $i) & ".txt"
	ConsoleWrite($fn & @CRLF)
	EditRecordAndExportData($pshName, $pshLoc, $fn, $AnalysisParameters[$i+2][1], $AnalysisParameters[$i+2][2], $AnalysisParameters[$i+2][0], $AnalysisParameters[$i+2][3], $AnalysisParameters[$i+2][4])
	Sleep(500)
Next


;~ Unused codes but kept for reference:
;~ Run("C:\Program Files (x86)\Malvern Instruments\Spraytec\RTSizer.exe")
;~ WinWaitActive("User name")
;~ ControlClick("User name", "", 1)
;~ WinWaitActive("ISAC Communications Package")
;~ ControlClick("ISAC Communications Package", "", 2)