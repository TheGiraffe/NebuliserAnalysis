#cs ----------------------------------------------------------------------------

 AutoIt Version: 3.3.16.1
 Author:         myName

 Script Function:
	Template AutoIt script.

#ce ----------------------------------------------------------------------------

; Script Start - Add your code below here
;~ Run("C:\Program Files (x86)\Malvern Instruments\Spraytec\RTSizer.exe")
;~ WinWaitActive("User name")
;~ ControlClick("User name", "", 1)
;~ WinWaitActive("ISAC Communications Package")
;~ ControlClick("ISAC Communications Package", "", 2)

WinWaitActive("Spraytec - [Water 1.psh]")
MouseClick("left", 89, 629) ;~ Click on the list item
MouseClick("left", 69, 33) ;~ Click on Edit
MouseClick("left", 108, 57) ;~ Click on Records

WinWaitActive("Select Range of Records to Edit")
ControlClick("Select Range of Records to Edit", "", 1108) ;~ Select the All radio button
ControlClick("Select Range of Records to Edit", "", 1220) ;~ Press Settings...

WinWaitActive("Edit result")
MouseClick("left", 379, 232) ;~ Click Analysis
MouseClick("left", 367, 271) ;~ Press the + next to Data Handling to expand the list
MouseClick("left", 367, 345) ;~ Press the + next to Additional properties to expand this list

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

;~ ;~  Used to test the full range of detector dropdowns
;~ for $i = 0 To 25
;~ 	If $i = 25 Then
;~ 		EditDetectorRange($i, $i)
;~ 	Else 
;~ 		EditDetectorRange($i+1, $i)
;~ 	EndIf
;~ 	Sleep(250)
;~ Next

EditDetectorRange(10, 0)
EditAlarms(25)
EditParticleDiameter(0.1, 2500)

;~ ControlClick("Edit result", "Cancel", 2)
;~ MouseClick("left", 810, 550) ;~ Cancel Button

MouseClick("left", 730, 550) ;~ Press OK Button
WinWaitActive("Select Range of Records to Edit")
MouseClick("left", 655, 300) ;~ Press next OK Button
WinWaitActive("Spraytec - [Water 1.psh]")
MouseClick("left", 36, 32) ;~ Go to File
Sleep(100)
MouseClick("left", 75, 125) ;~ Go to Export...
WinWaitActive("Select Range of Records to Save")
ControlClick("Select Range of Records to Save", "", 1108) ;~ Select the All radio button
ControlClick("Select Range of Records to Save", "", 1134) ;~ Select the All radio button
MouseClick("left", 655, 300) ;~ Press the OK Button

WinWaitActive("Save As")
$filename = "Exp020-1000.txt"
ControlSend("Save As", "", 1001, $filename)
Sleep(500)
MouseClick("left", 780, 555) ;~ Press the Save Button

WinWaitActive("Spraytec - [Water 1.psh]")
Send("#^{RIGHT}")
Sleep(500)
WinWaitActive("AnalysisParameters - Excel")
MouseClick("left", 750, 265)
Sleep(100)
Send( "{BACKSPACE}" )
MouseClick("left", 400, 200)
Sleep(100)
Send($filename)
Sleep(500)
Send("{Enter}")
Sleep(500)
Send("#^{LEFT}")