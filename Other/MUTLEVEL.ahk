#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
; #Warn  ; Enable warnings to assist with detecting common errors.
; SendMode Input ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.

+/::
Loop 6 {
	Send {Down down}
	Sleep 20
	Send {Down up}
	Sleep 50
}

Sleep 50
Send {Up down}
Sleep 20
Send {Up up}
Sleep 500

Loop 7 {
	Send {Space down}
	Sleep 20
	Send {Space up}
	Sleep 500
}

return



