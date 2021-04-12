#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
; #Warn  ; Enable warnings to assist with detecting common errors.
; SendMode Input ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.
#MaxThreads 2
#SingleInstance Force

loop = 1
While(loop > 0) {		; Circa an hour long loop
	Sleep 10000
	Loop 6 {
		Send {Down down}
		Sleep 50
		Send {Down up}
		Sleep 50
	}

	Sleep 50
	Send {Up down}
	Sleep 50
	Send {Up up}
	Sleep 500

	Loop 7 {
		Send {Space down}
		Sleep 50
		Send {Space up}
		Sleep 500
	}
	Sleep 30000
}

PgDn::loop:=0