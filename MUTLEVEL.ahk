#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
; #Warn  ; Enable warnings to assist with detecting common errors.
; SendMode Input ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.
#MaxThreads 2
#SingleInstance Force

loop = 1
While(loop > 0) {		; Circa an hour long loop
	Sleep 10000
	ReRun()
	Sleep 30000
}

PgDn::ReRun()

ReRun() {
	Loop 6 {				; Make sure we're at the bottom of the options list
		Send {Down down}
		Sleep 50
		Send {Down up}
		Sleep 50
	}

	Sleep 50				; 'Replay Challenge' is the second from bottom
	Send {Up down}
	Sleep 50
	Send {Up up}
	Sleep 500

	Loop 7 {				; Mash space bar until we do the first recommended play
		Send {Space down}
		Sleep 50
		Send {Space up}
		Sleep 500
	}

}