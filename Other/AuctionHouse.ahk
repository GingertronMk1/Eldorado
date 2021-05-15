#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
; #Warn  ; Enable warnings to assist with detecting common errors.
; SendMode Input ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.
#MaxThreads 2
#SingleInstance Force

While(True) {
	If (GetKeyState("ScrollLock", "T")) {
		Send {q down}
		Sleep 50
		Send {q up}
		Sleep 200
		Send {q down}
		Sleep 50
		Send {q up}
		Sleep 50
	}
	Random, rand, 15000, 16000
	Sleep rand
}