-- 123 is left
-- 124 is right
-- 125 is down
-- 126 is up 
-- 76 is enter
-- 36 is return

tell application "Terminal"
	activate
	tell application "System Events"
		repeat 5 times
			key code 126
			delay 1
		end repeat
	end tell
end tell

