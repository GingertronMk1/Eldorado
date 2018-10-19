import Utils



sample :: [Char]
sample = "title: \"DEAD: A Musical\"\nplaywright: Lawrence Cuthbert and Josh Mallalieu\nstudent_written: true\nseason: In House\nseason_sort: 140\nperiod: Autumn\nvenue: New Theatre\ndate_start: 2016-12-13\ndate_end: 2016-12-16\ncast:\n- role: George\nname: Harry Pavlou\n- role: Carol\nname: Rachel Connolly\n- role: Madge\nname: Sasha Gibson\n- role: Baron\nname: Cameron Walker\n\ncrew:\n- role: Director\nname: Laurence Cuthbert\n- role: Director\nname: Josh Mallalieu\n- role: Producer\nname: Emily Sterling\n- role: Musical Director\nname: Jacob Lloyd\n- role: Technical Director\nname: Nathan Penney\n\n---\n\nA fire breaks out in a Victorian manor house in 1895, killing all inside. Unfortunately, Heaven doesn’t notice. For ninety years the six neglected spirits sit about wondering what went wrong, until a young woman bursts through the door bringing all the glitz and glamour of the 1980s and throwing their ghoulish existences into chaos. Within a week, the young bachelor gentleman of the ghosts has fallen head over heels for the new tenant who can neither see nor hear him. Meanwhile, his best friend prepares to venture tentatively into the outside world for the first time, and the blustering patriarch of the ghost family struggles to conceal a dangerous secret from his love-starved wife, who is determined to breathe fresh passion into their long lifeless marriage. In an original musical that features everything from dancing mystics to underpaid angels and trips to the afterlife and back, everyone will be shown that it is never too late to begin life again, even if you’re already dead.\n"

srtest :: [Char] -> Bool
srtest = elem "student_written: true" . lines

sr cs = (takeWhile (/="crew:") cs,dropWhile (/="crew:") cs)
