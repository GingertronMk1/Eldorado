module Data where

import Type

captainTeam :: Team
captainTeam = "CAPTAIN"

captainTeams :: [Team]
captainTeams = [captainTeam]

offense :: LiterateLineup
offense =
  [ ( "qb",
      [ ("Justin Fields", ["Bears"]),
        ("Tim Tebow", ["Broncos", "Jets", "Legends"])
      ]
    ),
    ( "hb",
      [ ("Cordarrelle Patterson", ["Bears", "Falcons", "Raiders", "Patriots", "Vikings"]),
        ("Demarco Murray", ["Cowboys", "Eagles", "Titans", "Legends"]),
        ("Mark Ingram II", ["Saints", "Ravens", "Texans"])
      ]
    ),
    ( "fb",
      [ ("Jim Taylor", ["Packers", "Saints", "Legends"]),
        ("Reggie Gilliam", ["Bills"])
      ]
    ),
    ( "te",
      [ ("Dave Casper", ["Raiders", "Titans", "Vikings", "Legends"]),
        ("Evan Engram", ["Giants", "Jaguars"]),
        ("Dawson Knox", ["Bills"])
      ]
    ),
    ( "wr",
      [ ("DJ Moore", ["Panthers"]),
        ("Brandin Cooks", ["Patriots", "Rams", "Saints", "Texans"]),
        ("Devante Parker", ["Patriots", "Dolphins"]),
        ("Ceedee Lamb", ["Cowboys"]),
        ("Gabe Davis", ["Bills"])
      ]
    ),
    ( "lt",
      [ ("Orlando Brown", ["Chiefs", "Ravens"]),
        ("Terron Armstead", ["Dolphins", "Saints"])
      ]
    ),
    ( "lg",
      [ ("Isaac Seumalo", ["Eagles"])
      ]
    ),
    ( "c",
      [ ("Frank Ragnow", ["Lions"]),
        ("Tyler Shatley", ["Jaguars"])
      ]
    ),
    ( "rg",
      [ ("Nate Davis", ["Titans"])
      ]
    ),
    ( "rt",
      [ ("Lane Johnson", ["Eagles"])
      ]
    )
  ]

defense :: LiterateLineup
defense =
  [ ( "mlb",
      [ ("Isaiah Simmons", ["Cardinals"]),
        ("Tremaine Edmunds", ["Bills"]),
        ("Zaven Collins", ["Cardinals"])
      ]
    ),
    ( "rolb",
      [ ("Divine Deablo", ["Raiders"])
      ]
    ),
    ( "lolb",
      [ ("Jalen Reeves-Maybin", ["Lions", "Texans"])
      ]
    ),
    ( "ss",
      [ ("Harrison Smith", ["Vikings"]),
        ("Grant Delpit", ["Browns"])
      ]
    ),
    ( "fs",
      [ ("Trevon Moehrig", ["Raiders"]),
        ("Budda Baker", ["Cardinals"])
      ]
    ),
    ( "cb",
      [ ("Stephon Gilmore", ["Colts", "Bills", "Panthers", "Patriots"]),
        ("Sauce Gardner", ["Jets"]),
        ("Casey Hayward Jr", ["Raiders", "Packers", "Chargers", "Falcons"]),
        ("Sidney Jones IV", ["Seahawks", "Eagles", "Jaguars"]),
        ("Randy Moss", ["Raiders", "Patriots", "Titans", "Legends", "Vikings", "49ers"])
      ]
    ),
    ( "dt",
      [ ("Sam Adams", captainTeams),
        ("Deforest Buckner", ["49ers", "Colts"]),
        ("Derrick Brown", ["Panthers"])
      ]
    )
  ]

specialTeams :: LiterateLineup
specialTeams =
  [ ( "k",
      [ ("Justin Reid", ["Texans", "Chiefs"])
      ]
    ),
    ( "p",
      [ ("Sterling Hofrichter", ["Dolphins"])
      ]
    )
  ]

