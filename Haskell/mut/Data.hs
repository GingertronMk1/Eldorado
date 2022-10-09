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
        ("Demarco Murray", ["Cowboys", "Eagles", "Titans", "Legends"])
      ]
    ),
    ( "fb",
      [ ("Jim Taylor", ["Packers", "Saints", "Legends"])
      ]
    ),
    ( "te",
      [ ("Dave Casper", ["Raiders", "Titans", "Vikings", "Legends"]),
        ("Evan Engram", ["Giants", "Jaguars"])
      ]
    ),
    ( "wr",
      [
        ("Larry Fitzgerald", ["Cardinals", "Legends"]),
        ("Michael Crabtree", ["49ers", "Ravens", "Cardinals", "Raiders", "Legends"]),
        ("DJ Moore", ["Panthers"]),
        ("Brandin Cooks", ["Patriots", "Rams", "Saints", "Texans"]),
        ("Ceedee Lamb", ["Cowboys"])
      ]
    ),
    ( "lt",
      [ ("Orlando Brown", ["Chiefs", "Ravens"])
      ]
    ),
    ( "lg",
      [ ("Andrew Norwell", ["Commanders", "Jaguars", "Panthers"])
      ]
    ),
    ( "c",
      [ ("Frank Ragnow", ["Lions"])
      ]
    ),
    ( "rg",
      [ ("Mark Glowinski", ["Giants", "Colts", "Seahawks"])
      ]
    ),
    ( "rt",
      [ ("La'el Collins", ["Cowboys", "Bengals"])
      ]
    )
  ]

defense :: LiterateLineup
defense =
  [ ( "mlb",
      [ ("Kiko Alonso", ["Legends", "Bills", "Dolphins", "Eagles", "Saints"]),
        ("Tremaine Edmunds", ["Bills"])
      ]
    ),
    ( "rolb",
      [ ("Jonathan Casillas", ["Legends", "Buccaneers", "Giants", "Patriots", "Saints"])
      ]
    ),
    ( "lolb",
      [ ("Ted Hendricks", ["Colts", "Packers", "Raiders", "Legends"])
      ]
    ),
    ( "ss",
      [ ("Harrison Smith", ["Vikings"]),
        ("Grant Delpit", ["Browns"])
      ]
    ),
    ( "fs",
      [ ("Trevon Moehrig", ["Raiders"]),
        ("Marcus Williams", ["Saints", "Ravens"])
      ]
    ),
    ( "cb",
      [ ("Stephon Gilmore", ["Colts", "Bills", "Panthers", "Patriots"]),
        ("Sauce Gardner", ["Jets"]),
        ("Casey Hayward Jr", ["Raiders", "Packers", "Chargers", "Falcons"]),
        ("Sidney Jones IV", ["Seahawks", "Eagles", "Jaguars"])
      ]
    ),
    ( "dt",
      [ ("Sam Adams", captainTeams),
        ("Deforest Buckner", ["49ers", "Colts"])
      ]
    )
  ]

specialTeams :: LiterateLineup
specialTeams =
  [ ( "k",
      [ ("Zane Gonzalez", ["Browns", "Cardinals", "Panthers"])
      ]
    ),
    ( "p",
      [ ("Sterling Hofrichter", ["Dolphins"])
      ]
    )
  ]

