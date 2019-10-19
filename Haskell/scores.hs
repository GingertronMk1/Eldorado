ppStrings :: [String] -> IO ()
ppStrings = putStrLn . concat . intersperse "\n"
            where intersperse _ []     = []
                  intersperse _ [n]    = [n]
                  intersperse a (n:ns) = n:a:(intersperse a ns)

nfl :: Int -> IO ()
nfl = ppStrings . nfls
      where nfls n = [ show td
                   ++ " TD,\t"  -- Touchdowns
                   ++ show pat
                   ++ " PAT,\t" -- Point Afters
                   ++ show tpa
                   ++ " 2PA,\t" -- 2-point tries
                   ++ show fg
                   ++ " FG,\t"  -- Field Goals
                   ++ show s
                   ++ " S,\t"   -- Safeties
                    | td  <-[0..div n 6]
                    , pat <-[0..td]
                    , tpa <-[0..td-pat]
                    , fg  <-[0..div n 3]
                    , s   <-[0..div n 2]
                    , (6 * td) + pat + (3 * fg) + (2 * (s + tpa)) == n ]

ru :: Int -> IO ()
ru = ppStrings . rus
     where rus n = [ show try
                  ++ " T,\t"    -- Tries
                  ++ show con
                  ++ " Con,\t"  -- Conversions
                  ++ show thr
                  ++ " Pen/DG"  -- Penalties/Drop Goals
                   | try <-[0..div n 5]
                   , con <-[0..try]
                   , thr <-[0..div n 3]
                   , (5 * try) + (2 * con) + (3 * thr) == n ]


rl :: Int -> IO ()
rl = ppStrings . rls
     where rls n = [ show try
                  ++ " T,\t"    -- Tries
                  ++ show con
                  ++ " Con,\t"  -- Conversions
                  ++ show pen
                  ++ " Pen,\t"  -- Penalty Goals
                  ++ show dg
                  ++ " DG"      -- Drop Goals
                   | try <-[0..div n 4]
                   , con <-[0..try]
                   , pen <-[0..div n 2]
                   , dg  <-[0..n]
                   , (4 * try) + (2 * (con + pen)) + dg == n ]
