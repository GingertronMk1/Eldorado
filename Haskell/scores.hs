nfls :: Int -> [String]
nfls n = [show td ++ " TD, " ++ show pat ++ " PAT, " ++ show tpa ++ " 2PA, " ++ show fg ++ " FG, " ++ show s ++ " S"
          |td  <-[0..div n 6]
          ,pat <-[0..td]
          ,tpa <-[0..td-pat]
          ,fg  <-[0..div n 3]
          ,s   <-[0..div n 2]
          ,((6*td)+pat+(3*fg)+(2*(s+tpa)))==n]

rus :: Int -> [String]
rus n = [show try ++ " T, " ++ show con ++ " Con, " ++ show thr ++ " Pen/DG"
         |try <-[0..div n 5]
         ,con <-[0..try]
         ,thr <-[0..div n 3]
         ,((5*try)+(2*con)+(3*thr))==n]

rls :: Int -> [String]
rls n = [show try ++ " T, " ++ show con ++ " Con, " ++ show pen ++ " Pen, " ++ show dg ++ " DG"
         |try <-[0..div n 4]
         ,con <-[0..try]
         ,pen <-[0..div n 2]
         ,dg  <-[0..n]
         ,((4*try)+(2*(con+pen))+dg)==n]

intersperse a (n:[]) = [n]
intersperse a (n:ns) = n:a:(intersperse a ns)

flatten ass = [a|as<-ass,a<-as]

nfl = putStrLn . flatten . intersperse "\n" . nfls
ru = putStrLn . flatten . intersperse "\n" . rus
rl = putStrLn . flatten . intersperse "\n" . rls
