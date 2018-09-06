nfls :: Int -> [String]
nfls n = let ns = [0..(div n 2)]
         in [show td ++ " TD, " ++ show pat ++ " PAT, " ++ show fg ++ " FG, " ++ show s ++ " S\n"
             | td<-ns,pat<-[0..td],fg<-ns,s<-ns,((6*td)+pat+(3*fg)+(2*s))==n]

rus :: Int -> [String]
rus n = let ns = [0..(div n 3)]
        in [show try ++ " T, " ++ show con ++ " Con, " ++ show thr ++ " Pen/DG\n"
            | try<-ns,con<-[0..try],thr<-ns,((5*try)+(2*con)+(3*thr))==n]

rls :: Int -> [String]
rls n = let ns = [0..n]
        in [show try ++ " T, " ++ show con ++ " Con, " ++ show dg ++ " DG\n"
            | try<-ns,con<-[0..try],dg<-ns,((4*try)+(2*con)+dg)==n]

flatten ass = [a|as<-ass,a<-as]

nfl = putStrLn . flatten . nfls
ru = putStrLn . flatten . rus
rl = putStrLn . flatten . rls
