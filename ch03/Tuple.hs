-- file: ch03/Tuple.hs

third (_,_,c) = c

complicated (True, a, x:xs, 5) = (a, xs)
