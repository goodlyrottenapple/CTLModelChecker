module Test where
import CTLModelChecker
import Parser

initialS = S [0]

graph = G [(0,1), (1,3), (1,4), (2,2), (3,2), (4,4)]

labeling :: Labeling
labeling 0 = ["a"]
labeling 1 = ["a", "b"]
labeling 2 = ["c"]
labeling 3 = ["b", "c"]
labeling 4 = ["c"]
labeling _ = []

satF :: String -> Bool
satF s = initialS `subseq` sat (parseF s) graph labeling


satFS :: String -> States
satFS s = sat (parseF s) graph labeling