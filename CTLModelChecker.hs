module CTLModelChecker where
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

data Formula = T 
             | A String 
             | And Formula Formula 
             | Not Formula 
             | ENext Formula 
             | EUntil Formula Formula
             | ANext Formula
             | AUntil Formula Formula

instance Show Formula where
    show T = "true"
    show (A x) = x
    show (And x y) = show x ++ " /\\ " ++ show y
    show (Not (A x)) = "¬" ++ x
    show (Not x) = "¬(" ++ show x ++ ")"
    show (ENext (A x)) = "∃○" ++ x
    show (ANext (A x)) = "∀○" ++ x
    show (ENext x) = "∃○(" ++ show x ++ ")"
    show (ANext x) = "∀○(" ++ show x  ++ ")"
    show (EUntil x y) = "∃(" ++ show x ++ " U " ++ show y ++ ")"
    show (AUntil x y) = "∀(" ++ show x ++ " U " ++ show y ++ ")"

type Node = Integer

newtype States = S [Node] deriving Show

_S :: States -> [Node] 
_S (S s) = s

instance Eq States where
    (S l1) == (S l2) = Set.fromList l1 == Set.fromList l2

newtype Graph = G [(Node, Node)] deriving Show

type Labeling = (Node -> [String])

statesLift :: (Set Node -> Set Node -> Set Node) -> States -> States -> States
statesLift f (S x) (S y) = S $ Set.toList $ f (Set.fromList x) (Set.fromList y)

intersection :: States -> States -> States
intersection = statesLift Set.intersection

un :: States -> States -> States
un = statesLift Set.union

diff :: States -> States -> States
diff = statesLift Set.difference

subseq :: States -> States -> Bool
subseq (S x) (S y) = Set.isSubsetOf (Set.fromList x) (Set.fromList y)

empty :: States
empty = S []


post :: Graph -> Node -> States
post (G g) a = S $ (map snd) $ filter (\x -> fst x == a) g

states :: Graph -> States
states (G g) = S $ foldr (\ a res -> if a `elem` res then res else a:res) 
    (foldr (\ a res -> if a `elem` res then res else a:res) [] (map snd g)) 
        (map fst g)

sat :: Formula -> Graph -> Labeling -> States
sat T g l = states g
sat (A a) g l = trace("Formula: " ++ show (A a) ++ "\nSet: " ++ show r) $ r where 
    r = S [s | s <- allS, a `elem` (l s)]
    (S allS) = states g
sat (And x y) g l = trace("Formula: " ++ show (And x y) ++ "\nSet: " ++ show r) $ r where r = (sat x g l) `intersection` (sat y g l)
sat (Not x) g l = trace("Formula: " ++ show (Not x) ++ "\nSet: " ++ show r) $ r where 
    r = allS `diff` (sat x g l)
    allS = states g
sat (ENext x) g l = trace("Formula: " ++ show (ENext x) ++ "\nSet: " ++ show r) $ r where 
    r = S [s | s <- allS, (post g s) `intersection` (sat x g l) /= empty]
    (S allS) = states g
sat (ANext x) g l = trace("Formula: " ++ show (ANext x) ++ "\nSet: " ++ show r) $ r where 
    r = S [s | s <- allS, (post g s) `subseq` (sat x g l)]
    (S allS) = states g
sat (EUntil x y) g l = trace("Formula: " ++ show (EUntil x y) ++ "\nSet: " ++ show r) $ r where r = eUntil (sat y g l) (sat x g l) g
sat (AUntil x y) g l = trace("Formula: " ++ show (AUntil x y) ++ "\nSet: " ++ show r) $ r where r = aUntil (sat y g l) (sat x g l) g


eUntil :: States -> States -> Graph -> States
eUntil t satX g = if new `subseq` t then t else --trace ("new: " ++ show new ++ "t: " ++ show t) $ 
    eUntil (t `un` new) satX g
    where new = S [s | s <- _S satX, (post g s) `intersection` t /= empty ]

aUntil :: States -> States -> Graph -> States
aUntil t satX g = if new `subseq` t then t else --trace ("new: " ++ show new ++ "t: " ++ show t) $ 
    eUntil (t `un` new) satX g
    where new = S [s | s <- _S satX, (post g s) `subseq` t ]
