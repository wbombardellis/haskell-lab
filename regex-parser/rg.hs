-------- REGULAR GRAMMAR --------
module Rg (L, R(R, E), Rule((::=)), Rules, PTree(PT), parse) where
import Data.Maybe

---- Rules ----
type L = Char
data R = R Char L | E deriving (Eq, Show)

data Rule = (::=) L R deriving (Eq, Show)

type Rules = [Rule]

---- PARSING ----
data PTree = PT {ptrule::Rule, ptnext::Maybe PTree} deriving (Eq, Show)

parse :: String -> L -> Rules -> Maybe PTree
parse "" left rules = 
	let (ruleGoEmpty:_) = (select Nothing left rules) in
		Just (PT ruleGoEmpty Nothing)

parse input left rules =
	let applicables = select (Just (head input)) left rules in
		let notEmpties = filter (\r -> not (isNothing (sequel r))) applicables in
			let parses = map (\r -> let (Just s) = sequel r in 
						let pt = parse (tail input) s rules in
							if isNothing pt then Nothing else Just (PT r pt)) notEmpties in
								let ptrees = filter (\p -> not (isNothing p)) parses in
									if ptrees == [] then Nothing else head ptrees
			
select :: Maybe Char -> L -> Rules -> Rules
select tip left rules = 
	let candidates = filter (\((::=) l r) -> l == left) rules in
		filter (\((::=) l r) -> case r of E -> tip == Nothing;
						  R a _ -> tip == Just a) candidates

sequel :: Rule -> Maybe L
sequel ((::=) l E) = Nothing
sequel ((::=) l (R a s)) = Just s

---- TESTS ----
rE = ((::=) 'E' E)
r0 = ((::=) 'A' (R 'a' 'E'))
r1 = ((::=) 'A' (R 'a' 'A'))
g = [rE, r0, r1]

testRg =
	-- sequel
	sequel rE == Nothing
	&& sequel r0 == Just 'E'
	-- select
	&& select Nothing 'E' g == [rE]
	&& select Nothing 'A' g == []
	&& select (Just 'a') 'E' g == []
	&& select (Just 'a') 'A' g == [r0, r1]
	-- parse
	&& parse "" 'E' g == Just (PT rE Nothing)
	&& parse "a" 'E' g == Nothing
	&& parse "a" 'A' g == Just (PT r0 (Just (PT rE Nothing)))
	&& parse "aa" 'A' g == Just (PT r1 (Just (PT r0 (Just (PT rE Nothing))))) 
	&& parse "b" 'A' g == Nothing 
	&& parse "ab" 'A' g == Nothing
