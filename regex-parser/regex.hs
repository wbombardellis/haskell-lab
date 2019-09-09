-------- REGULAR EXPRESSION --------
module Regex where
import Rg
import Data.Maybe

---- COMPILER ----
compile :: String -> Rules
compile regex = 
	let pTree = parse regex 'R' regexg in
		case pTree of Nothing -> [];
			      Just pt -> let (r, p) = transform pt in
						if isNothing p then [] else r

transform :: PTree -> (Rules,Maybe PTree)
transform (PT rRE Nothing) = ([genEmpty], Nothing)
transform (PT r Nothing) = ([], Just (PT r Nothing))

transform (PT r (Just child)) = 
	-- go to the bottom
	-- identify matches during return
	-- upon match: build grammar/ rules and sum with accumulated grammar
	-- or
	-- transform child ptree
	let (rulesSoFar, restPT) = transform child in
		-- check current state for a match
		let newRules = match r restPT in
			-- sum both rules to construct new grammar
			if newRules /= [] then (sumRules rulesSoFar newRules, Nothing)
			else (rulesSoFar, Just (PT r restPT))
				


match :: Rule -> Maybe PTree -> Rules
match rule _ = [] --TODO: Implement it

-- TODO: Connect rules
sumRules :: Rules -> Rules -> Rules
sumRules a b = a ++ b

genEmpty :: Rule
genEmpty = rRE --TODO: Enhance

---- REGEX GRAMMAR INSTANCE ----
rR0 = (::=) 'R' (R 'a' 'R')
rR1 = (::=) 'R' (R 'a' 'O')
rR2 = (::=) 'R' (R 'a' 'M')
rR3 = (::=) 'R' (R '(' 'P')
rRE = (::=) 'R' E

rO = (::=) 'O' (R '?' 'R')
rM = (::=) 'M' (R '+' 'R')
rP0 = (::=) 'P' (R 'a' 'Q')
rP1 = (::=) 'P' (R 'a' 'I')
rQ0 = (::=) 'Q' (R ')' 'R')
rQ1 = (::=) 'Q' (R ')' 'O')
rQ2 = (::=) 'Q' (R ')' 'M')
rI = (::=) 'I' (R '|' 'A')
rA = (::=) 'A' (R 'a' 'Q')

regexg = [rO, rM, rP0, rP1, rQ0, rQ1, rQ2, rI, rA, rR0, rR1, rR2, rR3, rRE]

---- TESTS ----
testRegex = 
	parse "a" 'R' regexg == Just (PT rR0 (Just (PT rRE Nothing)))
	&& parse "(a)" 'R' regexg == Just (PT rR3 (Just (PT rP0 (Just (PT rQ0 (Just (PT rRE Nothing)))))))
	&& parse "(a|a)" 'R' regexg == Just (PT rR3 (Just (PT rP1 (Just (PT rI (Just (PT rA (Just (PT rQ0 (Just (PT rRE Nothing)))))))))))
	&& parse "(a)+" 'R' regexg == Just (PT rR3 (Just (PT rP0 (Just (PT rQ2 (Just (PT rM (Just (PT rRE Nothing)))))))))
	&& parse "(a)?" 'R' regexg == Just (PT rR3 (Just (PT rP0 (Just (PT rQ1 (Just (PT rO (Just (PT rRE Nothing)))))))))
	&& parse "a?" 'R' regexg == Just (PT rR1 (Just (PT rO (Just (PT rRE Nothing)))))
	&& parse "a+" 'R' regexg == Just (PT rR2 (Just (PT rM (Just (PT rRE Nothing)))))
