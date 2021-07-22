
-- This file is organized as such:
--   some data types and with their short derived instances
--   pickCard and its required functions
--   playCard and its required functions
--   makeMelds and its required functions
--   binTree data type and its utility functions
--   convenient functions
--   longer derived instances


module Player where

import Parser.Parser 
import Rummy.Types   
import Cards        

-- | Added imports
import Data.List(sort)
import Data.Monoid
import Data.Char
import Control.Monad
import Parser.Instances

-- | Some data types

-- Data types to separate processes cards from unprocessed cards
-- Melder works with melds and non melds,
-- while Potential groups cards which will form Leftovers eventually

data Melder = Melder {nonMeld :: [Card], melded :: Maybe [Meld]}

data Potential = Potential {noGroup :: [Card], gotGroup :: [[Card]]}
       
-- To store results of parsing memory, composed of two parts
-- deckHeight - number of cards left in the stock
-- notInDeck - cards that have already appeared in player's hand / on the discard pile, not in deck anymore
data MemoryData = MemoryData { deckHeight :: Int, notInDeck :: [String] }

-- Data type to determine the potential melds to be formed from cards that cannot be made into melds yet
data Leftover = StrPair Card Card | SetPair Card Card | Single Card
    deriving Eq


-- [   PICK CARD   ]  

-- | What does pick card do?
-- Main task: Check whether to take from Discard or Stock;
-- Will only pick from the discard if it will be able to form melds with cards that are 
-- unable to form melds yet
--
-- Subtask: Update memory;
-- Initialize memory upon first round of the game
-- Update the memory to record the card that appears on top of discard pile 
-- Update the memory to decrement deck height if anyone draws from the deck

pickCard :: ActionFunc
pickCard discardCard _ memString opponentAction myHand = 
    if pickDecision myHand discardCard == Stock then (Stock, removeMaybeString resultString) 
    else (Discard, removeMaybeString (updateDeckHeight resultString))
    where
        resultString = memUpdater memString opponentAction myHand discardCard


-- | Check whether to take from Discard or stock

-- Determines whether to pick from the Discard or the Stock
pickDecision :: [Card] -> Card -> Draw
pickDecision l c = if takeFromDiscard l c then Discard else Stock

-- Returns True if card from discard pile can form melds with current Leftover cards
takeFromDiscard :: [Card] -> Card -> Bool
takeFromDiscard = flip (any.foundMatch).filterPairs.allLeftover
    where
        filterPairs = filter isPair

-- Given a card and a Leftover, returns true if they can form a meld together
foundMatch :: Card -> Leftover -> Bool
foundMatch c l@(StrPair _ _ ) = verifyNeighbourStraight c l
foundMatch c l@(SetPair _ _) = verifyNeighbourSet c l
foundMatch _ _ = False


-- | Verify neighbours for potential melds 

-- Checks whether given card and leftover can form Straight Meld together
verifyNeighbourStraight :: Card -> Leftover -> Bool
verifyNeighbourStraight a (StrPair b c) = getSuit a == getSuit b && (safeTestRank a b pred || safeTestRank a c succ)
verifyNeighbourStraight _ _ = False

-- Returns true if two cards are of neighbouring ranks
-- Only attempts to verify neighbouring ranked cards if safe to do so
safeTestRank :: Card -> Card -> (Rank -> Rank) -> Bool
safeTestRank x y f 
    | shouldTest y = verifyNeighbourRank x y f
    | otherwise = False
        where
            shouldTest c = orderByRank c /= 0 && orderByRank c /= 12
            verifyNeighbourRank a b f1 = getRank a == f1 (getRank b)

-- Checks whether given card and leftover can form Set Meld together
verifyNeighbourSet :: Card -> Leftover -> Bool
verifyNeighbourSet a (SetPair b _) = getRank a == getRank b && getSuit a /= getSuit b
verifyNeighbourSet _ _ = False


-- | Form Leftover from cards unable to form melds 

-- Gets all cards that are unable to form melds as a list of Leftover
allLeftover :: [Card] -> [Leftover]
allLeftover l = let c = groupNonMelds l in 
    (join $ map convertLeftover (gotGroup c)) ++ (convertLeftover (noGroup c))

-- Group all non melds into pairs of straights / pairs of sets / singles into Potential context
-- gotGroup -> identified pairs of cards to form straights or sets
-- the rest is in noGroup
groupNonMelds :: [Card] -> Potential
groupNonMelds = getSetPairs.getStraightPairs.findNonMeld 

-- Find all cards that are not melds yet, these are the cards to form potential straights or sets from
findNonMeld :: [Card] -> [Card]
findNonMeld = nonMeld.undeadMelds

-- Find pairs of cards that can potentially form straights and place in Potential context
getStraightPairs :: [Card] -> Potential
getStraightPairs c = foldr placePotential (Potential [] []) (possibleStraight c)

-- Called after getStraightPairs
-- Find pairs of cards that can potentially form sets while retaining straight pairs in gotGroup compartment
getSetPairs :: Potential -> Potential
getSetPairs p = foldr placePotential (Potential [] (gotGroup p)) (groupRank (noGroup p))

-- Given a list of cards, place them in the appropriate compartment in Potential context
placePotential :: [Card] -> Potential -> Potential
placePotential c pot
    | length c > 1 = Potential (noGroup pot) (gotGroup pot ++ [c])
    | otherwise = Potential (noGroup pot ++ c) (gotGroup pot)

-- Checks whether a Leftover is a pair
isPair :: Leftover -> Bool
isPair (StrPair _ _ ) = True
isPair (SetPair _ _ ) = True
isPair _ = False

-- | Updating memory 

-- To update memory by:
-- Initializing memory upon the first round of the game; then update deck height and card sequence
memUpdater :: Maybe String -> Maybe Draw -> [Card] -> Card -> Maybe String
memUpdater Nothing d hand c = memUpdater (initializeMem Nothing hand) d hand c
memUpdater str@(Just _) d hand c = auxiliaryUpdater str hand c d

-- Helper function for memUpdater
-- updates deck height if opponent draws from stock previously;
-- and includes card on top of discard pile into memory
auxiliaryUpdater :: Maybe String -> [Card] -> Card -> Maybe Draw -> Maybe String
auxiliaryUpdater Nothing _ _ _ = Nothing
auxiliaryUpdater memStr _ c (Just Stock) = updateDeckHeight (updateCardSeq c memStr)
auxiliaryUpdater memStr _ c _ = updateCardSeq c memStr

-- During the first round of the game, update player's current hand into memory
initializeMem :: Maybe String -> [Card] -> Maybe String
initializeMem Nothing c = lift (++) ((Just . ("(32)["++).join.(<$>)show) c) (Just "]")
initializeMem s _ = s 

-- Includes another card into memory to remember cards that are not in deck anymore
updateCardSeq :: Card -> Maybe String ->  Maybe String
updateCardSeq _ Nothing = Nothing
updateCardSeq c (Just s) = if linearSearch (notInDeck $ getCardSequence s) (show c)
                            then (Just s)
                            else let (front, back) = parseTillLast s in Just (front ++ show c ++ back)

-- Updates memory by decrementing deck height if anyone draws from stock
updateDeckHeight :: Maybe String -> Maybe String
updateDeckHeight Nothing = Nothing
updateDeckHeight (Just s) = let (memData, rest) = parseDeckHeight s in 
                        Just $ "(" ++ newDeckHeight (deckHeight memData) ++ ")" ++ rest
    where 
        newDeckHeight x = if x <= 10 then "0" ++ show (x - 1) else show (x - 1)


-- [   PLAY CARD   ]  

-- | What does play card do ?
-- First, builds a memory tree;
-- 1) updates the last picked card into memory
-- 2) from the updated memory, build a tree of all the cards that are not in the deck anymore
--
-- Next, decide what to discard;
-- 1) find all cards that are unable to form melds from current hand, then group and sort as leftovers
-- 2) check leftovers starting from lowest priority (first in order) whether they can potentially form melds
--    by determining whether their neighbours are still in the deck; if all not in deck, that is the one to discard

-- Then, decide what to call;
-- 1) replace card to discard with last picked card and determine what to call 

playCard :: PlayFunc
playCard lastPicked _ memString myHand = ((Action (whatToCall newHand updatedMemory) cardToDiscard), updatedMemory)
    where
        updatedMemory = removeMaybeString (updateCardSeq lastPicked (Just memString))
        cardToDiscard = whoDiscard myHand updatedMemory
        newHand = replaceCard myHand cardToDiscard lastPicked 


-- | Builds a Binary Card Tree from memorized card sequence

-- Gets the card sequence from the memory and builds a binary tree from it
memoryTree :: String -> BinTree Card
memoryTree = buildTree.cardSeqToCardList

-- Converts memory string card sequence into an actual list of cards
cardSeqToCardList :: String -> [Card]
cardSeqToCardList = map stringToCard.notInDeck.getCardSequence

-- Converts a string representing a card into an actual card
stringToCard :: String -> Card
stringToCard s = let (su, r) = charInMaybeTuple (applyParser (isCardSuit) s) in
    Card (showToSuit su) (numToRank $ actualNumber r)

-- Given characters, determine which suit it represents
-- Call together with isCardSuit so that no invalid characters will appear
showToSuit :: Char -> Suit
showToSuit 'D' = Diamond
showToSuit 'H' = Heart
showToSuit 'S' = Spade
showToSuit _ = Club

-- Converts an integer into a rank it represents
numToRank :: Int -> Rank
numToRank = toEnum


-- | Determine which card to discard

-- Given a list of cards, returns card to be discarded
-- If there are no leftovers (all cards can form melds), discard the smallest card in the hand
-- If all leftovers still have their neighbours in the deck, discard the one with lowest priority
-- otherwise, discard a card part of a leftover that has no more neighbours in the deck
whoDiscard :: [Card] -> String -> Card
whoDiscard hand memString
    | length left == 0 = head $ sort hand
    | length toExecute == 0 = executioner $ head left
    | otherwise = executioner $ head toExecute
    where
        toExecute = judgeJudy left (memoryTree memString)
        left = sort $ allLeftover hand

-- Judges which card to discard by determining whether a leftover's neighbours are still in the deck
-- If not more neighbours in the deck for a leftover, that is the leftover to be discarded
-- the rest of the list, with the target leftover at the head will be returned
judgeJudy :: [Leftover] -> BinTree Card -> [Leftover]
judgeJudy l tree 
    | length l == 0 = []
    | neighbourAlive tree (gatherNeighbours $ head l) = judgeJudy (drop 1 l) tree
    | otherwise = [head l]

-- Given a leftover, get its neighbours
gatherNeighbours :: Leftover -> [Card]
gatherNeighbours s@(StrPair _ _) = whoStraightWith s
gatherNeighbours s@(SetPair _ _) = whoSetWith s
gatherNeighbours s@(Single _) = whoStraightWith s ++ whoSetWith s

-- Given a list of cards, check if any of them is not in the binary tree
-- all in tree > no neighbours alive > returns False
neighbourAlive :: BinTree Card -> [Card] -> Bool
neighbourAlive = (not.).all.searchTree

-- Given a leftover, returns card to be killed / discarded
executioner :: Leftover -> Card
executioner (StrPair _ c) = c
executioner (SetPair _ c) = c
executioner (Single c) = c


-- | Identify neighbours for potential melds 

-- Given a leftover, returns the list of cards it can form sets with
-- except for StrPair which we dont want to form straights from
whoSetWith :: Leftover -> [Card]
whoSetWith (SetPair (Card s1 r ) (Card s2 _)) = lift (flip Card) [r] (filter (neitherSuit s1 s2) [Spade ..])
whoSetWith (Single (Card s r)) = lift (flip Card) [r] (filter (neitherSuit s s) [Spade ..])
whoSetWith _ = []

-- Compare whether a given suit is the same with two other suits
neitherSuit :: Suit -> Suit -> Suit -> Bool
neitherSuit s1 s2  compareSuit = compareSuit /= s1 && compareSuit /= s2

-- Given a leftover, returns the list of cards it can from straights with
-- except for SetPair which we dont want to form sets from
whoStraightWith :: Leftover -> [Card]
whoStraightWith (StrPair c1 c2) = safeFindRankNeighbour c1 pred ++ safeFindRankNeighbour c2 succ
whoStraightWith (Single c) = map (whoRankNeighbour c) (findWho (getRank c))
whoStraightWith _ = []

-- Determine which functions to use to find neighbour for forming straights using a Single
findWho :: Rank -> [(Rank -> Rank)]
findWho r 
    | r /= Ace && r /= King = [pred, succ]
    | r == Ace = [succ]
    | r == King = [pred]
    | otherwise = []

-- Given a card, find its predecessing / successing ranked card
whoRankNeighbour :: Card -> (Rank -> Rank) -> Card
whoRankNeighbour (Card a b) f = (Card a (f b)) 

-- Only attempts to find neighbouring ranked cards if it is safe to do so 
safeFindRankNeighbour :: Card -> (Rank -> Rank) -> [Card]
safeFindRankNeighbour x f 
    | shouldFind x = [whoRankNeighbour x f]
    | otherwise = []
        where
            shouldFind (Card _ r) = r /= Ace && r /= King


-- | Replace card and determine what to call

-- Replace the discarded card with the last picked card to get current hand after discarding
replaceCard :: [Card] -> Card -> Card -> [Card]
replaceCard hand discard picked = map (toReplace discard picked) hand
-- replaceCard = (map.).flip toReplace
    where
        toReplace target lastPicked x = if x == target then lastPicked else x

-- Returns what to call (according to strategy explained in report)
whatToCall :: [Card] -> String -> Act
whatToCall c s 
    | length (notInDeck (getCardSequence s)) <= 12 = Drop -- if it is the first round
    | callGin c = Gin
    | not (canCallKnock c) = Drop
    | deadwoodCount c <= 5 || getDeckHeight s <= 16 = Knock
    | otherwise = Drop

-- Check if there are 0 deadwoods to call Gin 
callGin :: [Card] -> Bool
callGin = (==0).length.nonMeld.undeadMelds

-- Check if deadwood points less than 10 to see if can call knock
canCallKnock :: [Card] -> Bool
canCallKnock = (<= 10).deadwoodCount

-- Given a list of cards, count the number of points of deadwoods
deadwoodCount :: [Card] -> Int
deadwoodCount = sum. map toPoints.nonMeld.undeadMelds

-- Given a card, returns the amount of points it is worth (from Rules.hs)
toPoints :: Card -> Int
toPoints (Card _ rank) | rank < Jack = fromEnum rank + 1
                       | otherwise = 10 


-- [   MAKE MELDS   ]  

-- What does make melds do?
-- Just make melds! First try to form straights, then sets and the rest are deadwood

makeMelds :: MeldFunc
makeMelds = const (const (meldInMaybe.melded.allMelds.undeadMelds))
 
-- Given a list of card, get all straights and sets (non Deadwood melds) to be formed in a Melder context
undeadMelds :: [Card] -> Melder
undeadMelds = processSet.processStraight 

-- Given a list of cards, get all straights that we want to form first
processStraight :: [Card] -> Melder
processStraight cards = foldr (getLeftover convertStraight) (Melder [] (Just [])) (getLongestStraight cards)

-- Called after processStraight, to form sets from the rest of 'unstraightened' cards
processSet :: Melder -> Melder
processSet m = foldr (getLeftover convertSet) (Melder [] (melded m)) (groupRank (nonMeld m))
       
-- Last to be called, after straights and sets are obtained, convert the rest into Deadwoods
-- and combine them into the melded compartment of the Melder context
allMelds :: Melder -> Melder
allMelds m = foldr (placeDeadwood) (Melder [] (melded m)) (nonMeld m)

-- Given a list of cards, returns a list of lists of the possible straight cards grouped together
-- in an optimal way
getLongestStraight :: [Card] -> [[Card]]
getLongestStraight = (breakConsecutive =<<).possibleStraight

-- Convert a card which is unable to form straight or set melds into Deadwood
-- and put them into melded compartment in Melder context
placeDeadwood :: Card -> Melder -> Melder
placeDeadwood m (Melder a b) = Melder a (lift (:) (convertDeadwood m) b)

-- Converts list of cards into melds using given function and 
-- put them into melds in Melder context
getLeftover :: ([Card] -> Maybe Meld) -> [Card] -> Melder -> Melder
getLeftover f a (Melder b c) 
    | length a < 3 = Melder (b++a) c
    | otherwise = Melder b (lift (:) (f a) c)

-- Breaks a sequence of consecutive cards into list of list
-- to get the optimal combination to form straights from
-- i.e. if can form straights with more than five cards, 
-- form straight meld the greatest five cards of the sequence
breakConsecutive :: [Card] -> [[Card]]
breakConsecutive cardList
    | length cardList >= 7 && length cardList < 10 = [take 4 cardList] ++ [drop 4 cardList]
    | length cardList == 6 = [take 3 cardList] ++ [drop 3 cardList]
    | otherwise = [cardList]

-- | Get straights  

-- Given a list of cards, returns a list of lists of cards grouped in consecutive sequences, 
-- which are possible to form straights from
possibleStraight :: [Card] -> [[Card]]
possibleStraight l = join $ getConsecutive.sort <$> (groupSuit l)

-- Given a list of sorted cards, returns list of lists of cards grouped in consecutive sequences
getConsecutive :: [Card] -> [[Card]]
getConsecutive l
    | length l == 0 = []
    | otherwise = let c = checkConsecutive l in [c] ++ getConsecutive (drop (length c) l)
        
-- Given a list of sorted cards, returns the longest consecutive sequence from the start
checkConsecutive :: [Card] -> [Card]
checkConsecutive cl
    | length cl == 0 = []
    | consecutive cl = cl
    | otherwise = checkConsecutive (take (length cl - 1) cl)
        where
            consecutive l = ((orderByRank . last) l - (orderByRank . head) l) == (length l - 1)

-- Groups a list of cards into list of lists, whereby each list contains cards of the same suit
groupSuit :: [Card] -> [[Card]]
groupSuit = (<$> [Spade ..]).filterSuit


-- | Get sets 

-- Obtain all the sets that can be formed from a list of cards
getSet :: [Card] -> [[Card]]
getSet = (filter ((<=)3.length)).groupRank

-- Groups a list of cards into list of lists, whereby each list contains cards of the same rank
groupRank :: [Card] -> [[Card]]
groupRank = (<$> [Ace ..]).filterRank


-- | Binary tree data type and its utility functions


-- To store cards memorized in a binary tree for binary search to optimize complexity
data BinTree a = Leaf | Node a (BinTree a) (BinTree a)
    deriving Show

-- adapted from Week 11 slides
instance Foldable BinTree where
    foldMap _ Leaf = mempty
    foldMap f (Node v l r) = mconcat [foldMap f l, f v, foldMap f r]

-- Given a list of cards, build a binary tree of cards
buildTree :: [Card] -> BinTree Card
buildTree = foldr (flip insertTree) Leaf

-- Insert an item into the binary tree 
insertTree :: Ord a => BinTree a -> a -> BinTree a
insertTree Leaf q = Node q Leaf Leaf
insertTree (Node k l r) q
    | q < k = Node k (insertTree l q) r
    | q > k = Node k l (insertTree r q)
    | otherwise = Node q l r     

-- Returns a binary tree as a string using inorder traversal (adapted from Week 11 slides)
fromBinTree :: Show a => BinTree a -> String
fromBinTree Leaf = []
fromBinTree (Node v l r) = fromBinTree l ++ (show v) ++ fromBinTree r

-- Returns True if the tree contains the query, False otherwise (adapted from Week 11 slides)
searchTree :: Ord a => BinTree a -> a -> Bool
searchTree Leaf _ = False
searchTree (Node k l r) q
    | q < k = searchTree l q
    | q > k = searchTree r q
    | otherwise = True 


-- | Parser functions 
-- memory is stored in the form (deckHeight)[CardSequence]
-- where deckHeight is an integer and an example of the card sequence is as such C01D00H02H10...

-- | from Week 11 Lab

-- Return a parser that continues producing a list of values from the given parser.
list :: Parser a -> Parser [a]
list x = list1 x ||| pure []

-- Return a parser that produces at least one value from the given parser
-- then continues producing a list of values from the given parser (to
-- ultimately produce a non-empty list).
list1 :: Parser a -> Parser [a]
list1 p = do
    p' <- p
    p'' <- list p
    pure (p':p'')

-- Return a parser that produces a character but fails if:
-- the input is empty or the character does not satisfy the given predicate.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
    i <- character
    if p i then pure i else (unexpectedCharParser i)

-- Return a parser that produces any character but fails if:
-- the input is empty or the produced character is equal to the given character.
isNot :: Char -> Parser Char
isNot = satisfy.(/=)

-- Return a parser that produces a character between '0' and '9' but fails if
-- the input is empty or the produced character is not a digit.
digit :: Parser Char
digit = satisfy isDigit

-- Returns a parser that applies the given parser, parses 0 or more spaces, 
-- then produces the result of the given parser.
tok :: Parser a -> Parser a
tok p = do
    x <- P $ parse p               
    pure x

-- Returns a parser that parses the given char followed by 0 or more spaces.
charTok :: Char -> Parser Char
charTok = tok.is

-- Returns a parser that applies the first parser, runs the third parser keeping the result,
-- then runs the second parser and produces the obtained result.
between :: Parser o -> Parser c -> Parser a -> Parser a
between p1 p2 p3 = do
  _ <- p1
  x <- p3
  _ <- p2
  return x

-- Return a parser that produces the given number of values off the given
-- parser, which fails if the given parser fails in the attempt to
-- produce the given number of values.
thisMany :: Int -> Parser a -> Parser [a]
thisMany = (sequenceParser.).replicate

-- Return a parser that applies the given parser in between the two given
-- characters.
betweenCharTok :: Char -> Char -> Parser a -> Parser a
betweenCharTok = (.charTok).between.charTok

-- Return a parser that sequences the given list of parsers by producing all
-- their results but fails on the first failing parser of the list.
sequenceParser :: [Parser a] -> Parser [a]
sequenceParser = sequence


-- | Not from Week 11 Lab

-- Return a parser that only produces characters 'S','C','H' and 'D' but fails if:
-- the input is empty or the produced character any other character.
isCardSuit :: Parser Char
isCardSuit = is 'S' ||| is 'C' ||| is 'H' ||| is 'D'

-- Return a parser that produces a list of digits
readDeckHeight :: Parser String
readDeckHeight = list digit

-- Returns a parser that produces the deckHeight stored in the beginning of 
-- the memory string in the form (deckHeight).....
betweenParenthesis :: Parser String
betweenParenthesis = betweenCharTok '(' ')' readDeckHeight

-- Return a parser uses parsers isCardSuit, digit and digit in order and produces 
-- their results
readIndividualCard :: Parser String
readIndividualCard = sequenceParser [isCardSuit, digit, digit]

-- Returns a parser that produces a list of individual cards
readCardSequence :: Parser [String] 
readCardSequence = list readIndividualCard

-- Returns a parser that produces a list of cards memorized in the memory string
betweenSqBrkt :: Parser [String]
betweenSqBrkt = betweenCharTok '[' ']' readCardSequence

-- Returns a parser that produces characters until it encounters the '[' character
-- which indicate the start of a card sequence in the memory string
skipToCardSeq :: Parser String
skipToCardSeq = list (isNot '[')

-- Applies a parser on the memory string and returns the result and rest of input 
-- maybe context
applyParser :: Parser a -> String -> Maybe (a, String)
applyParser p s = destructure (parse p s) 
    where
        destructure (Result a b) = Just (b,a)
        destructure (Error _) = Nothing

-- Parses string up to get the deck height, returns (MemoryData storing deck height, rest of input)
parseDeckHeight :: String -> (MemoryData, String)
parseDeckHeight s = let (res, rest) = stringInMaybeTuple (applyParser betweenParenthesis s) in
    (MemoryData (actualNumber res) [], rest)

-- Parses off till the last character and returns (result, rest of input)
parseTillLast :: String -> (String, String)
parseTillLast s = stringInMaybeTuple (applyParser (thisMany (length s - 1) (isNot ']')) s)

-- Retrieves actual deck height
getDeckHeight :: String -> Int
getDeckHeight s = let (memData, _) = parseDeckHeight s in
    deckHeight memData

-- skips all unnecessary characters and returns MemoryData with updated card sequence (no more input left)
getCardSequence :: String -> MemoryData
getCardSequence s = let (res, _) = stringListInMaybeTuple (applyParser betweenSqBrkt (skipUnwanted s)) in
    MemoryData 0 res
    where
        skipUnwanted c = getRestInput(stringInMaybeTuple $ applyParser skipToCardSeq c)
        getRestInput (_,b) = b


-- | Convenient functions  

-- Apply a binary function in the applicative context (from Week 9 lab)
lift :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
lift f a b = f <$> a <*> b

getRank :: Card -> Rank
getRank (Card _ cr) = cr

getSuit :: Card -> Suit
getSuit (Card cs _) = cs

-- | Filtering

-- Filter out cards of the desired property
filterStuff :: Eq a => (Card -> a) -> [Card] -> a -> [Card]
filterStuff f cardList comparedProperty = [c | c <- cardList, f c == comparedProperty] 

-- Filter out cards of the desired rank
filterRank :: [Card] -> Rank -> [Card]
filterRank = filterStuff getRank

-- Filter out cards of the desired suit
filterSuit :: [Card] -> Suit -> [Card]
filterSuit = filterStuff getSuit 

-- Get the integer representation of a rank
orderByRank :: Card -> Int
orderByRank = fromEnum.getRank

-- Given a string of digits, converts its equivalent integer
actualNumber :: String -> Int
actualNumber s = foldl ((+).(*10)) 0 (charToIntList s)
    where
        charToIntList = (<$>)digitToInt

-- Returns true if target found in list
linearSearch :: Eq a => [a] -> a -> Bool
linearSearch l target | length l == 0 = False
                      | head l == target = True
                      | otherwise = linearSearch (drop 1 l) target


-- | Removing maybe context

removeMaybeString :: Maybe String -> String
removeMaybeString (Just s) = s
removeMaybeString Nothing = ""

stringListInMaybeTuple :: Maybe ([String], String) -> ([String], String)
stringListInMaybeTuple Nothing = ([""],"")
stringListInMaybeTuple (Just a) = a

stringInMaybeTuple :: Maybe (String, String) -> (String, String)
stringInMaybeTuple Nothing = ("","")
stringInMaybeTuple (Just a) = a
    
charInMaybeTuple :: Maybe (Char, String) -> (Char, String)
charInMaybeTuple Nothing = (' ',"")
charInMaybeTuple (Just a) = a

meldInMaybe :: Maybe [Meld] -> [Meld]
meldInMaybe Nothing = []
meldInMaybe (Just l) = l


-- | Conversion cards into different data types

convertDeadwood :: Card -> Maybe Meld
convertDeadwood = Just . Deadwood 

convertStraight :: [Card] -> Maybe Meld
convertStraight (a:b:c:[]) = Just $ Straight3 a b c
convertStraight (a:b:c:d:[]) = Just $ Straight4 a b c d
convertStraight (a:b:c:d:e:[]) = Just $ Straight5 a b c d e
convertStraight _ = Nothing

convertSet :: [Card] -> Maybe Meld
convertSet (a:b:c:[]) = Just $ Set3 a b c 
convertSet (a:b:c:d:[]) = Just $ Set4 a b c d
convertSet _ = Nothing

-- Convert list of cards form into appropriate Leftover datatype
convertLeftover :: [Card] -> [Leftover]
convertLeftover (a:b:[])| getRank a == getRank b = (SetPair a b):[]
                        | otherwise = (StrPair a b):[]
convertLeftover a = (<$>)Single a


-- | Some instances derived for types, i.e. Card, Rank, Suit, Draw, Leftover

instance Show Card where
    show (Card a b) =  show a ++ show b

instance Show Suit where
  show Spade = "S"
  show Club = "C"
  show Diamond = "D"
  show Heart = "H"

instance Show Rank where
  show Ace = "00"
  show Two = "01"
  show Three = "02"
  show Four = "03"
  show Five = "04"
  show Six = "05"
  show Seven = "06"
  show Eight = "07"
  show Nine = "08"
  show Ten = "09"
  show Jack = "10"
  show Queen = "11"
  show King = "12"

instance Eq Draw where
    (==) Stock Stock = True
    (==) Discard Discard = True
    (==) _ _ = False

-- | To order the leftover cards in increasing priority (explained in report)
instance Ord Leftover where
    -- prioritize straight pairs with sevens and sixes
    compare (StrPair _ (Card _ Seven)) (StrPair _ _) = GT
    compare (StrPair (Card _ Seven) _ ) (StrPair _ _ ) = GT
    compare (StrPair _ _ ) (StrPair _ (Card _ Seven)) = LT
    compare (StrPair _ _ ) (StrPair (Card _ Seven) _ ) = LT
    compare (StrPair _ (Card _ Six)) (StrPair _ _) = GT
    compare (StrPair (Card _ Six) _) (StrPair _ _) = GT
    compare (StrPair _ _ ) (StrPair _ (Card _ Six)) = LT
    compare (StrPair _ _ ) (StrPair (Card _ Six) _ ) = LT

    -- otherwise prioritize smaller straight pairs
    compare (StrPair _ (Card _ aRank)) (StrPair _ (Card _ bRank)) | aRank > bRank = LT
                                                                  | otherwise = GT

    -- Strpair > SetPair > Single
    compare (StrPair _ _) (SetPair _ _) = GT
    compare (StrPair _ _) (Single _) = GT
    compare _ (StrPair _ _) = LT
    compare (SetPair _ _) (Single _) = GT
    compare (Single _) (SetPair _ _) = LT

    compare (Single (Card _ Seven)) (Single _ ) = GT
    compare (Single _ ) (Single (Card _ Seven)) = LT
    compare (Single (Card _ Six)) (Single _ ) = GT
    compare (Single _ ) (Single (Card _ Six)) = LT
    compare (Single (Card _ aRank)) (Single (Card _ bRank)) | aRank > bRank = LT
                                                            | otherwise = GT
    compare _ _ = EQ



