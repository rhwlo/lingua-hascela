module LatinWords where

import Data.Char (isSpace)
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (fromMaybe)
import Control.Monad (join)
import qualified Data.Map as DM

data Verb = Regular Conjugation String String String (Maybe String)
            | Irregular String String String (Maybe String) IrregularTable deriving (Show, Eq)
data Noun = NounDeclension NounGender String String deriving (Show, Eq, Read)
data Adjective = AdjDeclension String String deriving (Show, Eq, Read)
type Conjugation = Int
type NounDeclension = Int
data NounGender = Masculine | Feminine | Neuter deriving (Show, Eq, Read)
data Mood = Imperative | Indicative | Subjunctive deriving (Show, Read, Eq, Ord)
data Voice = Active | Passive deriving (Show, Read, Eq, Ord)
data Tense = Future | Futureperfect | Imperfect | Pluperfect | Present | Perfect deriving (Show, Read, Eq, Ord)
type GrammaticalPerson = Int
data GrammaticalNumber = Singular | Plural deriving (Show, Read, Eq, Ord)

type ConjugationTable = DM.Map (Conjugation, Mood, Voice, Tense, GrammaticalPerson, GrammaticalNumber) String
type ConjugationTuple = ((Conjugation, Mood, Voice, Tense, GrammaticalPerson, GrammaticalNumber), String)
type IrregularTable = DM.Map (Mood, Voice, Tense, GrammaticalPerson, GrammaticalNumber) String
type IrregularConjugationTuple = ((Mood, Voice, Tense, GrammaticalPerson, GrammaticalNumber), String)

readRegularTable :: IO ConjugationTable
readRegularTable = do
    regularTableLines <- (filter (not . isPrefixOf "--") . lines) <$> readFile "RegularTable.txt"
    return $ DM.fromList $ join $ fromConjugationTable regularTableLines <$> conjugations <*> moods <*> voices <*> tenses
  where
    fromConjugationTable :: [String] -> Conjugation -> Mood -> Voice -> Tense -> [ConjugationTuple]
    fromConjugationTable corpus c m v t = let
        tenseSection :: [String]
        tenseSection = splitSection t $ splitSection v $ splitSection m $ splitSection c corpus
        endings :: [String]
        endings = map read $ words (fromMaybe "" $ maybeLast tenseSection)
        headings = (\p n -> (c, m, v, t, p, n)) <$> persons <*> numbers
        persons = [1, 2, 3]
        numbers = [Singular, Plural]
      in
        zip headings endings

readIrregularTable :: IO [Verb]
readIrregularTable = do
    irregularTableLines <- (filter (not . isPrefixOf "--") . lines) <$> readFile "IrregularTable.txt"
    return (verbsFrom irregularTableLines)
  where
    verbsFrom :: [String] -> [Verb]
    verbsFrom corpus = flip fmap (findVerbs corpus) $ \(firstPerson, infinitive, perfect, maybePassive) ->
      Irregular firstPerson infinitive perfect maybePassive (getConjugations corpus firstPerson)
    findVerbs :: [String] -> [(String, String, String, Maybe String)]
    findVerbs corpus = flip fmap (filter (not . isSpace . head) corpus) $ \line -> let
        readLine :: [String]
        readLine = map read (words line)
        (firstPerson:(infinitive:(perfect:theRest))) = readLine
        maybePassive = maybeLast theRest
      in (firstPerson, infinitive, perfect, maybePassive)
    getConjugations :: [String] -> String -> IrregularTable
    getConjugations corpus firstPerson = let
        startOfSection :: [String]
        startOfSection = dropWhile (not . isPrefixOf (show firstPerson)) corpus
        section :: [String]
        section = takeWhile (isSpace . head) (fromMaybe [] $ maybeTail startOfSection)
      in DM.fromList $ join $ fromConjugationTable section <$> moods <*> voices <*> tenses
    fromConjugationTable :: [String] -> Mood -> Voice -> Tense -> [IrregularConjugationTuple]
    fromConjugationTable corpus m v t = let
        tenseSection :: [String]
        tenseSection = splitSection t $ splitSection v $ splitSection m corpus
        endings :: [String]
        endings = map read $ words (fromMaybe "" $ maybeLast tenseSection)
        headings = (\p n -> (m, v, t, p, n)) <$> persons <*> numbers
        persons = [1, 2, 3]
        numbers = [Singular, Plural]
      in
        zip headings endings

conjugations :: [Conjugation]
conjugations = [1, 2, 3, 4]

moods :: [Mood]
moods = [Indicative, Subjunctive]

voices :: [Voice]
voices = [Active, Passive]

tenses :: [Tense]
tenses = [Future, Futureperfect, Imperfect, Pluperfect, Present, Perfect]

splitSection :: Show a => a -> [String] -> [String]
splitSection _ [] = []
splitSection keyWord fileLines = let
    startOfSection :: [String]
    startOfSection = dropWhile (not . isInfixOf (show keyWord)) fileLines
    indentFor :: String -> String
    indentFor = takeWhile isSpace
    sectionIndent :: String
    sectionIndent = indentFor (head startOfSection)
    section :: [String]
    section = takeWhile ((/= sectionIndent) . indentFor) (fromMaybe [] $ maybeTail startOfSection)
  in section

maybeTail :: [a] -> Maybe [a]
maybeTail [] = Nothing
maybeTail xs = Just (tail xs)

maybeLast :: [a] -> Maybe a
maybeLast [] = Nothing
maybeLast xs = Just (last xs)

pord :: Int -> [a] -> [a]
pord n = reverse . drop n . reverse

isPerfective :: Tense -> Bool
isPerfective = flip elem [Futureperfect, Perfect, Pluperfect]

conjugate :: ConjugationTable -> Mood -> Voice -> Tense -> GrammaticalPerson -> GrammaticalNumber -> Verb -> Maybe String
conjugate _ _ Passive _ _ _ (Regular _ _ _ _ Nothing)       = Nothing
conjugate _ _ Passive _ _ _ (Irregular _ _ _ Nothing _ )    = Nothing
conjugate _ Subjunctive _ Future _ _ _                      = Nothing
conjugate _ Subjunctive _ Futureperfect _ _ _               = Nothing
conjugate table mood voice tense person number verb         = case verb of
  (Regular conjugation _ infinitive perfect perfectPassiveParticiple) ->
    case (voice, isPerfective tense) of
      (Passive, True)     -> perfectPassiveParticiple
      (Active, True)      -> Just (perfectStem ++ ending)
      (_, False)          -> Just (stem ++ ending)
    where
      stem = pord 3 infinitive
      perfectStem = init perfect
      ending = table DM.! (conjugation, mood, voice, tense, person, number)
  (Irregular _ _ _ _ conjugationTable) ->
    if (mood, voice, tense, person, number) `elem` DM.keys conjugationTable
      then Just $ conjugationTable DM.! (mood, voice, tense, person, number)
      else Nothing
