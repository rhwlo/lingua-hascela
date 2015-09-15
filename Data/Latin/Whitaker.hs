module Data.Latin.Whitaker
where

import Data.Latin.Internal (
  NounCase(..),
  NounGender(..),
  VerbalCategory(..),
  VerbalForm(..))

import Data.List (foldl', sortBy)
import Data.Ord (comparing)

import qualified Data.Latin.Internal as DLI

data WhitakerLine = WV { wvStem       :: String,
                         wvConj       :: (Int, Int),
                         wvCat        :: VerbalCategory,
                         wvForm       :: VerbalForm,
                         wvId         :: Int }
                  | WN { wnStem       :: String,
                         wnDecl       :: (Int, Int),
                         wnGend       :: NounGender,
                         wnKind       :: Char,
                         wnCase       :: NounCase,
                         wnIntactStem :: Bool,
                         wnId         :: Int }
                  | WUnparseable deriving (Show, Eq)

getWId :: WhitakerLine -> Int
getWId (WV _ _ _ _ wId) = wId
getWId (WN _ _ _ _ _ _ wId) = wId
getWId _ = 0

getWForm :: WhitakerLine -> VerbalForm
getWForm (WV _ _ _ wForm _) = wForm
getWForm _ = OtherVerbalForm

parseWhitakerLine :: String -> WhitakerLine
parseWhitakerLine line = case words line of
    [stem, "V", conjOneS, conjTwoS, vCatS, vFormS, idS] ->
      WV stem (read conjOneS, read conjTwoS) (readvCat vCatS) (readvForm vFormS) (read idS)
    [stem, "N", declOneS, declTwoS, nGenderS, [nKind], nCaseS, idS] ->
      let intactStem = nCaseS == "1" in
        WN stem (read declOneS, read declTwoS)
           (readnGender nGenderS) nKind (readnCase nCaseS)
           intactStem (read idS)
    _ ->
      WUnparseable

readvCat :: String -> VerbalCategory
readvCat "DEP"      = Deponent
readvCat "INTRANS"  = Intransitive
readvCat "TRANS"    = Transitive
readvCat _          = OtherVerbalCategory

readvForm :: String -> VerbalForm
readvForm "2"       = PresPassiveStem
readvForm "3"       = PastActiveStem
readvForm "4"       = PastPassiveStem
readvForm _         = PresActiveStem

readnGender :: String -> NounGender
readnGender "F"     = Feminine
readnGender "M"     = Masculine
readnGender _       = Neuter

readnCase :: String
          -> NounCase
readnCase "2"       = Genitive
readnCase _         = Nominative

whitakerLinesToDLI :: [WhitakerLine]            -- the Whitaker Lines to convert into Data.Latin
                   -> ([DLI.Noun],              -- the Whitaker Lines that converted successfully into Nouns
                       [DLI.Verb])              -- the Whitaker Lines that converted successfuly into Verbs
whitakerLinesToDLI fullWLines = (nouns, verbs)
  where
    (_, nouns, verbs) = foldl' convertLines ([],[],[]) $ sortBy (comparing getWId) fullWLines

convertLines :: ([WhitakerLine], [DLI.Noun], [DLI.Verb])
              -> WhitakerLine
              -> ([WhitakerLine], [DLI.Noun], [DLI.Verb])
convertLines (digerenda, nouns, verbs) wLine = let
    lastLine = maybeHead digerenda
  in
    if fmap getWId lastLine /= Just (getWId wLine)
      then
        case lastLine of
          Just (WV {}) ->
            ([wLine], nouns, makeVerb digerenda : verbs)
          Just (WN {}) -> case makeNoun digerenda of
            Just newNoun -> ([wLine], newNoun : nouns, verbs)
            Nothing -> ([wLine], nouns, verbs)
          _ ->
            ([wLine], nouns, verbs)
      else
        (wLine:digerenda, nouns, verbs)
  where
    makeNoun :: [WhitakerLine] -> Maybe DLI.Noun
    makeNoun wLines = let
        maybeNom = maybeHead $ filter ((Nominative ==) . wnCase) wLines
        maybeGen = maybeHead $ filter ((Genitive ==) . wnCase) wLines
      in
        maybeNom >>= \nom ->
          return $ DLI.Noun (wnDecl nom)
                            (wnStem nom)
                            (wnIntactStem nom)
                            (wnStem <$> maybeGen)
                            (wnGend nom)
    makeVerb :: [WhitakerLine] -> DLI.Verb
    makeVerb wLines = let
        maybePresActive = wvStem <$> maybeHead (filter ((PresActiveStem ==) . wvForm) wLines)
        maybeInfinitive = wvStem <$> maybeHead (filter ((InfinitiveStem ==) . wvForm) wLines)
        maybePresPassive = wvStem <$> maybeHead (filter ((PresPassiveStem ==) . wvForm) wLines)
        maybePastActive = wvStem <$> maybeHead (filter ((PastActiveStem ==) . wvForm) wLines)
        maybePastPassive = wvStem <$> maybeHead (filter ((PastPassiveStem ==) . wvForm) wLines)
        WV _ vConj vCat _ _ : _ = wLines
      in
        DLI.Verb vConj vCat maybePresActive maybeInfinitive maybePresPassive maybePastActive maybePastPassive

maybeHead :: [a] -> Maybe a
maybeHead []    = Nothing
maybeHead (x:_) = Just x
