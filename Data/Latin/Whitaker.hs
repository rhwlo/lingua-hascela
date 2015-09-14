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
readvForm "1"       = PresActiveStem
readvForm "2"       = PresPassiveStem
readvForm "3"       = PastActiveStem
readvForm "4"       = PastPassiveStem
readvForm _         = OtherVerbalForm

readnGender :: String -> NounGender
readnGender "F"     = Feminine
readnGender "M"     = Masculine
readnGender _       = Neuter

readnCase :: String
          -> NounCase
readnCase "2"       = Genitive
readnCase _         = Nominative

whitakerLinesToDLI :: [WhitakerLine]            -- the Whitaker Lines to convert into Data.Latin
                   -> ([WhitakerLine],          -- the Whitaker Lines that failed conversion
                       [DLI.Noun],              -- the Whitaker Lines that converted successfully into Nouns
                       [DLI.Verb])              -- the Whitaker Lines that converted successfuly into Verbs
whitakerLinesToDLI = collectStragglers . foldl' convertLines ([],[],[])
  where
    collectStragglers :: ([WhitakerLine], [DLI.Noun], [DLI.Verb])
                      -> ([WhitakerLine], [DLI.Noun], [DLI.Verb])
    collectStragglers (stragglers, nouns, verbs) = foldl' aggressivelyConvertLines ([], nouns, verbs) stragglers
      where
        aggressivelyConvertLines :: ([WhitakerLine], [DLI.Noun], [DLI.Verb])
                                 -> WhitakerLine
                                 -> ([WhitakerLine], [DLI.Noun], [DLI.Verb])
        aggressivelyConvertLines (wLines, dliNouns, dliVerbs) wLine = case wLine of
          WN nNomStem nDecl nGend _ Nominative _ _ ->
            (wLines, DLI.Noun nDecl nNomStem False Nothing nGend : dliNouns, dliVerbs)
          _ -> (wLine:wLines, dliNouns, dliVerbs)
    convertLines :: ([WhitakerLine], [DLI.Noun], [DLI.Verb])
                 -> WhitakerLine
                 -> ([WhitakerLine], [DLI.Noun], [DLI.Verb])
    convertLines (wLines, dliNouns, dliVerbs) wLine = let
        searchForParts :: Int -> ([WhitakerLine], [WhitakerLine])
        searchForParts wIdent = break ((wIdent ==) . getWId) wLines
        sortByVerbPart :: [WhitakerLine] -> [WhitakerLine]
        sortByVerbPart = sortBy (comparing wvForm)
      in case wLine of
        WV _ _ vCat _ vId -> let
            (withoutMe, myOtherParts) = searchForParts vId
            myParts = sortByVerbPart (wLine:myOtherParts)
          in
            case (vCat, myParts) of
              (Intransitive, [WV psAStem conj _ PresActiveStem _, WV pfAStem _ _ PastActiveStem _ ]) ->
                (withoutMe, dliNouns, DLI.IntVerb conj psAStem pfAStem : dliVerbs)
              (Deponent, [WV psPStem conj _ PresPassiveStem _, WV pfPStem _ _ PastPassiveStem _ ]) ->
                (withoutMe, dliNouns, DLI.DepVerb conj psPStem pfPStem : dliVerbs)
              (Transitive, [WV psAStem conj _ PresActiveStem _,
                            WV pfAStem _ _ PastActiveStem _ ,
                            WV pfPStem _ _ PastPassiveStem _]) ->
                (withoutMe, dliNouns, DLI.TrsVerb conj psAStem pfAStem pfPStem : dliVerbs)
              _ -> (wLine:wLines, dliNouns, dliVerbs)
        WN nGenStem nDecl nGend _ Genitive _ nId -> case searchForParts nId of
          (withoutMe, [WN nNomStem _ _ _ Nominative nIStem _]) ->
            (withoutMe, DLI.Noun nDecl nNomStem nIStem (Just nGenStem) nGend : dliNouns, dliVerbs)
          _ -> (wLine:wLines, dliNouns, dliVerbs)
        WN nNomStem nDecl nGend _ Nominative nIStem nId -> case searchForParts nId of
          (withoutMe, [WN nGenStem _ _ _ Genitive _ _]) ->
            (withoutMe, DLI.Noun nDecl nNomStem nIStem (Just nGenStem) nGend : dliNouns, dliVerbs)
          _ -> (wLine:wLines, dliNouns, dliVerbs)
        _ -> (wLines, dliNouns, dliVerbs)
