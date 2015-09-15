module Data.Latin.Whitaker
where

import Data.Latin.Internal (
  NounCase(..),
  NounGender(..),
  VerbalCategory(..),
  VerbalForm(..))

import Data.List (foldl', partition, sortBy)
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
whitakerLinesToDLI = (nouns, verbs)
  where
    (_, nouns, verbs) = foldl' convertLines' ([],[],[]) . sortBy (comparing getWId)
    convertLines' :: ([WhitakerLine], [DLI.Noun], [DLI.Verb])
                  -> WhitakerLine
                  -> ([WhitakerLine], [DLI.Noun], [DLI.Verb])
    convertLines' (digerenda, nouns, verbs) wLine = let
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
              Nothing ->
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
            maybePresActive = maybeHead $ filter ((PresActiveStem ==) . wvForm) wLines
            maybeInfinitive = maybeHead $ filter ((InfinitiveStem ==) . wvForm) wLines
            maybePresPassive = maybeHead $ filter ((PresPassiveStem ==) . wvForm) wLines
            maybePastActive = maybeHead $ filter ((PastActiveStem ==) . wvForm) wLines
            maybePastPassive = maybeHead $ filter ((PastPassiveStem ==) . wvForm) wLines
            WV _ vConj vCat _ _ : _ = wLines
          in
            DLI.Verb vConj vCat maybePresActive maybeInfinitive maybePresPassive maybePastActive maybePastPassive

maybeHead :: [a] -> Maybe a
maybeHead []    = Nothing
maybeHead (x:_) = Just x

convertLines :: ([WhitakerLine], [DLI.Noun], [DLI.Verb])
             -> WhitakerLine
             -> ([WhitakerLine], [WhitakerLine], [DLI.Noun], [DLI.Verb])
convertLines (wNouns, wVerbs, dliNouns, dliVerbs) wLine = let
    searchForParts :: Int -> [WhitakerLine] -> ([WhitakerLine], [WhitakerLine])
    searchForParts wIdent = partition ((wIdent ==) . getWId)
    sortByVerbPart :: [WhitakerLine] -> [WhitakerLine]
    sortByVerbPart = sortBy (comparing getWForm)
  in case wLine of
    WV _ _ vCat _ vId -> let
        (myOtherParts, withoutMe) = searchForParts vId wVerbs
        -- we sort the parts with wLine included so that the order encountered doesn't matter
        myParts = sortByVerbPart (wLine:myOtherParts)
      in
        case (vCat, myParts) of
          -- if the verb is Intransitive, we only need the present active and past active stems
          (Intransitive, [WV psAStem conj _ PresActiveStem _, WV pfAStem _ _ PastActiveStem _ ]) ->
            (wNouns, withoutMe, dliNouns, DLI.IntVerb conj psAStem pfAStem : dliVerbs)
          (Intransitive, [WV psAStem conj _ PresActiveStem _,
                        WV pfAStem _ _ PastActiveStem _ ,
                        WV _ _ _ PastPassiveStem _]) ->
            (wNouns, withoutMe, dliNouns, DLI.IntVerb conj psAStem pfAStem : dliVerbs)
          -- if the verb is Deponent, we only need the present passive and past passive stems
          (Deponent, [WV psPStem conj _ PresPassiveStem _, WV pfPStem _ _ PastPassiveStem _ ]) ->
            (wNouns, withoutMe, dliNouns, DLI.DepVerb conj psPStem pfPStem : dliVerbs)
          (Deponent, [WV psAStem conj _ PresActiveStem _, WV pfPStem _ _ PastPassiveStem _ ]) ->
            (wNouns, withoutMe, dliNouns, DLI.DepVerb conj psAStem pfPStem : dliVerbs)
          -- if the verb is Transitive, we need present active and past active + passive stems
          (Transitive, [WV psAStem conj _ PresActiveStem _,
                        WV pfAStem _ _ PastActiveStem _ ,
                        WV pfPStem _ _ PastPassiveStem _]) ->
            (wNouns, withoutMe, dliNouns, DLI.TrsVerb conj psAStem psAStem pfAStem pfPStem : dliVerbs)
          (Transitive, [WV psAStem conj _ PresActiveStem _,
                        WV psPStem _ _ PresPassiveStem _,
                        WV pfAStem _ _ PastActiveStem _ ,
                        WV pfPStem _ _ PastPassiveStem _]) ->
            (wNouns, withoutMe, dliNouns, DLI.TrsVerb conj psAStem psPStem pfAStem pfPStem : dliVerbs)
          (OtherVerbalCategory, _) -> (wNouns, withoutMe, dliNouns, dliVerbs)
          _ -> (wNouns, wLine:wVerbs, dliNouns, dliVerbs)
    WN nGenStem nDecl nGend _ Genitive _ nId -> case searchForParts nId wNouns of
      -- if our line is nominative, look for a genitive line
      ([WN nNomStem _ _ _ Nominative nIStem _], withoutMe) ->
        (withoutMe, wVerbs, DLI.Noun nDecl nNomStem nIStem (Just nGenStem) nGend : dliNouns, dliVerbs)
      _ -> (wLine:wNouns, wVerbs, dliNouns, dliVerbs)
    WN nNomStem nDecl nGend _ Nominative nIStem nId -> case searchForParts nId wNouns of
      -- if our line is genitive, look for a nominative line
      ([WN nGenStem _ _ _ Genitive _ _], withoutMe) ->
        (withoutMe, wVerbs, DLI.Noun nDecl nNomStem nIStem (Just nGenStem) nGend : dliNouns, dliVerbs)
      _ -> (wLine:wNouns, wVerbs, dliNouns, dliVerbs)
    _ -> (wNouns, wVerbs, dliNouns, dliVerbs)
