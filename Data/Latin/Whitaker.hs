module Data.Latin.Whitaker
where

import Data.Latin.Internal (
  NounCase(..),
  NounGender(..),
  NounNumber(..),
  VerbMood(..),
  VerbTense(..),
  VerbVoice(..),
  VerbalCategory(..),
  VerbalForm(..))

import Data.List (foldl', sortBy)
import qualified Data.Map as DM
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
                  | WNR { wnrDecl     :: (Int, Int),
                          wnrCase     :: NounCase,
                          wnrNumber   :: NounNumber,
                          wnrGender   :: NounGender,
                          wnrStemType :: NounCase,
                          wnrEnding   :: String }
                  | WVR { wvrConj     :: (Int, Int),
                          wvrTense    :: VerbTense,
                          wvrVoice    :: VerbVoice,
                          wvrMood     :: VerbMood,
                          wvrPerson   :: Int,
                          wvrNumber   :: NounNumber,
                          wvrStemType :: VerbalForm,
                          wvrEnding   :: String }
                  | WUnparseable deriving (Show, Eq)

getWId :: WhitakerLine -> Int
getWId (WV _ _ _ _ wId) = wId
getWId (WN _ _ _ _ _ _ wId) = wId
getWId _ = 0

getWForm :: WhitakerLine -> VerbalForm
getWForm (WV _ _ _ wForm _) = wForm
getWForm _ = OtherVerbalForm

readvCat :: String -> VerbalCategory
readvCat "DEP"      = Deponent
readvCat "INTRANS"  = Intransitive
readvCat "TRANS"    = Transitive
readvCat _          = OtherVerbalCategory

readvForm :: String -> VerbalForm
readvForm "3"       = PastActiveStem
readvForm "4"       = PastPassiveStem
readvForm _         = PresActiveStem

readnGender :: String -> NounGender
readnGender "F"     = Feminine
readnGender "M"     = Masculine
readnGender "N"     = Neuter
readnGender "X"     = GenderX
readnGender _       = Common

readnCase :: String
          -> NounCase
readnCase "ABL"     = Ablative
readnCase "ACC"     = Accusative
readnCase "DAT"     = Dative
readnCase "GEN"     = Genitive
readnCase "VOC"     = Vocative
readnCase "2"       = Genitive
readnCase _         = Nominative

readnNumber :: String
            -> NounNumber
readnNumber "S"     = Singular
readnNumber _       = Plural

readvMood :: String
          -> VerbMood
readvMood "SUB" = Subjunctive
readvMood "IND" = Indicative
readvMood "IMP" = Imperative
readvMood _     = UnknownMood

readvTense :: String
           -> VerbTense
readvTense "IMPF" = Imperfect
readvTense "FUT"  = Future
readvTense "PERF" = Perfect
readvTense "PLUP" = Pluperfect
readvTense "FUTP" = Futureperfect
readvTense _      = Present

readvVoice :: String
           -> VerbVoice
readvVoice "PASSIVE" = Passive
readvVoice _         = Active


parseWhitakerLine :: String -> WhitakerLine
parseWhitakerLine line = case words line of
    [stem, "V", conjOneS, conjTwoS, vCatS, vFormS, idS] ->
      WV stem (read conjOneS, read conjTwoS) (readvCat vCatS) (readvForm vFormS) (read idS)
    [stem, "N", declOneS, declTwoS, nGenderS, [nKind], nCaseS, idS] ->
      let intactStem = nCaseS == "1" in
        WN stem (read declOneS, read declTwoS)
           (readnGender nGenderS) nKind (readnCase nCaseS)
           intactStem (read idS)
    "N":rest -> case take 10 rest of
      [declOneS, declTwoS, rCaseS, rNumberS, rGenderS, rStemTypeS, _, rEndingS, _, _] ->
        WNR (read declOneS, read declTwoS)
            (readnCase rCaseS)
            (readnNumber rNumberS)
            (readnGender rGenderS)
            (readnCase rStemTypeS)
            rEndingS
      [declOneS, declTwoS, rCaseS, rNumberS, rGenderS, rStemTypeS, _, _, _] ->
        WNR (read declOneS, read declTwoS)
            (readnCase rCaseS)
            (readnNumber rNumberS)
            (readnGender rGenderS)
            (readnCase rStemTypeS)
            ""
      _ -> WUnparseable
    "V":(conjOneS:(conjTwoS:(rTenseS:(rVoiceS:(rMoodS:(rPersonS:(rNumberS:(rStemTypeS:(_:(rEnding:_)))))))))) ->
      WVR (read conjOneS, read conjTwoS)
          (readvTense rTenseS)
          (readvVoice rVoiceS)
          (readvMood rMoodS)
          (read rPersonS)
          (readnNumber rNumberS)
          (readvForm rStemTypeS)
          rEnding
    _ ->
      WUnparseable

whitakerRulesToDLI :: [WhitakerLine]
                   -> (DLI.ConjugationRules,
                       DLI.DeclensionRules)
whitakerRulesToDLI = foldl' convertRulesToDLI (mempty, mempty)
  where
    convertRulesToDLI :: (DLI.ConjugationRules, DLI.DeclensionRules)
                      -> WhitakerLine
                      -> (DLI.ConjugationRules, DLI.DeclensionRules)
    convertRulesToDLI (dliConjRules, dliDeclRules) wLine = case wLine of
      WVR conj tense voice mood person number stemType ending ->
        (DM.insert (conj, tense, voice, mood, person, number) (stemType, ending) dliConjRules, dliDeclRules)
      WNR decl casus number gender stemType ending ->
        (dliConjRules, DM.insert (decl, casus, number, gender) (stemType, ending) dliDeclRules)
      _ ->
        (dliConjRules, dliDeclRules)

whitakerLinesToDLI :: [WhitakerLine]            -- the Whitaker Lines to convert into Data.Latin
                   -> ([DLI.Noun],              -- the Whitaker Lines that converted successfully into Nouns
                       [DLI.Verb])              -- the Whitaker Lines that converted successfuly into Verbs
whitakerLinesToDLI fullWLines = (dliNouns, dliVerbs)
  where
    (_, dliNouns, dliVerbs) = foldl' convertLines ([],[],[]) $ sortBy (comparing getWId) fullWLines
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
