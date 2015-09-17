module Data.Latin (
  Noun(..),
  NounCase(..),
  NounNumber(..),
  Verb(..),
  VerbMood(..),
  VerbTense(..),
  VerbVoice(..),
  VerbalCategory(..),
  abbreviate,
  conjugate,
  decline,
  readWhitakerRules,
  readWhitakerStems,
  withLatin)
where

import Data.Latin.Internal hiding (conjugate, decline)
import qualified Data.Latin.Internal as DLI (conjugate, decline)
import Data.Latin.Whitaker
import qualified Data.Map as DM
import Data.Maybe (catMaybes)

conjugate :: ConjugationRules -> Verb -> VerbTense -> VerbVoice -> VerbMood -> Int -> NounNumber -> Maybe String
conjugate rules verb tense voice mood person number = discardLeft (DLI.conjugate rules verb tense voice mood person number)

decline :: DeclensionRules -> Noun -> NounCase -> NounNumber -> Maybe String
decline rules noun casus number = discardLeft (DLI.decline rules noun casus number)

discardLeft :: Either a b -> Maybe b
discardLeft (Right x) = Just x
discardLeft _         = Nothing

readWhitakerRules :: String
                  -> IO (ConjugationRules, DeclensionRules)
readWhitakerRules fileName = whitakerRulesToDLI <$> fmap parseWhitakerLine . lines <$> readFile fileName

readWhitakerStems :: String
                  -> IO ([Noun], [Verb])
readWhitakerStems fileName = whitakerLinesToDLI <$> fmap parseWhitakerLine . lines <$> readFile fileName

withLatin :: String       -- rules file
          -> String       -- stem file
          -> (ConjugationRules -> DeclensionRules -> DM.Map String Noun -> DM.Map String Verb -> IO a)
          -> IO a
withLatin rulesFile stemFile f = do
    (conjRules, declRules) <- readWhitakerRules rulesFile
    (nounStems, verbStems) <- readWhitakerStems stemFile
    f conjRules declRules (makeNounMap declRules nounStems) (makeVerbMap conjRules verbStems)
  where
    makeNounMap :: DeclensionRules -> [Noun] -> DM.Map String Noun
    makeNounMap declRules nouns = DM.fromList $ catMaybes $ flip fmap nouns $ \noun ->
      (\x -> (x, noun)) <$> decline declRules noun Nominative Singular
    makeVerbMap :: ConjugationRules -> [Verb] -> DM.Map String Verb
    makeVerbMap conjRules verbs = DM.fromList $ catMaybes $ flip fmap verbs $ \verb ->
      (\x -> (x, verb)) <$> conjugate conjRules verb Present Active Indicative 1 Singular
