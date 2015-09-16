module Data.Latin.Internal where

import qualified Data.Map as DM
import Control.Monad (mplus)

type ConjugationRules = DM.Map ConjugationTuple (VerbalForm, String)

type ConjugationTuple = ((Int, Int), VerbTense, VerbVoice, VerbMood, Int, NounNumber)

type DeclensionRules  = DM.Map DeclensionTuple  (NounCase, String)

type DeclensionTuple  = ((Int, Int), NounCase, NounNumber, NounGender)

data Noun = Noun { declension   :: (Int, Int),
                   nomStem      :: String,              -- the Nominative stem
                   intactNom    :: Bool,                -- whether the Nominative stem needs an ending
                   genStem      :: Maybe String,        -- the Genitive stem (if present)
                   gender       :: NounGender           -- the gender of the noun
                   } deriving (Show, Eq)

data NounCase = Ablative
              | Accusative
              | Dative
              | Genitive
              | Nominative
              | Vocative deriving (Show, Eq, Ord, Read)

data NounGender = Common
                | Feminine
                | Masculine
                | Neuter deriving (Show, Eq, Ord, Read)

data NounNumber = Singular
                | Plural deriving (Show, Eq, Read, Ord)

data Verb = Verb { vConj           :: (Int, Int),        -- the conjugation
                   vCat            :: VerbalCategory,
                   vPresActiveStem :: Maybe String,    -- present active stem
                   vInfinitiveStem :: Maybe String,    -- infinitive stem
                   vPresPassivStem :: Maybe String,    -- present passive stem
                   vPerfActiveStem :: Maybe String,    -- perfect active stem
                   vPerfPassivStem :: Maybe String }   -- perfect passive stem
                 deriving (Show, Eq)

data VerbTense = Present
               | Imperfect
               | Future
               | Perfect
               | Pluperfect
               | Futureperfect deriving (Show, Eq, Ord, Read)

data VerbVoice = Active
               | Passive deriving (Show, Eq, Ord, Read)

data VerbMood = Imperative
              | Indicative
              | Subjunctive
              | UnknownMood deriving (Show, Eq, Ord, Read)

data VerbalCategory = Deponent
                    | Intransitive
                    | Transitive
                    | OtherVerbalCategory deriving (Eq, Ord, Show)

data VerbalForm = PresActiveStem
                | InfinitiveStem
                | PresPassiveStem
                | PastActiveStem
                | PastPassiveStem
                | OtherVerbalForm deriving (Eq, Ord, Show)

verbStemLookup :: Verb -> VerbalForm -> Maybe String
verbStemLookup verb vForm = case vForm of
  PresActiveStem  -> vPresActiveStem verb
  InfinitiveStem  -> vInfinitiveStem verb
  PresPassiveStem -> vPresPassivStem verb
  PastActiveStem  -> vPerfActiveStem verb
  PastPassiveStem -> vPerfPassivStem verb
  OtherVerbalForm -> Nothing

nounStemLookup :: Noun -> NounCase -> Maybe String
nounStemLookup noun nForm = case nForm of
  Nominative  -> Just (nomStem noun)
  Genitive    -> genStem noun
  _           -> Nothing

conjugate :: ConjugationRules       -- the conjugation rules to operate under
          -> Verb                   -- the verb to conjugate
          -> VerbTense
          -> VerbVoice
          -> VerbMood
          -> Int                    -- person (1..3)
          -> NounNumber
          -> Maybe String           -- perhaps a conjugated verb
conjugate rules verb tense voice mood person number =
      case vCat verb of
        Deponent ->
          conjugate rules (dedeponent verb) tense voice mood person number
        _ -> let
            attachEnding :: (VerbalForm, String) -> Maybe String
            attachEnding (vForm, ending) = (++ ending) <$> verbStemLookup verb vForm
            byConjugation :: (Int, Int) -> Maybe (VerbalForm, String)
            byConjugation conj = DM.lookup (conj, tense, voice, mood, person, number) rules
          in
            (byConjugation (vConj verb) >>= attachEnding)
              `mplus` (byConjugation (fst (vConj verb), 0) >>= attachEnding)
              `mplus` (byConjugation (0, 0) >>= attachEnding)

decline :: DeclensionRules
        -> Noun
        -> NounCase
        -> NounNumber
        -> Maybe String
decline rules noun nCase nNumber = if intactNom noun && nCase == Nominative && nNumber == Singular
  then
    Just (nomStem noun)
  else
    let
      attachEnding :: (NounCase, String) -> Maybe String
      attachEnding (nForm, ending) = (++ ending) <$> nounStemLookup noun nForm
      byDeclensionAndGender :: (Int, Int) -> NounGender -> Maybe (NounCase, String)
      byDeclensionAndGender decl nGender = DM.lookup (decl, nCase, nNumber, nGender) rules
    in
      (byDeclensionAndGender (declension noun) (gender noun) >>= attachEnding)
        `mplus` (byDeclensionAndGender (declension noun) Common >>= attachEnding)
        `mplus` (byDeclensionAndGender (fst (declension noun), 0) (gender noun) >>= attachEnding)
        `mplus` (byDeclensionAndGender (fst (declension noun), 0) Common >>= attachEnding)

dedeponent :: Verb
           -> Verb
dedeponent verb = case vCat verb of
  Deponent ->
    Verb (vConj verb)
         Intransitive
         (vPresPassivStem verb)
         (vInfinitiveStem verb)
         (vPresActiveStem verb)
         (vPerfPassivStem verb)
         (vPerfActiveStem verb)
  _ -> verb
