module Data.Latin.Internal where

import Control.Applicative
import qualified Data.Map as DM
import Text.Printf

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
                | GenderX
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

-- error types
type FailedDeclension =  (Noun, NounCase, NounNumber, String)
type FailedConjugation = (Verb, VerbTense, VerbVoice, VerbMood, Int, NounNumber, String)

class Abbreviable a where
  abbreviate :: a -> String

instance Abbreviable VerbTense where
  abbreviate Present        = "PRES"
  abbreviate Imperfect      = "IMPF"
  abbreviate Future         = "FUT "
  abbreviate Perfect        = "PERF"
  abbreviate Futureperfect  = "FUTP"
  abbreviate Pluperfect     = "PLUP"

instance Abbreviable VerbMood where
  abbreviate Subjunctive    = "SUBJ"
  abbreviate Indicative     = "IND "
  abbreviate Imperative     = "IMP "
  abbreviate _              = " X  "

instance Abbreviable VerbVoice where
  abbreviate Active         = "ACT "
  abbreviate Passive        = "PASS"

instance Abbreviable NounNumber where
  abbreviate Singular       = "SING"
  abbreviate Plural         = "PLUR"

instance Abbreviable NounCase where
  abbreviate Ablative       = "ABL"
  abbreviate Accusative     = "ACC"
  abbreviate Dative         = "DAT"
  abbreviate Genitive       = "GEN"
  abbreviate Nominative     = "NOM"
  abbreviate Vocative       = "VOC"

instance Abbreviable NounGender where
  abbreviate Common         = "UTR"
  abbreviate Feminine       = "FEM"
  abbreviate Masculine      = "MSC"
  abbreviate Neuter         = "NEU"
  abbreviate GenderX        = " X "


instance Abbreviable Int where
  abbreviate                = show

(||:<) :: Maybe b -> a -> Either a b
Nothing ||:< f  = Left f
(Just x) ||:< _ = Right x

infixl 4 ||:<

recoverEither :: Either a b -> Either a b -> Either a b
(Left _) `recoverEither` r = r
l `recoverEither` _        = l

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
  Genitive    -> genStem noun <|> nounStemLookup noun Nominative
  _           -> Nothing

conjugate :: ConjugationRules                -- the conjugation rules to operate under
          -> Verb                            -- the verb to conjugate
          -> VerbTense
          -> VerbVoice
          -> VerbMood
          -> Int                             -- person (1..3)
          -> NounNumber
          -> Either FailedConjugation String -- either a conjugated verb or the reason it failed
conjugate rules verb tense voice mood person number =
      case vCat verb of
        Deponent ->
          conjugate rules (dedeponent verb) tense voice mood person number
        _ -> let
            failConjugation :: String -> FailedConjugation
            failConjugation = (,,,,,,) verb tense voice mood person number
            attachEnding :: (VerbalForm, String) -> Either FailedConjugation String
            attachEnding (vForm, ending) = (++ ending) <$> (verbStemLookup verb vForm
                                                            ||:< failConjugation ("Couldn't find " ++ show vForm))
            byConjugation :: (Int, Int) -> Either FailedConjugation (VerbalForm, String)
            byConjugation conj = DM.lookup (conj, tense, voice, mood, person, number) rules
                                 ||:< failConjugation ("Failed lookup: " ++ show conj)
          in
            (byConjugation (vConj verb) >>= attachEnding)
              `recoverEither` (byConjugation (fst (vConj verb), 0) >>= attachEnding)
              `recoverEither` (byConjugation (0, 0) >>= attachEnding)

decline :: DeclensionRules
        -> Noun
        -> NounCase
        -> NounNumber
        -> Either FailedDeclension String
decline rules noun nCase nNumber = if intactNom noun && nCase == Nominative && nNumber == Singular
  then
    Right (nomStem noun)
  else
    let
      failDeclension :: String -> FailedDeclension
      failDeclension = (,,,) noun nCase nNumber
      attachEnding :: (NounCase, String) -> Either FailedDeclension String
      attachEnding (nForm, ending) = (++ ending) <$> (nounStemLookup noun nForm
                                                      ||:< failDeclension ("Couldn't find " ++ show nForm))
      byDeclensionAndGender :: (Int, Int) -> NounGender -> Either FailedDeclension (NounCase, String)
      byDeclensionAndGender decl nGender = DM.lookup (decl, nCase, nNumber, nGender) rules
                                            ||:< failDeclension (uncurry (printf "Failed lookup: %s (%i, %i)" (show nGender)) decl)
    in
      (byDeclensionAndGender (declension noun) (gender noun) >>= attachEnding)
        `recoverEither` (byDeclensionAndGender (declension noun) Common >>= attachEnding)
        `recoverEither` (byDeclensionAndGender (declension noun) GenderX >>= attachEnding)
        `recoverEither` (byDeclensionAndGender (fst (declension noun), 0) (gender noun) >>= attachEnding)
        `recoverEither` (byDeclensionAndGender (fst (declension noun), 0) Common >>= attachEnding)
        `recoverEither` (byDeclensionAndGender (fst (declension noun), 0) GenderX >>= attachEnding)

dedeponent :: Verb
           -> Verb
dedeponent verb = case vCat verb of
  Deponent ->
    Verb (vConj verb)
         Intransitive
         (vPresPassivStem verb <|> vPresActiveStem verb)
         (vInfinitiveStem verb)
         (vPresActiveStem verb <|> vPresPassivStem verb)
         (vPerfActiveStem verb)
         (vPerfPassivStem verb)
  _ -> verb
