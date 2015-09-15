module Data.Latin.Internal where

import qualified Data.Map as DM

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
              | Vocative deriving (Show, Eq, Read)

data NounGender = Common
                | Feminine
                | Masculine
                | Neuter deriving (Show, Eq, Read)

data NounNumber = Singular
                | Plural deriving (Show, Eq, Read, Ord)

data Verb = Verb (Int, Int)         -- the conjugation
                 VerbalCategory
                 (Maybe String)     -- present active stem
                 (Maybe String)     -- infinitive stem
                 (Maybe String)     -- present passive stem
                 (Maybe String)     -- perfect active stem
                 (Maybe String)     -- perfect passive stem
                 deriving (Show, Eq)

data VerbTense = Present
               | Imperfect
               | Future
               | Perfect
               | Pluperfect
               | Futureperfect deriving (Show, Eq, Ord, Read)

data VerbVoice = Active
               | Passive deriving (Show, Eq, Ord, Read)

data VerbMood = Indicative
              | Subjunctive deriving (Show, Eq, Ord, Read)

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
