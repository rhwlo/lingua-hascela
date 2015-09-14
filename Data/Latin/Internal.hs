module Data.Latin.Internal where

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

data NounGender = Feminine
                | Masculine
                | Neuter deriving (Show, Eq, Read)

data Verb = DepVerb (Int, Int)      -- the conjugation
                    String          -- the present stem
                    String          -- the perfect stem
          | IntVerb (Int, Int)      -- the conjugation
                    String          -- the present stem
                    String          -- the perfect stem
          | TrsVerb (Int, Int)      -- the conjugation
                    String          -- the present stem
                    String          -- the perfect stem
                    String          -- the perfect passive stem


data VerbalCategory = Deponent
                    | Intransitive
                    | Transitive
                    | OtherVerbalCategory deriving (Eq, Ord, Show)

data VerbalForm = PresActiveStem
                | PresPassiveStem
                | PastActiveStem
                | PastPassiveStem
                | OtherVerbalForm deriving (Eq, Ord, Show)
