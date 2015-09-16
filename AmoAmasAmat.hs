-- trite, I know

import Control.Monad (forM_)
import Data.Latin.Internal
import Data.Latin.Whitaker
import Data.Maybe (fromMaybe)
import Text.Printf

amare :: Verb
amare = Verb (1,1) Transitive (Just "am") (Just "am") (Just "am") (Just "amav") (Just "amat")

main :: IO ()
main = do
  (conjugationRules, _) <- whitakerRulesToDLI <$> fmap parseWhitakerLine <$> lines <$> readFile "INFLECTS.LAT"
  forM_ [Indicative, Subjunctive] $ \mood ->
    forM_ [Active, Passive] $ \voice ->
      forM_ [Present, Imperfect, Future, Perfect, Pluperfect, Futureperfect] $ \tense -> do
        print tense
        mapM_ (printf "%16s" . fromMaybe "") (conjugate conjugationRules amare tense voice mood <$> [1,2,3] <*> pure Singular)
        putStrLn ""
        mapM_ (printf "%16s" . fromMaybe "") (conjugate conjugationRules amare tense voice mood <$> [1,2,3] <*> pure Plural)
        printf "\n\n"
