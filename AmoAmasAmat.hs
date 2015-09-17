-- trite, I know

import Control.Monad (forM_)
import Data.Latin
import Data.Maybe (fromMaybe)
import Text.Printf

amare :: Verb
amare = Verb (1,1) Transitive (Just "am") (Just "am") (Just "am") (Just "amav") (Just "amat")

main :: IO ()
main = withLatin "INFLECTS.LAT" "STEMLIST.GEN" $ \conjRules _ _ _->
  forM_ [Indicative, Subjunctive] $ \mood -> do
    print mood
    forM_ [Active, Passive] $ \voice -> do
      putStr " "
      print voice
      forM_ [Present, Imperfect, Future, Perfect, Pluperfect, Futureperfect] $ \tense -> do
        putStr "  "
        print tense
        mapM_ (printf "%16s" . fromMaybe "[ABSENT]") (conjugate conjRules amare tense voice mood <$> [1,2,3] <*> pure Singular)
        putStrLn ""
        mapM_ (printf "%16s" . fromMaybe "[ABSENT]") (conjugate conjRules amare tense voice mood <$> [1,2,3] <*> pure Plural)
        printf "\n\n"
