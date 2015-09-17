import System.Random (getStdRandom, randomR)
import Data.Latin
import Data.Maybe (fromMaybe)
import qualified Data.Map as DM
import Text.Printf

main :: IO ()
main = withLatin "INFLECTS.LAT" "STEMLIST.GEN" $ \conjRules _ _ verbMap -> do
      (verbName, verb) <- randomFromList (DM.toList verbMap)
      (tense, voice, mood, person, number) <- randomFromList $ (,,,,) <$> tenses <*> voices <*> moods <*> persons <*> numbers
      printf "%s: %s %s %s %s %s\n" verbName (abbreviate tense) (abbreviate voice) (abbreviate mood) (abbreviate person) (abbreviate number)
      putStrLn $ fromMaybe "" $ conjugate conjRules verb tense voice mood person number
  where
    randomFromList :: [a] -> IO a
    randomFromList xs = (return . (xs !!)) =<< getStdRandom (randomR (0, length xs - 1))
    tenses = [Present, Imperfect, Future, Perfect, Pluperfect, Futureperfect]
    voices = [Active, Passive]
    moods = [Subjunctive, Indicative]
    persons = [1, 2, 3]
    numbers = [Singular, Plural]
