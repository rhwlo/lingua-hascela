import System.Environment (getArgs)
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe)
import LatinWords
import qualified WhitakerLatin as WL

main :: IO ()
main = do
    regularTable <- readRegularTable
    verbs <- fmap fromWhitakerVerb <$> WL.readVerbsFromFile
    [verb, moodS, voiceS, tenseS, personS, numberS] <- getArgs
    putStrLn $ fromMaybe "" $ conjugate regularTable (read moodS) (read voiceS) (read tenseS) (read personS) (read numberS) =<< select verbs verb
  where
    select :: [Verb] -> String -> Maybe Verb
    select verbList verbS = WL.maybeHead $ flip filter verbList $ \verb ->
      let
        (Regular _ _ presStem _ _) = verb
      in
        verbS == presStem ++ "o" || verbS == presStem ++ "or" || (verbS == pord 3 presStem && "re" `isSuffixOf` verbS)
