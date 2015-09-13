module WhitakerLatin where

import qualified Data.Set as DS
import Data.List (isInfixOf)
import Data.Maybe (catMaybes)

data WhitakerVerb   = WV { wvStem :: String,
                           wvConj :: Int,
                           wvCat :: VerbalCategory,
                           wvForm :: VerbalForm,
                           wvIdent :: Integer } deriving (Show, Eq)

readWV :: String -> WhitakerVerb
readWV wvString = let
      [stem, "V", cjString, _, ctString, vfString, viString] = words wvString
      vCat = readVC ctString
      vConj = read cjString :: Int
      vIdent = read viString :: Integer
      vForm = readVF vfString
    in WV stem vConj vCat vForm vIdent

data VerbalCategory = Transitive
                    | Intransitive
                    | Deponent deriving (Show, Eq, Ord)

readVC :: String -> VerbalCategory
readVC "INTRANS" = Intransitive
readVC "DEP"     = Deponent
readVC _         = Transitive

data VerbalForm     = PresentStem
                    | PresentPassiveStem
                    | PerfectStem
                    | PerfectPassiveStem deriving (Show, Eq, Ord)

readVF :: String -> VerbalForm
readVF "2" = PresentPassiveStem
readVF "3" = PerfectStem
readVF "4" = PerfectPassiveStem
readVF _   = PresentStem

data Verb           = Regular Int              -- conjugation class
                              VerbalCategory   -- transitive / intransitive / deponent
                              String           -- the present stem
                              String           -- the perfect stem
                              (Maybe String)   -- the perfect passive stem
                              deriving (Show, Eq)

convertWVtoVerb :: [WhitakerVerb] -> Maybe Verb
convertWVtoVerb wvParts = let
    WV _ conj vcat _ _ : _ = wvParts
    makeVerb :: Maybe String -> Maybe String -> Maybe String -> Maybe Verb
    makeVerb mPStem mPfStem mPPfStem = do
      pStem <- mPStem
      pfStem <- mPfStem
      return (Regular conj vcat pStem pfStem mPPfStem)
    selectVPart :: VerbalForm -> Maybe String
    selectVPart vPart = wvStem <$> maybeHead (filter ((vPart ==) . wvForm) wvParts)
    presStem = selectVPart PresentStem
    perfActStem = selectVPart PerfectStem
    perfPassStem = selectVPart PerfectPassiveStem
    presPassStem = selectVPart PresentPassiveStem
  in
    case vcat of
      Transitive        -> makeVerb presStem perfActStem perfPassStem
      Intransitive      -> makeVerb presStem perfActStem Nothing
      Deponent          -> makeVerb presPassStem perfPassStem Nothing

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead xs = Just (head xs)

convertWVsToVerbs :: [WhitakerVerb] -> [Verb]
convertWVsToVerbs wvList = let
    verbNumbers :: DS.Set Integer
    verbNumbers = DS.fromList $ fmap wvIdent wvList
    getVerbForms :: Integer -> [WhitakerVerb]
    getVerbForms vIdent = filter ((vIdent ==) . wvIdent) wvList
  in
    catMaybes (convertWVtoVerb <$> getVerbForms <$> DS.toList verbNumbers)

readVerbsFromFile :: IO [Verb]
readVerbsFromFile = do
    wvLines <- (sanitize . lines) <$> readFile "STEMLIST.GEN"
    return (convertWVsToVerbs (readWV <$> wvLines))
  where
    sanitize :: [String] -> [String]
    sanitize = filter lineWillParse
    lineWillParse :: String -> Bool
    lineWillParse line = " V " `isInfixOf` line && ("TRANS" `isInfixOf` line || "DEP" `isInfixOf` line)
