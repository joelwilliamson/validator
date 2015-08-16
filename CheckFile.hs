{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Main where

import AttoScoped (sep,value,statefulParseOnly)
import Event(Event,eventOrNamespace,event,source)
import Decision(Decision,decisionClass)
import Maker
import GatherStrings (gatherStrings)
import qualified  GatherLocalisations as GL (localisations)
import qualified GatherTraits as GT(traits)
import Localisation
import Extract(extractArchiveToTemp)

import Codec.Archive.FileCollection (
  FileCollection,
  AssocFile,
  File,
  getDirectoryContents,
  doesFileExist,
  getFile,
  readFile,
  fileName
  )

import Debug.Trace(trace)
  
import System.Console.GetOpt(ArgDescr(..),ArgOrder(Permute),OptDescr(..),getOpt,usageInfo)
import System.Environment(getArgs)
import System.Exit(ExitCode(..),exitWith,exitSuccess)
import System.Directory(doesDirectoryExist)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS(toStrict,fromStrict)
import qualified Data.Text as T
import Data.Text.IO as TIO hiding (readFile)
import Data.Text.Encoding (decodeLatin1)
import Text.Parsec.Pos(sourceName,initialPos)
import Data.Attoparsec.Text(many',parseOnly)
import Data.Either(isLeft,isRight,lefts,rights)
import Data.Maybe(fromJust,isNothing)
import Data.Monoid((<>))
import Data.Foldable(foldl')
import Data.List as L(nub,sort)
import qualified  Data.Set as S(Set,fromList,difference,toList,map,size)
import Control.Monad(filterM,liftM,when)
import Control.Applicative(liftA2)
import Codec.Archive.Zip(Archive,toArchive)

import Control.Monad.STM(atomically)
import Control.Concurrent.STM.TChan
import Control.Concurrent(forkIO)
import Control.Parallel.Strategies(parListChunk,rseq,using)

import Control.Monad.State(runStateT)

import Prelude hiding (readFile)

printErrors file (Left (e,Nothing)) = TIO.putStrLn (T.pack (fileName file) <> ": " <> e)
printErrors _ (Left (e,Just source)) = TIO.putStrLn $ e <> "\n\tat: " <> T.pack (show source)
printErrors _ (Right _) = return ()

stripBoM input = if BS.length input < 3
                 then input
                 else case BS.take 3 input of
                   "\xef\xbb\xbf" → BS.drop 3 input
                   _ → input

readEventFile :: File f ⇒ f → IO (Maybe [Event])
readEventFile file = do
  fileContents ← decodeLatin1 . stripBoM . BS.toStrict <$> readFile file
  parseResult ← case statefulParseOnly (sep *> many' value) (initialPos $ fileName file) fileContents of
    Right x → return x
    Left x → trace ("Parse failed in " <> fileName file) $ Prelude.putStrLn (fileName file <> ": " <> show x) >> return []
  let events' = map (runMaker eventOrNamespace) parseResult
  mapM_ (printErrors file) events'
  return $ if isRight $ sequence events'
           then Just $ lefts $ rights events'
           else Nothing

-- checkFiles takes a list of file paths and checks the syntax in each of them.
-- If any errors are found, a summary of the files with errors is printed and
-- the program exits. If no errors are found, the function returns a list of all
-- the events parsed.
readEventFiles :: [FilePath] → IO [Event]
readEventFiles = readFiles readEventFile

readDecisionFile :: FilePath → IO (Maybe [Decision])
readDecisionFile file = do
  fileContents ← decodeLatin1 . stripBoM . BS.toStrict <$> readFile file
  parseResult ← case statefulParseOnly (sep *> many' value) (initialPos file) fileContents of
    Right x → return x
    Left e → trace ("Parse failed in " <> file) $ return []
  let decisions = map (runMaker decisionClass) parseResult
  mapM_ (printErrors file) decisions
  return $ if isRight $ sequence decisions
           then Just $ concat $ rights decisions
           else mempty

readFiles :: (FilePath → IO (Maybe [a])) → [FilePath] → IO [a]
readFiles handler files = do
  checked ← mapM handler files
  case filter isNothing checked of
    [] → return $ concatMap fromJust checked
    _ → exitWith $ ExitFailure 1
  
strings :: ([Event],[Event]) → ([Decision],[Decision]) → T.Text
strings (_,modEvents) (_,modDecisions) = T.unlines . L.nub . L.sort $ gatherStrings modEvents <> gatherStrings modDecisions

locals :: S.Set Event → S.Set Entry → T.Text
locals events keys =
  -- For each event, get all the localisation keys and pair them with the name of the file
  let usedKeys = S.fromList $ concatMap (\e → map (`emptyEntry` (fileFromSource $ Event.source e)) $ GL.localisations e) $ S.toList events in
  let notDefined = usedKeys `S.difference` keys in
  "Used keys: " <> T.pack (show $ S.size usedKeys)
  <> ("\nUndefined keys:\n" <> T.unlines (S.toList
                          $ S.map (\l → Localisation.key l
                                      <> " in "
                                      <> T.pack (Localisation.source l))
                          notDefined))
  <> "Total undefined keys: " <> T.pack (show $ S.size notDefined)
  where emptyEntry k = Entry k "" "" "" "" "" "" "" ""

        fileFromSource (Just s) = sourceName s
        fileFromSource Nothing = "unknown"

traits (_,modEvents) (_,modDecisions) = T.unlines . L.nub . L.sort $ GT.traits modEvents <> GT.traits modDecisions

allLocalisations :: (FileCollection base, FileCollection mod) ⇒ base → Maybe mod → IO [Entry]
allLocalisations game mod = do
    rawLocalisations ← getLocalisations game mod
    if any isLeft rawLocalisations
    then TIO.putStr (T.unlines $ lefts rawLocalisations) >> return []
    else return $ concat $ rights rawLocalisations
getLocalisations :: (FileCollection base, FileCollection mod) ⇒ base → Maybe mod → IO [Either T.Text [Entry]]
getLocalisations gamePath modPath = do
  (baseFiles,modFiles) ← getFiles gamePath modPath "localisation"
  contents ← liftA2 (<>) (zip (map fileName baseFiles) <$> decodeFiles gamePath baseFiles)
             (zip (map fileName modFiles) <$> decodeFiles (fromJust modPath) modFiles)
  return $ map (uncurry localisationFile) contents
  where decodeFiles :: FileCollection fc ⇒ fc → [AssocFile fc] → IO [T.Text]
        -- The dummy fc parameter is needed to typecheck correctly
        decodeFiles _ = mapM (\f → decodeLatin1 <$> BS.toStrict <$> readFile f)

getDirectoryFiles :: FileCollection d ⇒ d → FilePath → IO [FilePath]
getDirectoryFiles root dir = (map (dir<>) <$> getDirectoryContents root dir)
                             >>= filterM (doesFileExist root) -- Is is a regular file?

getFiles :: (FileCollection base, FileCollection mod)
            ⇒ base → Maybe mod → FilePath → IO ([AssocFile base],[AssocFile mod])
getFiles base (Just mod) subPath = do
  modFiles ← getDirectoryFiles mod ("geheimnisnacht" <> "/"<>subPath<>"/")
  baseFiles ← filter (not . flip elem modFiles) <$> getDirectoryFiles base ("/"<>subPath<>"/")
  return (map (getFile base) baseFiles,map (getFile mod) modFiles)
getFiles base Nothing subPath = do
  baseFiles ← getDirectoryFiles base ("/"<>subPath<>"/")
  return (map (getFile base) baseFiles,[])


-- Get the base game events and any mod events
getEvents :: FilePath → Maybe FilePath → IO ([Event],[Event])
getEvents base mod = do
  (baseFiles,modFiles) ← getFiles base mod "events"
  trace ("Found " <> show (length baseFiles + length modFiles) <> " event files") $ return ()
  (,) <$> readEventFiles baseFiles <*> readEventFiles modFiles

-- | Given the base and mod directories, return lists of all decisions in the
-- game.
readDecisions :: FilePath → Maybe FilePath → IO ([Decision],[Decision])
readDecisions base mod = do
  (baseFiles,modFiles) ← getFiles base mod "decisions"
  trace ("Found " <> show (length baseFiles + length modFiles) <> " decision files") $ return ()
  (,) <$> readFiles readDecisionFile baseFiles <*> readFiles readDecisionFile modFiles


data Action = Localisations | Strings | Traits deriving (Eq,Show)

data Resources = Resources {
  baseEvents :: [Event],
  modEvents :: [Event],
  baseDecisions :: [Decision],
  modDecisions :: [Decision],
  localisations :: [Entry]
  } deriving (Eq,Show)

dispatch :: Resources → Action → T.Text
dispatch r Localisations = locals (S.fromList $ baseEvents r <> modEvents r) (S.fromList $ localisations r)
dispatch r Strings = strings (baseEvents r, modEvents r) (baseDecisions r, modDecisions r)
dispatch r Traits = traits (baseEvents r, modEvents r) (baseDecisions r, modDecisions r)

dispatchFromChan :: Resources → TChan Action → TChan T.Text → IO ()
dispatchFromChan resources action response = do
  atomically $ do
    next ← readTChan action
    let result = dispatch resources next
    writeTChan response result
  dispatchFromChan resources action response

-- Start a checker thread for a given mod. The first TChan is used to send check
-- requests to the thread, while the second is used to send results back
startCheck :: Resources → IO (TChan Action, TChan T.Text)
startCheck resources = do
  action ← atomically newTChan
  response ← atomically newTChan
  _ ← forkIO $ dispatchFromChan resources action response
  return (action,response)

argDispatcher :: Args → TChan Action → TChan T.Text → IO ()
argDispatcher a action result = do
  _ ← when (_traits a) $ onChan Traits
  _ ← when (stringResources a) $ onChan Strings
  when (localisationKeys a) $ onChan Localisations
    where onChan a = atomically (writeTChan action a) >> atomically (readTChan result) >>= TIO.putStrLn


-- There will be three entry possibilities.
-- Mode   | baseType  | modType
-- server | directory | archive
-- local  | directory | archivePath
-- local  | directory | directoryPath
-- In each mode, we want a function that returns a path to a directory containing
-- the mod files.
archiveMod :: Archive → IO FilePath
archiveMod = extractArchiveToTemp
localMod path = do
  isDir ← doesDirectoryExist path
  if isDir
    then return path
    else liftM ( toArchive . BS.fromStrict ) (BS.readFile path) >>= extractArchiveToTemp

data Args = Args {
  showHelp :: Bool
  , stringResources :: Bool
  , localisationKeys :: Bool
  , _traits :: Bool
  , gameRootPath :: FilePath
  , modRootPath :: Maybe FilePath
  } deriving (Eq,Show)

defaultArgs = Args {
  showHelp = False
  , stringResources = False
  , localisationKeys = False
  , _traits = False
  , gameRootPath = "/home/joel/.local/share/Steam/steamapps/common/Crusader Kings II/"
  , modRootPath = Nothing
  }

options :: [OptDescr (Args → Args)]
options =
  [ Option "s" ["strings"] (NoArg $ \o → o { stringResources = True }) "find all string resources"
  , Option "l" ["localisations"] (NoArg $ \o → o { localisationKeys = True }) "find all localisation keys"
  , Option "t" ["traits"] (NoArg $ \o → o { _traits = True }) "find all referenced traits"
  ]
  <> [ Option "G" ["game-dir"] (ReqArg (\fp o → o { gameRootPath = fp }) "DIR") "folder containing the base game"
     , Option "M" ["mod-dir"] (ReqArg (\fp o → o { modRootPath = Just fp}) "DIR") "folder containing the mod"
     ]
  <> [ Option "h" ["help"] (NoArg $ \o → o { showHelp = True }) "print this message"
     ]

main = do
  (rawArgs,_,_) ← getOpt Permute options <$> getArgs
  let args = foldl' (flip ($)) defaultArgs rawArgs
  when (showHelp args)
    $ Prelude.putStrLn (usageInfo "validator:" options) >> exitSuccess
  let (rawModPath,gamePath) = (modRootPath args,gameRootPath args)
  modPath ← sequence $ localMod <$> rawModPath
  (baseEvents,modEvents) ← getEvents gamePath modPath
  (baseDecisions,modDecisions) ← readDecisions gamePath modPath
  uncheckedLocalisations ← sequence <$> getLocalisations gamePath modPath
  let locs = case uncheckedLocalisations of
        Right l → trace "got localisations" l
        Left e → error $ "localisation error: " <> T.unpack e
  (action,result) ← startCheck Resources { baseEvents, modEvents, baseDecisions, modDecisions, localisations = concat locs }
  argDispatcher args action result
  exitSuccess
