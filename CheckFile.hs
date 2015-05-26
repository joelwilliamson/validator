{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Main where


import Event(Event,eventFile,event,source)
import Maker
import GatherStrings (gatherStrings)
import qualified  GatherLocalisations as GL (localisations)
import Localisation

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

import Debug.Trace
  
import System.Console.GetOpt
import System.Environment
import System.Exit

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS(toStrict,fromStrict)
import qualified Data.Text as T
import Data.Text.IO as TIO hiding (readFile)
import Data.Text.Encoding (decodeLatin1)
import qualified Data.Text as T(pack,unlines,length)
import Text.Parsec(parse,ParseError)
import Text.Parsec.Pos(sourceName)
import Data.Attoparsec.Text(parseOnly)
import Data.Either
import Data.Maybe
import Data.Monoid((<>))
import Data.Foldable(foldl')
import Data.List as L(nub,sort)
import qualified  Data.Set as S(Set,fromList,difference,size,toList,map)
import Control.Exception
import Control.Monad(filterM)
import Control.Applicative(liftA2)
import Codec.Archive.Zip(toArchive)

import Control.Monad.STM(atomically)
import Control.Concurrent.STM.TChan
import Control.Concurrent(forkIO)
import Control.Parallel.Strategies(parListChunk,rseq,using)

import Prelude hiding (readFile)

printErrors file (Left (e,Nothing)) = TIO.putStrLn (T.pack (fileName file) <> ": " <> e)
printErrors _ (Left (e,Just source)) = TIO.putStrLn $ e <> "\n\tat: " <> T.pack (show source)
printErrors _ (Right _) = return ()

stripBoM input = if BS.length input < 3
                 then input
                 else case BS.take 3 input of
                   "\xef\xbb\xbf" → BS.drop 3 input
                   _ → input

checkFile :: File f ⇒ f → IO (Maybe [Event])
checkFile file = do
  fileContents ← decodeLatin1 . stripBoM . BS.toStrict <$> readFile file
  parseResult ← case parseOnly eventFile fileContents of
    Right x → return x
    Left x → Prelude.putStrLn (fileName file <> ": " <> show x) >> return ([],[])
  let events' = map (runMaker event) $ snd parseResult
  mapM_ (printErrors file) events'
  return $ if isRight $ sequence events'
           then Just $ rights events'
           else Nothing

-- checkFiles takes a list of file paths and checks the syntax in each of them.
-- If any errors are found, a summary of the files with errors is printed and
-- the program exits. If no errors are found, the function returns a list of all
-- the events parsed.
checkFiles :: File file ⇒ [file] → IO [Event]
checkFiles names = do
  checked ← sequence $ (map checkFile names `using` parListChunk 4 rseq)
  let results = zip names checked
  let errors = filter (isNothing . snd) results
  if null errors
    then return $ concat $ map fromJust checked
    else exitWith $ ExitFailure 1

thenDo :: Bool → IO () → IO ()
cond `thenDo` action = if cond then action else return ()

strings :: ([Event],[Event]) → T.Text
strings (_,modEvents) = T.unlines . L.nub . L.sort $ gatherStrings modEvents

locals :: S.Set Event → S.Set Entry → T.Text
locals events keys =
  -- For each event, get all the localisation keys and pair them with the name of the file
  let usedKeys = S.fromList $ concat $ map (\e → map (flip emptyEntry (fileFromSource $ Event.source e)) $ GL.localisations e) $ S.toList events in
  let unused = keys `S.difference` usedKeys in
  let notDefined = usedKeys `S.difference` keys in
  "Undefined keys:\n" <> (T.unlines $ S.toList
                          $ S.map (\l → Localisation.key l
                                      <> " in "
                                      <> T.pack (Localisation.source l))
                          $ notDefined)
  where emptyEntry k s = Entry k "" "" "" "" "" "" "" "" s

        fileFromSource (Just s) = sourceName s
        fileFromSource Nothing = "unknown"

  
allLocalisations :: (FileCollection base, FileCollection mod) ⇒ base → Maybe mod → IO [Entry]
allLocalisations game mod = do
    rawLocalisations ← getLocalisations game mod
    if any isLeft rawLocalisations
    then (Prelude.putStr $ unlines . map unlines $ map (map show) $ lefts rawLocalisations) >> return []
    else return $ concat $ rights rawLocalisations
getLocalisations :: (FileCollection base, FileCollection mod) ⇒ base → Maybe mod → IO [Either [T.Text] [Entry]]
getLocalisations gamePath modPath = do
  (baseFiles,modFiles) ← getFiles gamePath modPath "localisation"
  contents ← liftA2 (<>) (zip (map fileName baseFiles) <$> decodeFiles gamePath baseFiles)
             (zip (map fileName modFiles) <$> decodeFiles (fromJust modPath) modFiles)
  return $ map (uncurry localisationFile) contents
  where decodeFiles :: FileCollection fc ⇒ fc → [AssocFile fc] → IO [T.Text]
        -- The dummy fc parameter is needed to typecheck correctly
        decodeFiles _ = mapM (\f → decodeLatin1 <$> BS.toStrict <$> readFile f)


data Args = Args {
  showHelp :: Bool
  , stringResources :: Bool
  , localisationKeys :: Bool
  , gameRootPath :: FilePath
  , modRootPath :: Maybe FilePath
  } deriving (Eq,Show)

defaultArgs = Args {
  showHelp = False
  , stringResources = False
  , localisationKeys = False
  , gameRootPath = "/home/joel/.local/share/Steam/steamapps/common/Crusader Kings II/"
  , modRootPath = Nothing
  } 

options :: [OptDescr (Args → Args)]
options =
  [ Option ['s'] ["strings"] (NoArg $ \o → o { stringResources = True }) "find all string resources referenced by events"
  , Option ['l'] ["localisations"] (NoArg $ \o → o { localisationKeys = True }) "find all localisation keys"
  ]
  <> [ Option ['G'] ["game-dir"] (ReqArg (\fp o → o { gameRootPath = fp }) "DIR") "folder containing the base game"
     , Option ['M'] ["mod-dir"] (ReqArg (\fp o → o { modRootPath = Just fp}) "DIR") "folder containing the mod"
     ]
  <> [ Option ['h'] ["help"] (NoArg $ \o → o { showHelp = True }) "print this message"
     ]

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
getEvents :: (FileCollection base, FileCollection mod) ⇒ base → Maybe mod → IO ([Event],[Event])
getEvents base mod = do
  (baseFiles,modFiles) ← trace "Got Files ..." $ getFiles base mod "events"
  trace ("Found " <> show (length baseFiles + length modFiles) <> " files") $ return ()
  (,) <$> checkFiles baseFiles <*> checkFiles modFiles

data Action = Localisations | Strings deriving (Eq,Show)

data Resources = Resources {
  baseEvents :: [Event],
  modEvents :: [Event],
  localisations :: [Entry]
  } deriving (Eq,Show)

dispatch :: Resources → Action → T.Text
dispatch r Localisations = locals (S.fromList $ baseEvents r <> modEvents r) (S.fromList $ localisations r)
dispatch r Strings = strings (baseEvents r, modEvents r)

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
  action ← atomically $ newTChan
  response ← atomically $ newTChan
  forkIO $ dispatchFromChan resources action response
  return (action,response)

argDispatcher :: Args → TChan Action → TChan T.Text → IO ()
argDispatcher a action result = do
  if stringResources a
    then atomically (writeTChan action Strings) >> atomically (readTChan result) >>= TIO.putStrLn
    else return ()
  if localisationKeys a
    then atomically (writeTChan action Localisations) >> atomically (readTChan result)  >>= TIO.putStrLn
    else return ()

main = do
  (rawArgs,_,_) ← getOpt Permute options <$> getArgs
  let args = foldl' (flip ($)) defaultArgs rawArgs
  if showHelp args
    then (Prelude.putStrLn $ usageInfo "validator:" options) >> exitWith ExitSuccess
    else return ()
  let (modPath,gamePath) = (modRootPath args,gameRootPath args)
  modArch ← sequence $ (toArchive . BS.fromStrict <$>) <$> (BS.readFile <$> modPath)
  (baseEvents,modEvents) ← getEvents gamePath modArch
  -- WARNING: Partial
  (Right locs) ← sequence <$> getLocalisations gamePath modArch
  Prelude.putStrLn $ show gamePath
  (action,result) ← startCheck Resources { baseEvents, modEvents, localisations = concat locs }
  argDispatcher args action result
  exitWith $ ExitSuccess
