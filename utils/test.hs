#!/usr/bin/env cabal
{- cabal:
build-depends:
    base          >= 4.20.2   && < 4.23
  , bytestring    >= 0.12.2   && < 0.13
  , conduit       >= 1.3.6.1  && < 1.4
  , conduit-extra >= 1.3.8    && < 1.4
  , directory     >= 1.3.8.5  && < 1.4
  , filepath      >= 1.5.4    && < 1.6
  , process       >= 1.6.26.1 && < 1.7
  , temporary     >= 1.3      && < 1.4
  , time          >= 1.12.2   && < 1.16
  , ttc           >= 1.5.0.1  && < 1.6
default-language: GHC2021
ghc-options: -Wall -Wcompat -threaded
-}
{- project:
--with-compiler: ghc-9.10.3
--with-compiler: ghc-9.12.4
--with-compiler: ghc-9.14.1
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- https://hackage.haskell.org/package/base
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.MVar qualified as MVar
import Control.Monad (forM_, when)
import Data.Char qualified as Char
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Void (Void)
import System.Environment qualified as Env
import System.Exit qualified as Exit
import System.IO qualified as IO
import Text.Printf (printf)

-- https://hackage.haskell.org/package/bytestring
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS

-- https://hackage.haskell.org/package/conduit
import Data.Conduit (ConduitT, (.|))
import Data.Conduit.Combinators qualified as CC

-- https://hackage.haskell.org/package/conduit-extra
import Data.Conduit.Process qualified as CP

-- https://hackage.haskell.org/package/directory
import System.Directory qualified as Dir

-- https://hackage.haskell.org/package/filepath
import System.FilePath ((</>))
import System.FilePath qualified as FilePath

-- https://hackage.haskell.org/package/process
import System.Process qualified as Process

-- https://hackage.haskell.org/package/temporary
import System.IO.Temp (withSystemTempDirectory)

-- https://hackage.haskell.org/package/time
import Data.Time.Clock qualified as Clock
import Data.Time.Format qualified as TF
import Data.Time.LocalTime qualified as LocalTime

-- https://hackage.haskell.org/package/ttc
import Data.TTC qualified as TTC

------------------------------------------------------------------------------

backends :: [String]
backends = ["native", "ptx"]

------------------------------------------------------------------------------

main :: IO ()
main = do
    timestamp <- formatTimestamp <$> LocalTime.getZonedTime
    (backend, opts, mTag) <- parseArgs
    projectRootDir <- findProjectRoot
    checkSubmodules
    let prefix = "acc-nofib-" ++ backend ++ "-"
        logFile = formatFilename prefix mTag timestamp ".log"
        xmlFile = formatFilename prefix mTag timestamp ".xml"
    IO.withFile logFile IO.WriteMode $ \handle -> do
      lock <- MVar.newMVar ()
      let putLineLog = writeLine lock [handle]
          putLineOut = writeLine lock [IO.stdout, handle]
          putLineErr = writeLine lock [IO.stderr, handle]

      logHeader putLineOut timestamp backend opts mTag

      hr putLineLog "repository"
      run_ putLineLog putLineErr "git" ["log", "--max-count=1", "--oneline"]
      putLineLog BS.empty
      run_ putLineLog putLineErr "git" ["status", "--short", "--branch"]

      forM_ ["cabal.project", "cabal.project.local"] $ \configFile -> do
        let configPath = projectRootDir </> configFile
        found <- Dir.doesFileExist configPath
        when found $ do
          hr  putLineLog configFile
          cat putLineLog configPath

      hr putLineOut "accelerate-llvm"
      run putLineOut putLineErr "cabal" ["build", "accelerate-llvm"]

      let pkg = "accelerate-llvm-" ++ backend
      hr putLineOut pkg
      run putLineOut putLineErr "cabal" ["build", pkg]

      let tst = pkg ++ ":test:nofib-llvm-" ++ backend
      hr putLineOut tst
      run putLineOut putLineErr "cabal" ["build", tst]

      hr putLineOut "test"
      bin <- getBin putLineErr tst
      withSystemTempDirectory prefix $ \tmpDir -> do
        logDetails putLineOut bin tmpDir
        Env.setEnv "XDG_CACHE_HOME" tmpDir
        run putLineOut putLineErr bin $
          ["--color", "never", "--xml", xmlFile] ++ opts

      hr putLineOut "fin"
      logFooter putLineOut logFile xmlFile

------------------------------------------------------------------------------

type Timestamp = String

formatTimestamp :: LocalTime.ZonedTime -> Timestamp
formatTimestamp = TF.formatTime TF.defaultTimeLocale "%Y%m%d%H%M%S"

type Backend = String
type Option  = String
type Tag     = String

parseArgs :: IO (Backend, [Option], Maybe Tag)
parseArgs = do
    args <- Env.getArgs
    when ("--help" `elem` args) $ usageExit False
    case fmap (List.span isOption) <$> List.uncons args of
      Just (backend, (opts, args'))
        | isValidBackend backend -> case args' of
            []                 -> pure (backend, opts, Nothing)
            [tag]
              | isValidTag tag -> pure (backend, opts, Just tag)
              | otherwise      -> errorExit $ "invalid tag: " ++ show tag
            _otherwise         -> usageExit True
        | otherwise -> errorExit $ "invalid backend: " ++ show backend
      Nothing -> usageExit True
  where
    isOption :: String -> Bool
    isOption = (== "-") . take 1

    isValidBackend :: String -> Bool
    isValidBackend = (`elem` backends)

    isValidTag :: String -> Bool
    isValidTag s = not (List.null s) && all isValidTagChar s

    isValidTagChar :: Char -> Bool
    isValidTagChar c =
      let o = Char.ord c
      in  (||) (o == 0x2D) $  -- -
          (&&) (o >= 0x30) $
          (||) (o <= 0x39) $  -- [0-9]
          (&&) (o >= 0x41) $
          (||) (o <= 0x5A) $  -- [A-Z]
          (||) (o == 0x5F) $  -- _
          (&&) (o >= 0x61)
               (o <= 0x7A)    -- [a-z]

    usageExit :: Bool -> IO a
    usageExit isError = do
      progName <- Env.getProgName
      mapM_ (if isError then IO.hPutStrLn IO.stderr else putStrLn)
        [ "Usage: " ++ progName ++ " BACKEND [OPTIONS] [tag]"
        , ""
        , "Backends: " ++ List.intercalate ", " backends
        , "Options:  options to pass to the Tasty executable"
        , "Tag:      filename tag (/^[-0-9A-Z_a-z]+$/)"
        ]
      Exit.exitWith $ if isError then Exit.ExitFailure 2 else Exit.ExitSuccess

    errorExit :: String -> IO a
    errorExit msg = do
      IO.hPutStrLn IO.stderr $ "error: " ++ msg
      Exit.exitWith $ Exit.ExitFailure 1

type Prefix    = String
type Extension = String

formatFilename :: Prefix -> Maybe Tag -> Timestamp -> Extension -> FilePath
formatFilename prefix mTag timestamp = FilePath.addExtension $
    prefix ++ maybe "" (++ "-") mTag ++ timestamp

findProjectRoot :: IO FilePath
findProjectRoot = go =<< Dir.getCurrentDirectory
  where
    go :: FilePath -> IO FilePath
    go dir = Dir.doesFileExist (dir </> "cabal.project") >>= \case
      True  -> pure dir
      False -> case FilePath.takeDirectory dir of
        dir'
          | dir' /= dir -> go dir'
          | otherwise   -> do
              IO.hPutStrLn IO.stderr
                "error: unable to find project root: cabal.project not found"
              Exit.exitWith $ Exit.ExitFailure 1

checkSubmodules :: IO ()
checkSubmodules = do
    (_, stdout, _) <- Process.readProcessWithExitCode
      "git" ["submodule", "status", "--recursive"] ""
    case filter isNotOK (lines stdout) of
      []         -> pure ()
      submodules -> do
        mapM_ (IO.hPutStrLn IO.stderr) $
          "error: git submodules out-of-sync or uninitialized:"
            : map ("  " ++) submodules
        Exit.exitWith $ Exit.ExitFailure 1
  where
    isNotOK :: String -> Bool
    isNotOK = (`elem` ["-", "+", "U"]) . take 1

type PutLine = ByteString -> IO ()

writeLine :: MVar () -> [IO.Handle] -> PutLine
writeLine lock handles line = MVar.withMVar lock $ \() ->
    forM_ handles $ \handle -> do
      BS.hPut handle line
      BS.hPut handle $ BS.singleton 0x0A  -- newline
      IO.hFlush handle

logHeader :: PutLine -> Timestamp -> Backend -> [Option] -> Maybe Tag -> IO ()
logHeader putLine timestamp backend opts mTag = mapM_ (putLine . TTC.toBS)
    [ "Timestamp: " ++ timestamp
    , "Backend:   " ++ backend
    , "Options:   " ++ show opts
    , "Tag:       " ++ show mTag
    ]

hr :: PutLine -> String -> IO ()
hr putLine label = do
    let l = "==|" ++ label ++ "|=="
        r = replicate (max 0 (80 - length l)) '='
    mapM_ (putLine . TTC.toBS)
      [ ""
      , l ++ r
      , ""
      ]

run :: PutLine -> PutLine -> FilePath -> [String] -> IO ()
run putLineOut putLineErr cmd args = run' timed putLineOut putLineErr cmd args
  where
    timed :: IO a -> IO a
    timed m = do
      timeBegin <- LocalTime.getZonedTime
      mapM_ (putLineOut . TTC.toBS)
        [ "Begin time: " ++ formatTime timeBegin
        , ""
        ]
      x <- m
      timeEnd <- LocalTime.getZonedTime
      let duration = LocalTime.diffLocalTime
            (LocalTime.zonedTimeToLocalTime timeEnd)
            (LocalTime.zonedTimeToLocalTime timeBegin)
      mapM_ (putLineOut . TTC.toBS)
        [ ""
        , "End time: " ++ formatTime timeEnd
        , "Duration: " ++ formatDuration duration
        ]
      pure x

run_ :: PutLine -> PutLine -> FilePath -> [String] -> IO ()
run_ = run' id

run'
  :: (forall a. IO a -> IO a)
  -> PutLine
  -> PutLine
  -> FilePath
  -> [String]
  -> IO ()
run' timed putLineOut putLineErr cmd args = do
    exitCode <- timed $ do
      (exitCode, (), ()) <- CP.sourceProcessWithStreams
        (Process.proc cmd args)
        (pure ())
        (sinkLines putLineOut)
        (sinkLines putLineErr)
      pure exitCode
    checkSuccess exitCode
  where
    sinkLines :: PutLine -> ConduitT ByteString Void IO ()
    sinkLines putLine = CC.linesUnboundedAscii .| CC.mapM_ putLine

    checkSuccess :: Exit.ExitCode -> IO ()
    checkSuccess = \case
      Exit.ExitSuccess   -> pure ()
      Exit.ExitFailure n -> do
        mapM_ (putLineErr . TTC.toBS)
          [ ""
          , "error: calling " ++ unwords (cmd : args)
              ++ "exited with code " ++ show n
          ]
        Exit.exitWith $ Exit.ExitFailure (100 + n)

formatTime :: LocalTime.ZonedTime -> String
formatTime = TF.formatTime TF.defaultTimeLocale "%Y-%m-%d %H:%M:%S"

formatDuration :: Clock.NominalDiffTime -> String
formatDuration duration
    | totalSecs < 60 = printf "%d.%03d seconds" totalSecs ms
    | otherwise = unwords $ Maybe.catMaybes
        [ go hours   "hour"   "hours"
        , go minutes "minute" "minutes"
        , go seconds "second" "seconds"
        , Just $ printf "(%02d:%02d:%02d)" hours minutes seconds
        ]
  where
    totalMs :: Integer
    totalMs = abs $ round (duration * 1_000)

    totalSecs, ms :: Integer
    (totalSecs, ms) = totalMs `divMod` 1_000

    roundedSecs :: Integer
    roundedSecs
      | ms < 500  = totalSecs
      | otherwise = totalSecs + 1

    hours, hSecs :: Integer
    (hours, hSecs) = roundedSecs `divMod` 3_600

    minutes, seconds :: Integer
    (minutes, seconds) = hSecs `divMod` 60

    go :: Integer -> String -> String -> Maybe String
    go n singular plural
      | n == 0    = Nothing
      | n == 1    = Just $ show n ++ ' ' : singular
      | otherwise = Just $ show n ++ ' ' : plural

cat :: PutLine -> FilePath -> IO ()
cat putLine path = BS.readFile path >>= \bs ->
    case BS.unsnoc bs of
      Just (bs', w)
        | w == 0x0A -> putLine bs'  -- newline
        | otherwise -> putLine bs
      Nothing       -> pure ()

getBin :: PutLine -> String -> IO FilePath
getBin putLineErr name = do
    path <- stripEnd <$> Process.readProcess "cabal" ["list-bin", name] ""
    when (null path) $ do
      putLineErr . TTC.toBS $ "error: unable to find executable: " ++ name
      Exit.exitWith $ Exit.ExitFailure 1
    pure path
  where
    stripEnd :: String -> String
    stripEnd = List.dropWhileEnd Char.isSpace

logDetails :: PutLine -> FilePath -> FilePath -> IO ()
logDetails putLine bin tmpDir = mapM_ (putLine . TTC.toBS)
    [ "Executable path:     " ++ bin
    , "Temporary directory: " ++ tmpDir
    , ""
    ]

logFooter :: PutLine -> FilePath -> FilePath -> IO ()
logFooter putLine logFile xmlFile = mapM_ (putLine . TTC.toBS)
    [ "Log file: " ++ logFile
    , "XML file: " ++ xmlFile
    ]
