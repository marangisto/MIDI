{-# LANGUAGE DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}
module Main where

import System.Console.CmdArgs
--import qualified Data.ByteString.Char8 as B
--import System.Hardware.Serialport
--import System.Console.Haskeline
--import System.Process (system)
import System.Directory
--import Control.Concurrent
--import Control.Exception
--import Control.Monad.IO.Class
--import Control.Monad.Loops
import Control.Monad.Extra
import Codec.Midi as MIDI
import Numeric (showHex)
import Data.Monoid
import Data.Maybe
--import Control.Monad
--import Data.List (stripPrefix)
--import Data.Maybe
--import Data.IORef
--import System.IO

data Options = Options
    { port      :: FilePath
    , dir       :: Maybe FilePath
    , files     :: [FilePath]
    } deriving (Show, Eq, Data, Typeable)

options :: Main.Options
options = Main.Options
    { port = def &= help "serial port"
    , dir = def &= help "working directory"
    , files = def &= args &= typ "FILES"
    } &=
    verbosity &=
    help "MIDI" &=
    summary "MIDI v0.0.0, (c) Bengt Marten Agren 2020" &=
    details [ "MIDI tool"
            ]

main :: IO ()
main = do
    opts@Options{..} <- cmdArgs options
    whenJust dir setCurrentDirectory
    -- hSetNewlineMode stdout noNewlineTranslation
    -- print opts
    forM_ files $ \fp -> either error processMIDI =<< MIDI.importFile fp

processMIDI :: Midi -> IO ()
processMIDI Midi{..} = do
    -- print fileType
    -- print timeDiv
    -- putStrLn $ "nTracks " <> show (length tracks)
    forM_ tracks processTrack

processTrack :: Track Ticks -> IO ()
processTrack xs = do
    -- putStrLn $ "trackLength " <> show (length xs)
    mapM_ putStrLn $ mapMaybe (uncurry translateMidi) xs

translateMidi :: Ticks -> Message -> Maybe String
translateMidi n NoteOn{..} = Just $ unwords
    [ show n
    , "9" <> showHex channel ""
    , show key
    , show velocity
    ]
translateMidi n NoteOff{..} = Just $ unwords
    [ show n
    , "8" <> showHex channel ""
    , show key
    , show velocity
    ]
translateMidi _ _ = Nothing

