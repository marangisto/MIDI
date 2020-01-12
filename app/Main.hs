{-# LANGUAGE DeriveDataTypeable, RecordWildCards, OverloadedStrings, TupleSections #-}
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
import Data.List.Extra
import Data.List (sortOn)
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
    forM_ (mergeTracks tracks) processTrack

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

mergeTracks :: [Track Ticks] -> [Track Ticks]
mergeTracks
    = (:[])
    . concatMap ungroup
    . differentiateTicks
    . groupSort
    . sortOn fst
    . filter (keeper . snd)
    . concatMap integrateTicks
    . zipWith remapChannel [1..]

integrateTicks :: Track Ticks -> Track Ticks
integrateTicks [] = []
integrateTicks xs = scanl f (head xs) xs
    where f (tx, mx) (ty, my) = (tx + ty, my)

differentiateTicks :: [(Ticks, [Message])] -> [(Ticks, [Message])] 
differentiateTicks xs@(x:_) = x : zipWith f (init xs) (tail xs)
    where f (tx, mxs) (ty, mys) = (ty - tx, mys)

ungroup :: (Ticks, [Message]) -> [(Ticks, Message)]
ungroup (t, (x:xs)) = (t, x) : map (0,) xs
    
keeper :: Message -> Bool
keeper NoteOn{} = True
keeper NoteOff{} = True
keeper _ = False

remapChannel :: Int -> Track Ticks -> Track Ticks
remapChannel i = map f
    where f (t, m@NoteOn{}) = (t, m { channel = i })
          f (t, m@NoteOff{}) = (t, m { channel = i })
          f (t, m) = (t, m)


