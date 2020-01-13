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
import Control.Monad
import Control.Arrow (second)
import Codec.Midi as MIDI
import Numeric (showHex)
import Data.List.Extra
import Data.List (sortOn)
import Data.Monoid
import Data.Maybe
--import Data.List (stripPrefix)
--import Data.Maybe
--import Data.IORef
--import System.IO

data Options = Options
    { port      :: FilePath
    , dir       :: Maybe FilePath
    , analyze   :: Bool
    , files     :: [FilePath]
    } deriving (Show, Eq, Data, Typeable)

options :: Main.Options
options = Main.Options
    { port = def &= help "serial port"
    , dir = def &= help "working directory"
    , analyze = def &= help "analyze contents"
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
    forM_ files $ \fp -> either error (processMIDI opts) =<< MIDI.importFile fp

processMIDI :: Options -> Midi -> IO ()
processMIDI Options{analyze=True,..} Midi{..}  = do
    print fileType
    print timeDiv
    putStrLn $ "nTracks " <> show (length tracks)
    forM_ (zip [0..] tracks) $ \(i, xs) -> do
        putStrLn $ "track[" <> show i <> "]: " <> fromMaybe "" (trackName xs)
        putStrLn $ "channels" <> show (catMaybes $ map fst $ separateChannels xs)
        forM_ (trackText xs) $ putStrLn . ("    "<>)
        forM_ (copyright xs) $ putStrLn . ("    "<>)
        forM_ (messageStats xs) $ \(c, n) ->
            putStrLn (unwords [ "   ", show c, show n ])

processMIDI _ Midi{..} = forM_ (mergeTracks tracks) processTrack

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

toIntervals :: Track Ticks -> [((Ticks, Message), Maybe (Ticks, Message))]  -- note-on, maybe note-off
toIntervals xs = undefined
    -- fixme: this should be typed to capture all events for this channel bracketed by on and off messages
    -- where bynotes = groupSortOn undefined

separateChannels :: Track Ticks -> [(Maybe Channel, Track Ticks)] -- use on integrated time!
separateChannels = groupSort . map (\(t, m) -> (messageChannel m, (t, m)))

messageStats :: Track Ticks -> [(Constructor, Int)]
messageStats = map (second length) . groupSort . map ((,()) . constructor . snd)

data Constructor
    = CNoteOff
    | CNoteOn
    | CKeyPressure
    | CControlChange
    | CProgramChange
    | CChannelPressure
    | CPitchWheel
    | CSequenceNumber
    | CText
    | CCopyright
    | CTrackName
    | CInstrumentName
    | CLyrics
    | CMarker
    | CCuePoint
    | CChannelPrefix
    | CProgramName
    | CDeviceName
    | CTrackEnd
    | CTempoChange
    | CSMPTEOffset
    | CTimeSignature
    | CKeySignature
    | CReserved
    | CSysex
    deriving (Eq, Ord, Show)

constructor :: Message -> Constructor
constructor ChannelPrefix{} = CChannelPrefix
constructor ChannelPressure{} = CChannelPressure
constructor ControlChange{} = CControlChange
constructor Copyright{} = CCopyright
constructor CuePoint{} = CCuePoint
constructor DeviceName{} = CDeviceName
constructor InstrumentName{} = CInstrumentName
constructor KeyPressure{} = CKeyPressure
constructor KeySignature{} = CKeySignature
constructor Lyrics{} = CLyrics
constructor Marker{} = CMarker
constructor NoteOff{} = CNoteOff
constructor NoteOn{} = CNoteOn
constructor PitchWheel{} = CPitchWheel
constructor ProgramChange{} = CProgramChange
constructor ProgramName{} = CProgramName
constructor Reserved{} = CReserved
constructor SequenceNumber{} = CSequenceNumber
constructor SMPTEOffset{} = CSMPTEOffset
constructor Sysex{} = CSysex
constructor TempoChange{} = CTempoChange
constructor Text{} = CText
constructor TimeSignature{} = CTimeSignature
constructor TrackEnd{} = CTrackEnd
constructor TrackName{} = CTrackName

trackName :: Track Ticks -> Maybe String
trackName xs
    | (s:_) <- [ s | (_, TrackName s) <- xs ] = Just s
    | otherwise = Nothing

trackText :: Track Ticks -> [String]
trackText xs = [ s |  (_, Text s) <- xs ]

copyright :: Track Ticks -> [String]
copyright xs = [ s |  (_, Copyright s) <- xs ]

messageChannel :: Message -> Maybe Int
messageChannel NoteOff{..} = Just channel
messageChannel NoteOn{..} = Just channel
messageChannel KeyPressure{..} = Just channel
messageChannel ControlChange{..} = Just channel
messageChannel ProgramChange{..} = Just channel
messageChannel ChannelPressure{..} = Just channel
messageChannel PitchWheel{..} = Just channel
messageChannel (ChannelPrefix channel) = Just channel
messageChannel _ = Nothing
