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
import qualified Data.Vector as Vector
import Data.Vector ((//), (!))
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
        forM_ (messageStats xs) $ \(c, n) -> putStrLn (unwords [ "   ", show c, show n ])
        let ys = noteOnOff $ integrateTicks xs
        mapM_ print ys
--processMIDI _ Midi{..} = forM_ (mergeTracks tracks) processTrack
processMIDI _ Midi{..}
    = forM_ (mergeTracks tracks)
    $ processTrack
    . differentiateTicks
    . noteOnOff
    . integrateTicks

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

--differentiateTicks :: [(Ticks, [Message])] -> [(Ticks, [Message])]
differentiateTicks :: [(Ticks, a)] -> [(Ticks, a)]
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

separateKeys :: Track Ticks -> [(Maybe Key, [(Ticks, Message)])] -- use on integrated time!
separateKeys = groupSort . map (\(t, m) -> (messageKey m, (t, m)))

separateChannels :: Track Ticks -> [(Maybe Channel, Track Ticks)] -- use on integrated time!
separateChannels = groupSort . map (\(t, m) -> (messageChannel m, (t, m)))

xnoteOnOff :: Track Ticks -> Track Ticks -- use on integrated time!
xnoteOnOff xs = concat
    [ [ (t, NoteOn c k v), (t, NoteOff c k 127) ]
    | ((t, NoteOn { key = k, velocity = v }), c)
    <- zip ys $ concat $ repeat [1..8]
    ]
    where ys = [ x | x@(_, NoteOn{}) <- xs ]

data State
    = Idle Ticks
    | Playing Ticks Key
    deriving (Eq, Ord, Show)

type Chans = Vector.Vector State

mkChans :: Int -> Chans
mkChans n = Vector.replicate n (Idle 0)

isIdle :: State -> Bool
isIdle (Idle _) = True
isIdle _ = False

ticks :: State -> Ticks
ticks (Idle t) = t
ticks (Playing t _) = t

lru :: Chans -> Maybe Int
lru chans
    | ((_, c):_) <- sort ys = Just c
    | otherwise = Nothing
    where xs = Vector.toList $ Vector.findIndices isIdle chans
          ys = map (\c -> (ticks $ chans!c, c)) xs

isPlaying :: Key -> State -> Bool
isPlaying key (Playing _ k) = k == key
isPlaying _ _ = False

noteOnOff :: Track Ticks -> Track Ticks -- use on integrated time!
noteOnOff = snd . mapAccumL playNote (mkChans 8) . filter playable
    where playable (_, NoteOn{}) = True
          playable (_, NoteOff{}) = True
          playable _ = False

playNote :: Chans -> (Ticks, Message) -> (Chans, (Ticks, Message))
playNote chans (t, m@NoteOn{..})
    | Just c <- lru chans
    = (chans // [(c, Playing t key)], (t, m { channel = c + 1 }))
    | otherwise = error "out of channels"
playNote chans (t, m@NoteOff{..})
    | Just c <- Vector.findIndex (isPlaying key) chans
    = (chans // [(c, Idle t)], (t, m { channel = c + 1 }))
playNote _ _ = error "unexpected message type"

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

messageChannel :: Message -> Maybe Channel
messageChannel NoteOff{..} = Just channel
messageChannel NoteOn{..} = Just channel
messageChannel KeyPressure{..} = Just channel
messageChannel ControlChange{..} = Just channel
messageChannel ProgramChange{..} = Just channel
messageChannel ChannelPressure{..} = Just channel
messageChannel PitchWheel{..} = Just channel
messageChannel (ChannelPrefix channel) = Just channel
messageChannel _ = Nothing

messageKey :: Message -> Maybe Key
messageKey NoteOff{..} = Just key
messageKey NoteOn{..} = Just key
messageKey KeyPressure{..} = Just key
messageKey _ = Nothing

