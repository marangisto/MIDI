{-# LANGUAGE DeriveDataTypeable, RecordWildCards, OverloadedStrings, TupleSections #-}
module Main where

import System.Console.CmdArgs
import System.Directory
import Control.Monad.Extra
import Control.Monad
import Control.Arrow (second)
import Codec.Midi as MIDI
import Numeric (showHex)
import Data.List.Extra
import Data.List (sortOn)
import Data.Char (isAscii)
import Data.Monoid
import Data.Maybe
import qualified Data.Vector as Vector
import Data.Vector ((//), (!))

data Options = Options
    { port      :: FilePath
    , dir       :: Maybe FilePath
    , analyze   :: Bool
    , track     :: [Int]
    , files     :: [FilePath]
    } deriving (Show, Eq, Data, Typeable)

options :: Main.Options
options = Main.Options
    { port = def &= help "serial port"
    , dir = def &= help "working directory"
    , analyze = def &= help "analyze contents"
    , track = def &= help "select specific track(s)"
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
        putStrLn $ "track[" <> show i <> "]: " <> maybe "" (filter isAscii) (trackName xs)
        putStrLn $ "channels" <> show (catMaybes $ map fst $ separateChannels xs)
        forM_ (trackText xs) $ putStrLn . ("    "<>) . filter isAscii
        forM_ (copyright xs) $ putStrLn . ("    "<>) . filter isAscii
        forM_ (messageStats xs) $ \(c, n) -> putStrLn (unwords [ "   ", show c, show n ])
processMIDI Options{..} Midi{..}
    = forM_ (mergeTracks $ select track tracks)
    $ processTrack
    . differentiate
    . noteOnOff
    . integrate
    . toRealTime timeDiv

select :: [Int] -> [Track t] -> [Track t]
select [] = id
select xs = map snd . filter ((`elem` xs) . fst) . zip [0..]

processTrack :: Show t => Track t -> IO ()
processTrack xs = mapM_ putStrLn $ mapMaybe (uncurry translateMidi) xs

translateMidi :: Show t => t -> Message -> Maybe String
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

mergeTracks :: (Num t, Ord t) => [Track t] -> [Track t]
mergeTracks
    = (:[])
    . concatMap ungroup
    . differentiate
    . groupSort
    . sortOn fst
    . filter (keeper . snd)
    . concatMap integrate
    . zipWith remapChannel [1..]

integrate :: Num t => Track t -> Track t
integrate [] = []
integrate xs = scanl f (head xs) xs
    where f (tx, mx) (ty, my) = (tx + ty, my)

differentiate :: Num t => [(t, a)] -> [(t, a)]
differentiate xs@(x:_) = x : zipWith f (init xs) (tail xs)
    where f (tx, mxs) (ty, mys) = (ty - tx, mys)

ungroup :: Num t => (t, [Message]) -> [(t, Message)]
ungroup (t, (x:xs)) = (t, x) : map (0,) xs

keeper :: Message -> Bool
keeper NoteOn{} = True
keeper NoteOff{} = True
keeper _ = False

remapChannel :: Int -> Track t -> Track t
remapChannel i = map f
    where f (t, m@NoteOn{}) = (t, m { channel = i })
          f (t, m@NoteOff{}) = (t, m { channel = i })
          f (t, m) = (t, m)

separateKeys :: Track t -> [(Maybe Key, [(t, Message)])] -- use on integrated time!
separateKeys = groupSort . map (\(t, m) -> (messageKey m, (t, m)))

separateChannels :: Track t -> [(Maybe Channel, Track t)] -- use on integrated time!
separateChannels = groupSort . map (\(t, m) -> (messageChannel m, (t, m)))

data State
    = Idle Time
    | Playing Time Key
    deriving (Eq, Ord, Show)

type Chans = Vector.Vector State

mkChans :: Int -> Chans
mkChans n = Vector.replicate n (Idle 0)

isIdle :: State -> Bool
isIdle (Idle _) = True
isIdle _ = False

ticks :: State -> Time
ticks (Idle t) = t
ticks (Playing t _) = t

ckey :: State -> Key
ckey (Idle _) = error "no key on idle channel"
ckey (Playing _ k) = k

lru :: Chans -> Maybe Int
lru chans
    | ((_, c):_) <- sort ys = Just c
    | otherwise = Nothing
    where xs = Vector.toList $ Vector.findIndices isIdle chans
          ys = map (\c -> (ticks $ chans!c, c)) xs

lru' :: Chans -> (Int, Key)
lru' chans = let c = snd . head $ sort ys in (c, ckey $ chans!c)
    where xs = [0..Vector.length chans - 1]
          ys = map (\c -> (ticks $ chans!c, c)) xs

isPlaying :: Key -> State -> Bool
isPlaying key (Playing _ k) = k == key
isPlaying _ _ = False

noteOnOff :: Track Time -> Track Time -- use on integrated time!
noteOnOff = concat . snd . mapAccumL playNote (mkChans 8) . filter playable
    where playable (_, NoteOn{}) = True
          playable (_, NoteOff{}) = True
          playable _ = False

playNote :: Chans -> (Time, Message) -> (Chans, [(Time, Message)])
playNote chans (t, m@NoteOn{..})
    | velocity == 0
    = playNote chans (t, NoteOff{..})
    | Just c <- Vector.findIndex (isPlaying key) chans
    = ( chans
      , [ (t, NoteOff (c + 1) key 127)
        , (t, m { channel = c + 1 })
        ]
      )
    | Just c <- lru chans
    = ( chans // [ (c, Playing t key) ]
      , [ (t, m { channel = c + 1 })
        ]
      )
    | otherwise
    = let (c, k) = lru' chans in
        ( chans // [ (c, Playing t key) ]
        , [ (t, NoteOff (c + 1) k 127)
          , (t, m { channel = c + 1 })
          ]
        )
    -- | otherwise = error "out of channels"
playNote chans (t, m@NoteOff{..})
    | Just c <- Vector.findIndex (isPlaying key) chans
    = (chans // [(c, Idle t)], [(t, m { channel = c + 1 })])
    | otherwise = (chans, [])
playNote _ m = error $ "unexpected message type" <> show m

messageStats :: Track t -> [(Constructor, Int)]
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

trackName :: Track t -> Maybe String
trackName xs
    | (s:_) <- [ s | (_, TrackName s) <- xs ] = Just s
    | otherwise = Nothing

trackText :: Track t -> [String]
trackText xs = [ s |  (_, Text s) <- xs ]

copyright :: Track t -> [String]
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

