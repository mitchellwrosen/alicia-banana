{-# LANGUAGE LambdaCase, OverloadedStrings, PatternSynonyms, RecursiveDo,
             ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Concurrent
import Control.Exception (handle, throwIO)
import Control.Monad
import Data.Foldable
import Data.IntMap (IntMap)
import Data.Maybe
import Reactive.Banana
import Reactive.Banana.Frameworks


import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified SDL
import qualified SDL.Mixer
import qualified Termbox.Banana as Tb

main :: IO ()
main = do
  SDL.initialize [SDL.InitAudio]

  SDL.Mixer.withAudio audio 256 $ do
    SDL.Mixer.setChannels 16

    chunks :: IntMap SDL.Mixer.Chunk <- do
      traverse SDL.Mixer.load pianoFiles

    let
      notes :: IntMap (IO ())
      notes =
        IntMap.map (ignore . SDL.Mixer.play) chunks
       where
        ignore :: IO () -> IO ()
        ignore =
          handle $ \case
            NoFreeChannels -> pure ()
            e -> throwIO e

    let
      play :: Key -> IO ()
      play i =
        fromMaybe (pure ()) (IntMap.lookup (fromEnum i) notes)

    Tb.main (Tb.InputModeEsc Tb.MouseModeNo) Tb.OutputModeNormal (main' play)
    for_ chunks SDL.Mixer.free

  SDL.Mixer.quit

 where
  audio :: SDL.Mixer.Audio
  audio =
    SDL.Mixer.Audio
      { SDL.Mixer.audioFrequency = 11025 -- 22050
      , SDL.Mixer.audioFormat    = SDL.Mixer.FormatS16_Sys
      , SDL.Mixer.audioOutput    = SDL.Mixer.Stereo
      }

data InterfaceIn
  = InterfaceIn
      (Event Tb.Event)
      (Event PianoOutput)
      (Behavior PianoView)

data InterfaceOut
  = InterfaceOut
      (Event PianoInput)
      (Event ())
      (Behavior Tb.Scene)

data PianoOut
  = PianoOut
      (Event PianoOutput)
      (Behavior PianoView)

data PianoInput
  = Tick
  | Play Key
  deriving Eq

data PianoOutput
  = PianoOutput

type PianoView
  = Piano
  -- TODO Don't provide the whole piano, only a view

main'
  :: (Key -> IO ())
  -> Event Tb.Event
  -> Behavior (Int, Int)
  -> MomentIO (Behavior Tb.Scene, Event ())
main' play eEvent _bSize = mdo
  InterfaceOut ePianoInput eDone bScene <-
    makeInterface (InterfaceIn eEvent ePianoOutput bPianoView)
  PianoOut ePianoOutput bPianoView <-
    makePiano play ePianoInput
  pure (bScene, eDone)

makeInterface
  :: InterfaceIn
  -> MomentIO InterfaceOut
makeInterface (InterfaceIn eEvent _ePianoOutput bPianoView) = do
  eTick :: Event PianoInput <- do
    (eTick_, fireTick) <- newEvent
    (liftIO . void . forkIO . forever) $ do
      threadDelay (250*1000)
      fireTick Tick
    pure eTick_

  let
    eKeyLeft :: Event Tb.Event
    eKeyLeft =
      filterE (== Tb.EventKey Tb.KeyArrowLeft False) eEvent

  let
    eKeyRight :: Event Tb.Event
    eKeyRight =
      filterE (== Tb.EventKey Tb.KeyArrowRight False) eEvent

  let
    eKeyChar :: Event Char
    eKeyChar =
      filterJust
        ((\case
          Tb.EventKey (Tb.KeyChar c) _ -> Just c
          _ -> Nothing)
        <$> eEvent)

  let
    eDone :: Event ()
    eDone =
      () <$ filterE (== Tb.EventKey Tb.KeyEsc False) eEvent

  let
    eZip :: Event (Z a -> Z a)
    eZip =
      unions
        [ zleft  <$ eKeyLeft
        , zright <$ eKeyRight
        ]

  bKeyBindings :: Behavior KeyBindings <-
    (fmap.fmap) zextract $
      accumB (zfromList 13 [minBound..maxBound]) eZip

  let
    ePlay :: Event Key
    ePlay =
      filterJust (keyBindingsToKey <$> bKeyBindings <@> eKeyChar)

  let
    ePianoInput :: Event PianoInput
    ePianoInput =
      unionWith const (Play <$> ePlay) eTick

  let
    bScene :: Behavior Tb.Scene
    bScene =
      Tb.Scene
        <$> mconcat
              [ renderPiano <$> bPianoView
              , renderKeyBindings <$> bKeyBindings
              ]
        <*> pure Tb.NoCursor

  pure (InterfaceOut ePianoInput eDone bScene)

makePiano
  :: (Key -> IO ())
  -> Event PianoInput
  -> MomentIO PianoOut
makePiano play ePianoInput = do
  let
    eTick :: Event PianoInput
    eTick =
      filterE (== Tick) ePianoInput

  let
    ePlay :: Event Key
    ePlay =
      filterJust (f <$> ePianoInput)
     where
      f :: PianoInput -> Maybe Key
      f = \case
        Play key -> Just key
        Tick     -> Nothing

  let
    bKeyGen :: MonadMoment m => Key -> m (Behavior Int)
    bKeyGen k = mdo
      bKey :: Behavior Int <-
        stepper (0::Int)
          (unionWith const
            (2 <$ filterE (== k) ePlay)
            (filterJust
              ((\n ->
                if n > 0
                  then Just (n - 1)
                  else Nothing)
                <$> bKey
                <@ eTick)))
      pure bKey

  bA0  <- bKeyGen A0
  bAs0 <- bKeyGen As0
  bB0  <- bKeyGen B0
  bC1  <- bKeyGen C1
  bCs1 <- bKeyGen Cs1
  bD1  <- bKeyGen D1
  bDs1 <- bKeyGen Ds1
  bE1  <- bKeyGen E1
  bF1  <- bKeyGen F1
  bFs1 <- bKeyGen Fs1
  bG1  <- bKeyGen G1
  bGs1 <- bKeyGen Gs1
  bA1  <- bKeyGen A1
  bAs1 <- bKeyGen As1
  bB1  <- bKeyGen B1
  bC2  <- bKeyGen C2
  bCs2 <- bKeyGen Cs2
  bD2  <- bKeyGen D2
  bDs2 <- bKeyGen Ds2
  bE2  <- bKeyGen E2
  bF2  <- bKeyGen F2
  bFs2 <- bKeyGen Fs2
  bG2  <- bKeyGen G2
  bGs2 <- bKeyGen Gs2
  bA2  <- bKeyGen A2
  bAs2 <- bKeyGen As2
  bB2  <- bKeyGen B2
  bC3  <- bKeyGen C3
  bCs3 <- bKeyGen Cs3
  bD3  <- bKeyGen D3
  bDs3 <- bKeyGen Ds3
  bE3  <- bKeyGen E3
  bF3  <- bKeyGen F3
  bFs3 <- bKeyGen Fs3
  bG3  <- bKeyGen G3
  bGs3 <- bKeyGen Gs3
  bA3  <- bKeyGen A3
  bAs3 <- bKeyGen As3
  bB3  <- bKeyGen B3
  bC4  <- bKeyGen C4
  bCs4 <- bKeyGen Cs4
  bD4  <- bKeyGen D4
  bDs4 <- bKeyGen Ds4
  bE4  <- bKeyGen E4
  bF4  <- bKeyGen F4
  bFs4 <- bKeyGen Fs4
  bG4  <- bKeyGen G4
  bGs4 <- bKeyGen Gs4
  bA4  <- bKeyGen A4
  bAs4 <- bKeyGen As4
  bB4  <- bKeyGen B4
  bC5  <- bKeyGen C5
  bCs5 <- bKeyGen Cs5
  bD5  <- bKeyGen D5
  bDs5 <- bKeyGen Ds5
  bE5  <- bKeyGen E5
  bF5  <- bKeyGen F5
  bFs5 <- bKeyGen Fs5
  bG5  <- bKeyGen G5
  bGs5 <- bKeyGen Gs5
  bA5  <- bKeyGen A5
  bAs5 <- bKeyGen As5
  bB5  <- bKeyGen B5
  bC6  <- bKeyGen C6
  bCs6 <- bKeyGen Cs6
  bD6  <- bKeyGen D6
  bDs6 <- bKeyGen Ds6
  bE6  <- bKeyGen E6
  bF6  <- bKeyGen F6
  bFs6 <- bKeyGen Fs6
  bG6  <- bKeyGen G6
  bGs6 <- bKeyGen Gs6
  bA6  <- bKeyGen A6
  bAs6 <- bKeyGen As6
  bB6  <- bKeyGen B6
  bC7  <- bKeyGen C7
  bCs7 <- bKeyGen Cs7
  bD7  <- bKeyGen D7
  bDs7 <- bKeyGen Ds7
  bE7  <- bKeyGen E7
  bF7  <- bKeyGen F7
  bFs7 <- bKeyGen Fs7
  bG7  <- bKeyGen G7
  bGs7 <- bKeyGen Gs7
  bA7  <- bKeyGen A7
  bAs7 <- bKeyGen As7
  bB7  <- bKeyGen B7
  bC8  <- bKeyGen C8

  let
    bPiano :: Behavior Piano
    bPiano =
      Piano
        <$> bA0  <*> bAs0 <*> bB0  <*> bC1  <*> bCs1 <*> bD1  <*> bDs1 <*> bE1
        <*> bF1  <*> bFs1 <*> bG1  <*> bGs1 <*> bA1  <*> bAs1 <*> bB1  <*> bC2
        <*> bCs2 <*> bD2  <*> bDs2 <*> bE2  <*> bF2  <*> bFs2 <*> bG2  <*> bGs2
        <*> bA2  <*> bAs2 <*> bB2  <*> bC3  <*> bCs3 <*> bD3  <*> bDs3 <*> bE3
        <*> bF3  <*> bFs3 <*> bG3  <*> bGs3 <*> bA3  <*> bAs3 <*> bB3  <*> bC4
        <*> bCs4 <*> bD4  <*> bDs4 <*> bE4  <*> bF4  <*> bFs4 <*> bG4  <*> bGs4
        <*> bA4  <*> bAs4 <*> bB4  <*> bC5  <*> bCs5 <*> bD5  <*> bDs5 <*> bE5
        <*> bF5  <*> bFs5 <*> bG5  <*> bGs5 <*> bA5  <*> bAs5 <*> bB5  <*> bC6
        <*> bCs6 <*> bD6  <*> bDs6 <*> bE6  <*> bF6  <*> bFs6 <*> bG6  <*> bGs6
        <*> bA6  <*> bAs6 <*> bB6  <*> bC7  <*> bCs7 <*> bD7  <*> bDs7 <*> bE7
        <*> bF7  <*> bFs7 <*> bG7  <*> bGs7 <*> bA7  <*> bAs7 <*> bB7  <*> bC8

  reactimate (play <$> ePlay)

  pure (PianoOut never bPiano)

data Key
  = A0 | As0 | B0
  | C1 | Cs1 | D1 | Ds1 | E1 | F1 | Fs1 | G1 | Gs1 | A1 | As1 | B1
  | C2 | Cs2 | D2 | Ds2 | E2 | F2 | Fs2 | G2 | Gs2 | A2 | As2 | B2
  | C3 | Cs3 | D3 | Ds3 | E3 | F3 | Fs3 | G3 | Gs3 | A3 | As3 | B3
  | C4 | Cs4 | D4 | Ds4 | E4 | F4 | Fs4 | G4 | Gs4 | A4 | As4 | B4
  | C5 | Cs5 | D5 | Ds5 | E5 | F5 | Fs5 | G5 | Gs5 | A5 | As5 | B5
  | C6 | Cs6 | D6 | Ds6 | E6 | F6 | Fs6 | G6 | Gs6 | A6 | As6 | B6
  | C7 | Cs7 | D7 | Ds7 | E7 | F7 | Fs7 | G7 | Gs7 | A7 | As7 | B7
  | C8
  deriving (Bounded, Enum, Eq)

data KeyBindings
  = KeyBindingsA0
  | KeyBindingsAs0
  | KeyBindingsC1
  | KeyBindingsCs1
  | KeyBindingsDs1
  | KeyBindingsF1
  | KeyBindingsFs1
  | KeyBindingsGs1
  | KeyBindingsAs1
  | KeyBindingsC2
  | KeyBindingsCs2
  | KeyBindingsDs2
  | KeyBindingsF2
  | KeyBindingsFs2
  | KeyBindingsGs2
  | KeyBindingsAs2
  | KeyBindingsC3
  | KeyBindingsCs3
  | KeyBindingsDs3
  | KeyBindingsF3
  | KeyBindingsFs3
  | KeyBindingsGs3
  | KeyBindingsAs3
  | KeyBindingsC4
  | KeyBindingsCs4
  | KeyBindingsDs4
  | KeyBindingsF4
  | KeyBindingsFs4
  | KeyBindingsGs4
  | KeyBindingsAs4
  | KeyBindingsC5
  deriving (Bounded, Enum)

keyBindingsToKey :: KeyBindings -> Char -> Maybe Key
keyBindingsToKey = \case
  KeyBindingsA0  -> flip Map.lookup (Map.fromList (zip keysA  [A0  ..]))
  KeyBindingsAs0 -> flip Map.lookup (Map.fromList (zip keysAs [As0 ..]))
  KeyBindingsC1  -> flip Map.lookup (Map.fromList (zip keysC  [C1  ..]))
  KeyBindingsCs1 -> flip Map.lookup (Map.fromList (zip keysCs [Cs1 ..]))
  KeyBindingsDs1 -> flip Map.lookup (Map.fromList (zip keysDs [Ds1 ..]))
  KeyBindingsF1  -> flip Map.lookup (Map.fromList (zip keysF  [F1  ..]))
  KeyBindingsFs1 -> flip Map.lookup (Map.fromList (zip keysFs [Fs1 ..]))
  KeyBindingsGs1 -> flip Map.lookup (Map.fromList (zip keysGs [Gs1 ..]))
  KeyBindingsAs1 -> flip Map.lookup (Map.fromList (zip keysAs [As1 ..]))
  KeyBindingsC2  -> flip Map.lookup (Map.fromList (zip keysC  [C2  ..]))
  KeyBindingsCs2 -> flip Map.lookup (Map.fromList (zip keysCs [Cs2 ..]))
  KeyBindingsDs2 -> flip Map.lookup (Map.fromList (zip keysDs [Ds2 ..]))
  KeyBindingsF2  -> flip Map.lookup (Map.fromList (zip keysF  [F2  ..]))
  KeyBindingsFs2 -> flip Map.lookup (Map.fromList (zip keysFs [Fs2 ..]))
  KeyBindingsGs2 -> flip Map.lookup (Map.fromList (zip keysGs [Gs2 ..]))
  KeyBindingsAs2 -> flip Map.lookup (Map.fromList (zip keysAs [As2 ..]))
  KeyBindingsC3  -> flip Map.lookup (Map.fromList (zip keysC  [C3  ..]))
  KeyBindingsCs3 -> flip Map.lookup (Map.fromList (zip keysCs [Cs3 ..]))
  KeyBindingsDs3 -> flip Map.lookup (Map.fromList (zip keysDs [Ds3 ..]))
  KeyBindingsF3  -> flip Map.lookup (Map.fromList (zip keysF  [F3  ..]))
  KeyBindingsFs3 -> flip Map.lookup (Map.fromList (zip keysFs [Fs3 ..]))
  KeyBindingsGs3 -> flip Map.lookup (Map.fromList (zip keysGs [Gs3 ..]))
  KeyBindingsAs3 -> flip Map.lookup (Map.fromList (zip keysAs [As3 ..]))
  KeyBindingsC4  -> flip Map.lookup (Map.fromList (zip keysC  [C4  ..]))
  KeyBindingsCs4 -> flip Map.lookup (Map.fromList (zip keysCs [Cs4 ..]))
  KeyBindingsDs4 -> flip Map.lookup (Map.fromList (zip keysDs [Ds4 ..]))
  KeyBindingsF4  -> flip Map.lookup (Map.fromList (zip keysF  [F4  ..]))
  KeyBindingsFs4 -> flip Map.lookup (Map.fromList (zip keysFs [Fs4 ..]))
  KeyBindingsGs4 -> flip Map.lookup (Map.fromList (zip keysGs [Gs4 ..]))
  KeyBindingsAs4 -> flip Map.lookup (Map.fromList (zip keysAs [As4 ..]))
  KeyBindingsC5  -> flip Map.lookup (Map.fromList (zip keysC  [C5  ..]))
 where
  keysA  = "zsxcfvgbnjmk,l./'q2we4r5t6yu8i9op-[=]"
  keysAs = "azxdcfvbhnjmk,.;/'qw3e4r5ty7u8io0p-[=]"
  keysC  = "zsxdcvgbhnjm,l.;/q2w3e4rt6y7ui9o0p-[]"
  keysCs = "azsxcfvgbhnmk,l./'q2w3er5t6yu8i9o0p[=]"
  keysDs = "azxdcfvgbnjmk,.;/'q2we4r5ty7u8i9op-[=]"
  keysF  = "zsxdcfvbhnjm,l.;/'qw3e4rt6y7u8io0p-[]"
  keysFs = "azsxdcvgbhnmk,l.;/q2w3er5t6y7ui9o0p[=]"
  keysGs = "azsxcfvgbnjmk,l./'q2we4r5t6yu8i9op-[=]"

renderKeyBindings :: KeyBindings -> Tb.Cells
renderKeyBindings = \case
  KeyBindingsA0  -> keyBindingsA   0
  KeyBindingsAs0 -> keyBindingsAs  2
  KeyBindingsC1  -> keyBindingsC   5
  KeyBindingsCs1 -> keyBindingsCs  7
  KeyBindingsDs1 -> keyBindingsDs 10
  KeyBindingsF1  -> keyBindingsF  14
  KeyBindingsFs1 -> keyBindingsFs 16
  KeyBindingsGs1 -> keyBindingsGs 19
  KeyBindingsAs1 -> keyBindingsAs 23
  KeyBindingsC2  -> keyBindingsC  26
  KeyBindingsCs2 -> keyBindingsCs 28
  KeyBindingsDs2 -> keyBindingsDs 31
  KeyBindingsF2  -> keyBindingsF  35
  KeyBindingsFs2 -> keyBindingsFs 37
  KeyBindingsGs2 -> keyBindingsGs 40
  KeyBindingsAs2 -> keyBindingsAs 44
  KeyBindingsC3  -> keyBindingsC  47
  KeyBindingsCs3 -> keyBindingsCs 49
  KeyBindingsDs3 -> keyBindingsDs 52
  KeyBindingsF3  -> keyBindingsF  56
  KeyBindingsFs3 -> keyBindingsFs 58
  KeyBindingsGs3 -> keyBindingsGs 61
  KeyBindingsAs3 -> keyBindingsAs 65
  KeyBindingsC4  -> keyBindingsC  68
  KeyBindingsCs4 -> keyBindingsCs 70
  KeyBindingsDs4 -> keyBindingsDs 73
  KeyBindingsF4  -> keyBindingsF  77
  KeyBindingsFs4 -> keyBindingsFs 79
  KeyBindingsGs4 -> keyBindingsGs 82
  KeyBindingsAs4 -> keyBindingsAs 86
  KeyBindingsC5  -> keyBindingsC  89
 where
  keyBindingsA, keyBindingsAs, keyBindingsC, keyBindingsCs, keyBindingsDs,
    keyBindingsF :: Int -> Tb.Cells
  keyBindingsA  c = whites (c+1) <> blacksA  (c+3)
  keyBindingsAs c = whites (c+2) <> blacksAs (c+1)
  keyBindingsC  c = whites (c+2) <> blacksC  (c+4)
  keyBindingsCs c = whites (c+3) <> blacksCs (c+2)
  keyBindingsDs c = whites (c+3) <> blacksDs (c+2)
  keyBindingsF  c = whites (c+2) <> blacksF  (c+4)
  keyBindingsFs c = whites (c+3) <> blacksFs (c+2)
  keyBindingsGs c = whites (c+3) <> blacksGs (c+2)

  blacksA, blacksAs, blacksC, blacksCs :: Int -> Tb.Cells
  blacksA  = blacks [0,6,9,15,18,21,27,30,36,39,42,48,51,57,60]    "sfgjkl'245689-="
  blacksAs = blacks [0,6,9,15,18,21,27,30,36,39,42,48,51,57,60,63] "adfhjk;'345780-="
  blacksC  = blacks [0,3,9,12,15,21,24,30,33,36,42,45,51,54,57]    "sdghjl;2346790-"
  blacksCs = blacks [0,3,9,12,15,21,24,30,33,36,42,45,51,54,57,63] "asfghkl'2356890="
  blacksDs = blacks [0,6,9,12,18,21,27,30,33,39,42,48,51,54,60,63] "asfghkl'2356890="
  blacksF  = blacks [0,3,6,12,15,21,24,27,33,36,42,45,48,54,57]    "sdfhjl;'346780-"
  blacksFs = blacks [0,3,6,12,15,21,24,27,33,36,42,45,48,54,57,63] "asdghkl;2356790="
  blacksGs = blacks [0,3,9,12,18,21,24,30,33,39,42,45,51,54,60,63] "asfgjkl'245689-="

  whites :: Int -> Tb.Cells
  whites c0 =
    foldMap
      (white c0)
      (zip [0,3..] "zxcvbnm,./qwertyuiop[]")

  blacks :: [Int] -> [Char] -> Int -> Tb.Cells
  blacks xs ys c0 =
    foldMap (black c0) (zip xs ys)

  white :: Int -> (Int, Char) -> Tb.Cells
  white c0 (c, x) =
    Tb.set (c0+c) 8 (Tb.Cell x Tb.black Tb.white)

  black :: Int -> (Int, Char) -> Tb.Cells
  black c0 (c, x) =
    Tb.set (c0+c) 4 (Tb.Cell x Tb.white Tb.black)

data Piano
  = Piano !Int !Int !Int
          !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int
          !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int
          !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int
          !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int
          !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int
          !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int
          !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int
          !Int

renderPiano :: Piano -> Tb.Cells
renderPiano (Piano a0 as0 b0
                   c1 cs1 d1 ds1 e1 f1 fs1 g1 gs1 a1 as1 b1
                   c2 cs2 d2 ds2 e2 f2 fs2 g2 gs2 a2 as2 b2
                   c3 cs3 d3 ds3 e3 f3 fs3 g3 gs3 a3 as3 b3
                   c4 cs4 d4 ds4 e4 f4 fs4 g4 gs4 a4 as4 b4
                   c5 cs5 d5 ds5 e5 f5 fs5 g5 gs5 a5 as5 b5
                   c6 cs6 d6 ds6 e6 f6 fs6 g6 gs6 a6 as6 b6
                   c7 cs7 d7 ds7 e7 f7 fs7 g7 gs7 a7 as7 b7
                   c8) =

  let
    octave i c cs d ds e f fs g gs a as b =
      mconcat
        [ keyL i      r c
        , keyB (i+2)  r cs
        , keyU (i+3)  r d
        , keyB (i+5)  r ds
        , keyR (i+6)  r e
        , keyL (i+9)  r f
        , keyB (i+11) r fs
        , keyU (i+12) r g
        , keyB (i+14) r gs
        , keyU (i+15) r a
        , keyB (i+17) r as
        , keyR (i+18) r b
        ]
  in
    mconcat
      [ keyU 0 r a0, keyB 2 r as0, keyR 3 r b0
      , octave 6 c1 cs1 d1 ds1 e1 f1 fs1 g1 gs1 a1 as1 b1
      , octave 27 c2 cs2 d2 ds2 e2 f2 fs2 g2 gs2 a2 as2 b2
      , octave 48 c3 cs3 d3 ds3 e3 f3 fs3 g3 gs3 a3 as3 b3
      , octave 69 c4 cs4 d4 ds4 e4 f4 fs4 g4 gs4 a4 as4 b4
      , octave 90 c5 cs5 d5 ds5 e5 f5 fs5 g5 gs5 a5 as5 b5
      , octave 111 c6 cs6 d6 ds6 e6 f6 fs6 g6 gs6 a6 as6 b6
      , octave 132 c7 cs7 d7 ds7 e7 f7 fs7 g7 gs7 a7 as7 b7
      , keyL 153 r c8
      ]
 where
  r = 0

keyL :: Int -> Int -> Int -> Tb.Cells
keyL c r p =
  mconcat
    [ d c r,     d (c+1) r
    , d c (r+1), d (c+1) (r+1)
    , d c (r+2), d (c+1) (r+2)
    , d c (r+3), d (c+1) (r+3)
    , d c (r+4), d (c+1) (r+4)
    , d c (r+5), d (c+1) (r+5), d (c+2) (r+5)
    , d c (r+6), d (c+1) (r+6), d (c+2) (r+6)
    , d c (r+7), d (c+1) (r+7), d (c+2) (r+7)
    , d c (r+8), d (c+1) (r+8), d (c+2) (r+8)
    ]
 where
  d u v = Tb.set u v (Tb.Cell ' ' mempty (if p>0 then Tb.blue else Tb.white))

keyU :: Int -> Int -> Int -> Tb.Cells
keyU c r p =
  mconcat
    [            d (c+1) r
    ,            d (c+1) (r+1)
    ,            d (c+1) (r+2)
    ,            d (c+1) (r+3)
    ,            d (c+1) (r+4)
    , d c (r+5), d (c+1) (r+5), d (c+2) (r+5)
    , d c (r+6), d (c+1) (r+6), d (c+2) (r+6)
    , d c (r+7), d (c+1) (r+7), d (c+2) (r+7)
    , d c (r+8), d (c+1) (r+8), d (c+2) (r+8)
    ]
 where
  d u v = Tb.set u v (Tb.Cell ' ' mempty (if p>0 then Tb.blue else Tb.white))

keyR :: Int -> Int -> Int -> Tb.Cells
keyR c r p =
  mconcat
    [            d (c+1) r    , d (c+2) r
    ,            d (c+1) (r+1), d (c+2) (r+1)
    ,            d (c+1) (r+2), d (c+2) (r+2)
    ,            d (c+1) (r+3), d (c+2) (r+3)
    ,            d (c+1) (r+4), d (c+2) (r+4)
    , d c (r+5), d (c+1) (r+5), d (c+2) (r+5)
    , d c (r+6), d (c+1) (r+6), d (c+2) (r+6)
    , d c (r+7), d (c+1) (r+7), d (c+2) (r+7)
    , d c (r+8), d (c+1) (r+8), d (c+2) (r+8)
    ]
 where
  d u v = Tb.set u v (Tb.Cell ' ' mempty (if p>0 then Tb.blue else Tb.white))

keyB :: Int -> Int -> Int -> Tb.Cells
keyB c r p =
  mconcat
    [ d c r,     d (c+1) r
    , d c (r+1), d (c+1) (r+1)
    , d c (r+2), d (c+1) (r+2)
    , d c (r+3), d (c+1) (r+3)
    , d c (r+4), d (c+1) (r+4)
    ]
 where
  d v u = Tb.set v u x
  x = Tb.Cell ' ' mempty (if p>0 then Tb.red else Tb.black)

data Z a
  = Z [a] a [a]

zfromList :: Int -> [a] -> Z a
zfromList n = \case
  (x:xs) ->
    foldr (.) id (replicate n zright) (Z [] x xs)
  [] ->
    error "zfromList: empty list"

zextract :: Z a -> a
zextract = \case
  Z _ x _ -> x

zleft :: Z a -> Z a
zleft = \case
  Z (x:xs) y zs ->
    Z xs x (y:zs)
  z ->
    z

zright :: Z a -> Z a
zright = \case
  Z xs y (z:zs) ->
    Z (y:xs) z zs
  z ->
    z

pianoFiles :: IntMap FilePath
pianoFiles =
  IntMap.fromList
    [ ( 0, "samples/jobro_piano/39148__jobro__piano-ff-001.wav")
    , ( 1, "samples/jobro_piano/39149__jobro__piano-ff-002.wav")
    , ( 2, "samples/jobro_piano/39150__jobro__piano-ff-003.wav")
    , ( 3, "samples/jobro_piano/39151__jobro__piano-ff-004.wav")
    , ( 4, "samples/jobro_piano/39152__jobro__piano-ff-005.wav")
    , ( 5, "samples/jobro_piano/39153__jobro__piano-ff-006.wav")
    , ( 6, "samples/jobro_piano/39154__jobro__piano-ff-007.wav")
    , ( 7, "samples/jobro_piano/39155__jobro__piano-ff-008.wav")
    , ( 8, "samples/jobro_piano/39156__jobro__piano-ff-009.wav")
    , ( 9, "samples/jobro_piano/39157__jobro__piano-ff-010.wav")
    , (10, "samples/jobro_piano/39158__jobro__piano-ff-011.wav")
    , (11, "samples/jobro_piano/39159__jobro__piano-ff-012.wav")
    , (12, "samples/jobro_piano/39160__jobro__piano-ff-013.wav")
    , (13, "samples/jobro_piano/39161__jobro__piano-ff-014.wav")
    , (14, "samples/jobro_piano/39162__jobro__piano-ff-015.wav")
    , (15, "samples/jobro_piano/39163__jobro__piano-ff-016.wav")
    , (16, "samples/jobro_piano/39164__jobro__piano-ff-017.wav")
    , (17, "samples/jobro_piano/39165__jobro__piano-ff-018.wav")
    , (18, "samples/jobro_piano/39166__jobro__piano-ff-019.wav")
    , (19, "samples/jobro_piano/39167__jobro__piano-ff-020.wav")
    , (20, "samples/jobro_piano/39168__jobro__piano-ff-021.wav")
    , (21, "samples/jobro_piano/39169__jobro__piano-ff-022.wav")
    , (22, "samples/jobro_piano/39170__jobro__piano-ff-023.wav")
    , (23, "samples/jobro_piano/39171__jobro__piano-ff-024.wav")
    , (24, "samples/jobro_piano/39172__jobro__piano-ff-025.wav")
    , (25, "samples/jobro_piano/39173__jobro__piano-ff-026.wav")
    , (26, "samples/jobro_piano/39174__jobro__piano-ff-027.wav")
    , (27, "samples/jobro_piano/39175__jobro__piano-ff-028.wav")
    , (28, "samples/jobro_piano/39176__jobro__piano-ff-029.wav")
    , (29, "samples/jobro_piano/39177__jobro__piano-ff-030.wav")
    , (30, "samples/jobro_piano/39178__jobro__piano-ff-031.wav")
    , (31, "samples/jobro_piano/39179__jobro__piano-ff-032.wav")
    , (32, "samples/jobro_piano/39180__jobro__piano-ff-033.wav")
    , (33, "samples/jobro_piano/39181__jobro__piano-ff-034.wav")
    , (34, "samples/jobro_piano/39182__jobro__piano-ff-035.wav")
    , (35, "samples/jobro_piano/39183__jobro__piano-ff-036.wav")
    , (36, "samples/jobro_piano/39184__jobro__piano-ff-037.wav")
    , (37, "samples/jobro_piano/39185__jobro__piano-ff-038.wav")
    , (38, "samples/jobro_piano/39186__jobro__piano-ff-039.wav")
    , (39, "samples/jobro_piano/39187__jobro__piano-ff-040.wav")
    , (40, "samples/jobro_piano/39188__jobro__piano-ff-041.wav")
    , (41, "samples/jobro_piano/39189__jobro__piano-ff-042.wav")
    , (42, "samples/jobro_piano/39190__jobro__piano-ff-043.wav")
    , (43, "samples/jobro_piano/39191__jobro__piano-ff-044.wav")
    , (44, "samples/jobro_piano/39193__jobro__piano-ff-045.wav")
    , (45, "samples/jobro_piano/39194__jobro__piano-ff-046.wav")
    , (46, "samples/jobro_piano/39195__jobro__piano-ff-047.wav")
    , (47, "samples/jobro_piano/39196__jobro__piano-ff-048.wav")
    , (48, "samples/jobro_piano/39197__jobro__piano-ff-049.wav")
    , (49, "samples/jobro_piano/39198__jobro__piano-ff-050.wav")
    , (50, "samples/jobro_piano/39199__jobro__piano-ff-051.wav")
    , (51, "samples/jobro_piano/39200__jobro__piano-ff-052.wav")
    , (52, "samples/jobro_piano/39201__jobro__piano-ff-053.wav")
    , (53, "samples/jobro_piano/39202__jobro__piano-ff-054.wav")
    , (54, "samples/jobro_piano/39203__jobro__piano-ff-055.wav")
    , (55, "samples/jobro_piano/39204__jobro__piano-ff-056.wav")
    , (56, "samples/jobro_piano/39205__jobro__piano-ff-057.wav")
    , (57, "samples/jobro_piano/39206__jobro__piano-ff-058.wav")
    , (58, "samples/jobro_piano/39207__jobro__piano-ff-059.wav")
    , (59, "samples/jobro_piano/39208__jobro__piano-ff-060.wav")
    , (60, "samples/jobro_piano/39209__jobro__piano-ff-061.wav")
    , (61, "samples/jobro_piano/39210__jobro__piano-ff-062.wav")
    , (62, "samples/jobro_piano/39211__jobro__piano-ff-063.wav")
    , (63, "samples/jobro_piano/39212__jobro__piano-ff-064.wav")
    , (84, "samples/jobro_piano/39213__jobro__piano-ff-085.wav")
    , (85, "samples/jobro_piano/39214__jobro__piano-ff-086.wav")
    , (86, "samples/jobro_piano/39215__jobro__piano-ff-087.wav")
    , (87, "samples/jobro_piano/39216__jobro__piano-ff-088.wav")
    ]

pattern NoFreeChannels :: SDL.SDLException
pattern NoFreeChannels <- SDL.SDLCallFailed { SDL.sdlExceptionError = "No free channels available" }

instance Monoid a => Monoid (Behavior a) where
  mempty = pure mempty

instance Semigroup a => Semigroup (Behavior a) where
  (<>) = liftA2 (<>)
