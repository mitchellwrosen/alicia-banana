{-# LANGUAGE LambdaCase, OverloadedStrings, PatternSynonyms, RecursiveDo,
             ScopedTypeVariables, TupleSections #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Concurrent
import Control.Exception (handle, throwIO)
import Control.Monad
import Data.Bifunctor (second)
import Data.Foldable
import Data.Function ((&), on)
import Data.IntMap (IntMap)
import Data.List (deleteBy)
import Data.Map (Map)
import Data.Maybe
import Reactive.Banana
import Reactive.Banana.Frameworks


import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified SDL
import qualified SDL.Mixer
import qualified Termbox.Banana as Tb

-- TODO Render pressed key better (same background in binding)

main :: IO ()
main = do
  SDL.initialize [SDL.InitAudio]

  SDL.Mixer.withAudio audio 256 $ do
    SDL.Mixer.setChannels 32

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
      play k =
        fromMaybe (pure ()) (IntMap.lookup (fromEnum k) notes)

    Tb.run $ \(Tb.Inputs _size eKey _eSize _eMouse) -> mdo
      InterfaceOut ePianoInput eDone bScene <-
        makeInterface (InterfaceIn eKey ePianoOutput bPianoView)
      PianoOut ePianoOutput bPianoView <-
        makePiano play ePianoInput
      pure (Tb.Outputs bScene eDone)

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
      (Event Tb.Key)
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

newtype PianoView
  = PianoView [Key]

makeInterface
  :: InterfaceIn
  -> MomentIO InterfaceOut
makeInterface (InterfaceIn eEvent _ePianoOutput bPianoView) = do
  eTick :: Event PianoInput <- do
    (eTick_, fireTick) <- newEvent
    (liftIO . void . forkIO . forever) $ do
      threadDelay (125*1000)
      fireTick Tick
    pure eTick_

  let
    eKeyChar :: Event Char
    eKeyChar =
      filterJust
        ((\case
          Tb.KeyChar c -> Just c
          _ -> Nothing)
        <$> eEvent)

  let
    eDone :: Event ()
    eDone =
      () <$ filterE (== Tb.KeyEsc) eEvent

  bKeyBindings :: Behavior KeyBindings <- do
    let
      eLeft  = filterE (== Tb.KeyArrowLeft) eEvent
      eRight = filterE (== Tb.KeyArrowRight) eEvent
      eDown  = filterE (== Tb.KeyArrowDown) eEvent
      eUp    = filterE (== Tb.KeyArrowUp) eEvent
    (fmap.fmap) zextract $
      accumB (iterate zright (zfromList [minBound..maxBound]) !! 13)
        (unions
          [ zleft                   <$ eLeft
          , zright                  <$ eRight
          , (!! 7) . iterate zleft  <$ eDown
          , (!! 7) . iterate zright <$ eUp
          ])

  let
    ePlay :: Event Key
    ePlay =
      filterJust (readKey <$> bKeyBindings <@> eKeyChar)

  let
    ePianoInput :: Event PianoInput
    ePianoInput =
      unionWith const (Play <$> ePlay) eTick

  let
    bScene :: Behavior Tb.Scene
    bScene =
      mconcat
        [ renderPiano <$> bPianoView
        , renderKeyBindings <$> bKeyBindings
        ]

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

  rec
    bPiano :: Behavior [(Key, Int)] <- do
      let
        eTicked :: Event [(Key, Int)]
        eTicked =
          filterJust (f <$> bPiano <@ eTick)
         where
          f :: [(Key, Int)] -> Maybe [(Key, Int)]
          f = \case
            [] ->
              Nothing
            ks ->
              (Just . map dec . filter pressed) ks
           where
            dec :: (Key, Int) -> (Key, Int)
            dec =
              second (subtract 1)

            pressed :: (Key, Int) -> Bool
            pressed =
              (> 1) . snd

      let
        ePlayed :: Event [(Key, Int)]
        ePlayed =
          f <$> bPiano <@> ePlay
         where
          f :: [(Key, Int)] -> Key -> [(Key, Int)]
          f ks k =
            (k, 2) : deleteBy ((==) `on` fst) (k, undefined) ks

      stepper [] (unionWith const eTicked ePlayed)

  let
    bPianoView :: Behavior PianoView
    bPianoView =
      PianoView . map fst <$> bPiano

  reactimate (play <$> ePlay)

  pure (PianoOut never bPianoView)

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
  deriving (Bounded, Enum, Eq, Ord)

keyOffset :: Key -> Int
keyOffset = \case
  A0  -> 0
  As0 -> 2
  B0  -> 3
  C1  -> 6
  Cs1 -> 8
  D1  -> 9
  Ds1 -> 11
  E1  -> 12
  F1  -> 15
  Fs1 -> 17
  G1  -> 18
  Gs1 -> 20
  A1  -> 21
  As1 -> 23
  B1  -> 24
  C2  -> 27
  Cs2 -> 29
  D2  -> 30
  Ds2 -> 32
  E2  -> 33
  F2  -> 36
  Fs2 -> 38
  G2  -> 39
  Gs2 -> 41
  A2  -> 42
  As2 -> 44
  B2  -> 45
  C3  -> 48
  Cs3 -> 50
  D3  -> 51
  Ds3 -> 53
  E3  -> 54
  F3  -> 57
  Fs3 -> 59
  G3  -> 60
  Gs3 -> 62
  A3  -> 63
  As3 -> 65
  B3  -> 66
  C4  -> 69
  Cs4 -> 71
  D4  -> 72
  Ds4 -> 74
  E4  -> 75
  F4  -> 78
  Fs4 -> 80
  G4  -> 81
  Gs4 -> 83
  A4  -> 84
  As4 -> 86
  B4  -> 87
  C5  -> 90
  Cs5 -> 92
  D5  -> 93
  Ds5 -> 95
  E5  -> 96
  F5  -> 99
  Fs5 -> 101
  G5  -> 102
  Gs5 -> 104
  A5  -> 105
  As5 -> 107
  B5  -> 108
  C6  -> 111
  Cs6 -> 113
  D6  -> 114
  Ds6 -> 116
  E6  -> 117
  F6  -> 120
  Fs6 -> 122
  G6  -> 123
  Gs6 -> 125
  A6  -> 126
  As6 -> 128
  B6  -> 129
  C7  -> 132
  Cs7 -> 134
  D7  -> 135
  Ds7 -> 137
  E7  -> 138
  F7  -> 141
  Fs7 -> 143
  G7  -> 144
  Gs7 -> 146
  A7  -> 147
  As7 -> 149
  B7  -> 150
  C8  -> 153

keyShape :: Key -> KeyShape
keyShape = \case
  A0  -> KeyShapeL
  As0 -> KeyShapeB
  B0  -> KeyShapeR
  C1  -> KeyShapeL
  Cs1 -> KeyShapeB
  D1  -> KeyShapeU
  Ds1 -> KeyShapeB
  E1  -> KeyShapeR
  F1  -> KeyShapeL
  Fs1 -> KeyShapeB
  G1  -> KeyShapeU
  Gs1 -> KeyShapeB
  A1  -> KeyShapeU
  As1 -> KeyShapeB
  B1  -> KeyShapeR
  C2  -> KeyShapeL
  Cs2 -> KeyShapeB
  D2  -> KeyShapeU
  Ds2 -> KeyShapeB
  E2  -> KeyShapeR
  F2  -> KeyShapeL
  Fs2 -> KeyShapeB
  G2  -> KeyShapeU
  Gs2 -> KeyShapeB
  A2  -> KeyShapeU
  As2 -> KeyShapeB
  B2  -> KeyShapeR
  C3  -> KeyShapeL
  Cs3 -> KeyShapeB
  D3  -> KeyShapeU
  Ds3 -> KeyShapeB
  E3  -> KeyShapeR
  F3  -> KeyShapeL
  Fs3 -> KeyShapeB
  G3  -> KeyShapeU
  Gs3 -> KeyShapeB
  A3  -> KeyShapeU
  As3 -> KeyShapeB
  B3  -> KeyShapeR
  C4  -> KeyShapeL
  Cs4 -> KeyShapeB
  D4  -> KeyShapeU
  Ds4 -> KeyShapeB
  E4  -> KeyShapeR
  F4  -> KeyShapeL
  Fs4 -> KeyShapeB
  G4  -> KeyShapeU
  Gs4 -> KeyShapeB
  A4  -> KeyShapeU
  As4 -> KeyShapeB
  B4  -> KeyShapeR
  C5  -> KeyShapeL
  Cs5 -> KeyShapeB
  D5  -> KeyShapeU
  Ds5 -> KeyShapeB
  E5  -> KeyShapeR
  F5  -> KeyShapeL
  Fs5 -> KeyShapeB
  G5  -> KeyShapeU
  Gs5 -> KeyShapeB
  A5  -> KeyShapeU
  As5 -> KeyShapeB
  B5  -> KeyShapeR
  C6  -> KeyShapeL
  Cs6 -> KeyShapeB
  D6  -> KeyShapeU
  Ds6 -> KeyShapeB
  E6  -> KeyShapeR
  F6  -> KeyShapeL
  Fs6 -> KeyShapeB
  G6  -> KeyShapeU
  Gs6 -> KeyShapeB
  A6  -> KeyShapeU
  As6 -> KeyShapeB
  B6  -> KeyShapeR
  C7  -> KeyShapeL
  Cs7 -> KeyShapeB
  D7  -> KeyShapeU
  Ds7 -> KeyShapeB
  E7  -> KeyShapeR
  F7  -> KeyShapeL
  Fs7 -> KeyShapeB
  G7  -> KeyShapeU
  Gs7 -> KeyShapeB
  A7  -> KeyShapeU
  As7 -> KeyShapeB
  B7  -> KeyShapeR
  C8  -> KeyShapeW

data KeyShape
  = KeyShapeB
  | KeyShapeL
  | KeyShapeR
  | KeyShapeU
  | KeyShapeW

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

readKey :: KeyBindings -> Char -> Maybe Key
readKey = \case
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

renderKeyBindings :: KeyBindings -> Tb.Scene
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
    keyBindingsF :: Int -> Tb.Scene
  keyBindingsA  c = whites (c+1) <> blacksA  (c+3)
  keyBindingsAs c = whites (c+2) <> blacksAs (c+1)
  keyBindingsC  c = whites (c+2) <> blacksC  (c+4)
  keyBindingsCs c = whites (c+3) <> blacksCs (c+2)
  keyBindingsDs c = whites (c+3) <> blacksDs (c+2)
  keyBindingsF  c = whites (c+2) <> blacksF  (c+4)
  keyBindingsFs c = whites (c+3) <> blacksFs (c+2)
  keyBindingsGs c = whites (c+3) <> blacksGs (c+2)

  blacksA, blacksAs, blacksC, blacksCs :: Int -> Tb.Scene
  blacksA  = blacks [0,6,9,15,18,21,27,30,36,39,42,48,51,57,60]    "sfgjkl'245689-="
  blacksAs = blacks [0,6,9,15,18,21,27,30,36,39,42,48,51,57,60,63] "adfhjk;'345780-="
  blacksC  = blacks [0,3,9,12,15,21,24,30,33,36,42,45,51,54,57]    "sdghjl;2346790-"
  blacksCs = blacks [0,3,9,12,15,21,24,30,33,36,42,45,51,54,57,63] "asfghkl'2356890="
  blacksDs = blacks [0,6,9,12,18,21,27,30,33,39,42,48,51,54,60,63] "asfghkl'2356890="
  blacksF  = blacks [0,3,6,12,15,21,24,27,33,36,42,45,48,54,57]    "sdfhjl;'346780-"
  blacksFs = blacks [0,3,6,12,15,21,24,27,33,36,42,45,48,54,57,63] "asdghkl;2356790="
  blacksGs = blacks [0,3,9,12,18,21,24,30,33,39,42,45,51,54,60,63] "asfgjkl'245689-="

  whites :: Int -> Tb.Scene
  whites c0 =
    foldMap
      (white c0)
      (zip [0,3..] "zxcvbnm,./qwertyuiop[]")

  blacks :: [Int] -> [Char] -> Int -> Tb.Scene
  blacks xs ys c0 =
    foldMap (black c0) (zip xs ys)

  white :: Int -> (Int, Char) -> Tb.Scene
  white c0 (c, x) =
    Tb.cell (Tb.Pos 9 (c0+c)) (Tb.char x & Tb.fg (Tb.gray 0) & Tb.bg Tb.white)

  black :: Int -> (Int, Char) -> Tb.Scene
  black c0 (c, x) =
    Tb.cell (Tb.Pos 5 (c0+c)) (Tb.char x & Tb.fg Tb.white & Tb.bg (Tb.gray 0))

renderPiano :: PianoView -> Tb.Scene
renderPiano (PianoView keys) =
  foldMap (\c -> Tb.cell (Tb.Pos 0 c) (Tb.char ' ' & Tb.bg Tb.red)) [0..155]
  <>
  foldMap (\k -> renderKey 0 1 (k, k `elem` keys)) [minBound..maxBound]
 where
  renderKey :: Int -> Int -> (Key, Bool) -> Tb.Scene
  renderKey c0 r0 =
    (fromMaybe mempty . flip Map.lookup m)
   where
    m :: Map (Key, Bool) Tb.Scene
    m =
      Map.fromList $ do
        key <- [minBound..maxBound]
        p   <- [False, True]
        pure ((key, p), renderKey_ (c0 + keyOffset key) r0 p (keyShape key))

-- TODO clean this function up
renderKey_ :: Int -> Int -> Bool -> KeyShape -> Tb.Scene
renderKey_ c r p = \case
  KeyShapeL ->
    let
      d u v =
        Tb.cell (Tb.Pos v u) (Tb.char ' ' & Tb.bg (if p then Tb.blue else Tb.white))
    in
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
  KeyShapeU ->
    let
      d u v =
        Tb.cell (Tb.Pos v u) (Tb.char ' ' & Tb.bg (if p then Tb.blue else Tb.white))
    in
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
  KeyShapeR ->
    let
      d u v =
        Tb.cell (Tb.Pos v u) (Tb.char ' ' & Tb.bg (if p then Tb.blue else Tb.white))
    in
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
  KeyShapeW ->
    let
      d u v =
        Tb.cell (Tb.Pos v u) (Tb.char ' ' & Tb.bg (if p then Tb.blue else Tb.white))
    in
      mconcat
        [ d c r,     d (c+1) r    , d (c+2) r
        , d c (r+1), d (c+1) (r+1), d (c+2) (r+1)
        , d c (r+2), d (c+1) (r+2), d (c+2) (r+2)
        , d c (r+3), d (c+1) (r+3), d (c+2) (r+3)
        , d c (r+4), d (c+1) (r+4), d (c+2) (r+4)
        , d c (r+5), d (c+1) (r+5), d (c+2) (r+5)
        , d c (r+6), d (c+1) (r+6), d (c+2) (r+6)
        , d c (r+7), d (c+1) (r+7), d (c+2) (r+7)
        , d c (r+8), d (c+1) (r+8), d (c+2) (r+8)
        ]
  KeyShapeB ->
    let
      d v u = Tb.cell (Tb.Pos u v) (Tb.char ' ' & Tb.bg (if p then Tb.red else Tb.gray 0))
    in
      mconcat
        [ d c r,     d (c+1) r
        , d c (r+1), d (c+1) (r+1)
        , d c (r+2), d (c+1) (r+2)
        , d c (r+3), d (c+1) (r+3)
        , d c (r+4), d (c+1) (r+4)
        ]

data Z a
  = Z [a] a [a]

zfromList :: [a] -> Z a
zfromList = \case
  (x:xs) ->
    Z [] x xs
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
