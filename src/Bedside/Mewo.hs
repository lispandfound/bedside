-- | Mewo the cat: a pure state machine. The app holds the state and
-- turns emitted poses into widgets; randomness (picking a pose to
-- wander to) stays with the caller so this stays deterministic.
module Bedside.Mewo
  ( Pose (..),
    Mewo (..),
    Event (..),
    initial,
    step,
  )
where

data Pose = Sleeping | Desk | Floor
  deriving (Eq, Show, Enum, Bounded)

data Mewo = Mewo
  { pose :: Maybe Pose,
    asleep :: Bool
  }
  deriving (Eq, Show)

initial :: Mewo
initial = Mewo {pose = Nothing, asleep = False}

data Event
  = -- | The hourly nudge, with a pre-picked pose to wander to.
    Wander Pose
  | -- | 9pm: lie down and stop wandering.
    Bedtime
  | -- | 7am: start reacting to nudges again (no redraw by itself).
    WakeUp
  deriving (Eq, Show)

-- | Advance the state; a returned pose means "redraw Mewo like this".
step :: Event -> Mewo -> (Mewo, Maybe Pose)
step (Wander p) cat
  | asleep cat = (cat, Nothing)
  | otherwise = (cat {pose = Just p}, Just p)
step Bedtime cat
  | asleep cat = (cat, Nothing)
  | otherwise =
      ( Mewo {pose = Just Sleeping, asleep = True},
        -- No redraw needed if she already happened to be sleeping.
        if pose cat == Just Sleeping then Nothing else Just Sleeping
      )
step WakeUp cat = (cat {asleep = False}, Nothing)
