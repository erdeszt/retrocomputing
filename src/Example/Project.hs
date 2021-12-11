module Example.Project (topEntity, plus) where

import Clash.Annotations.TH
import Clash.Prelude

data Polarity = High | Low

newtype Active (p :: Polarity) = MkActive {activeLevel :: Bit}
  deriving (Show, Eq, Ord, Generic, NFDataX, BitPack)

active :: Bit -> Active p
active = MkActive

class IsActive p where
  fromActive :: Active p -> Bool
  toActive :: Bool -> Active p

instance IsActive High where
  fromActive = bitToBool . activeLevel
  toActive = MkActive . boolToBit

instance IsActive Low where
  fromActive = bitToBool . complement . activeLevel
  toActive = MkActive . complement . boolToBit

-- | Add two numbers. Example:
--
-- >>> plus 3 5
-- 8
plus :: Signed 8 -> Signed 8 -> Signed 8
plus a b = a + b

-- | 'topEntity' is Clash's equivalent of 'main' in other programming
-- languages. Clash will look for it when compiling 'Example.Project'
-- and translate it to HDL. While polymorphism can be used freely in
-- Clash projects, a 'topEntity' must be monomorphic and must use non-
-- recursive types. Or, to put it hand-wavily, a 'topEntity' must be
-- translatable to a static number of wires.
-- topEntity :: Signed 8 -> Signed 8 -> Signed 8
-- topEntity = plus
topEntity ::
  "SWITCHES" ::: Signal System (Vec 8 Bit) ->
  "LEDS" ::: Signal System (Vec 8 Bit)
topEntity switches = reverse <$> switches

-- makeTopEntity 'topEntity

{-
topEntity ::
  "BTN"
    ::: ( "1" ::: Signal System Bit,
          "2" ::: Signal System Bit
        ) ->
  "LED"
    ::: ( "1" ::: Signal System Bit,
          "2" ::: Signal System Bit
        )
topEntity (btn1, btn2) = (both, either)
  where
    both = (.&.) <$> btn1 <*> btn2
    either = (.|.) <$> btn1 <*> btn2

-}
