{-# LANGUAGE MagicHash,
             NamedFieldPuns
             #-}

module InPatternField where

-- NOTE: non-exhaustive

data Rec = Rec { x# :: Int }    {-* MagicHash *-}

f5 :: Rec -> ()
f5 Rec{x# = 0} = ()             {-* MagicHash *-}
f5 Rec{x#}     = ()             {-* MagicHash *-} --RecordPuns
