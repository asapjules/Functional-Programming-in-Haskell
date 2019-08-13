module Lab08 where

import Lab08Def

-- Implement the following methods for Forky. See the PDF for what to aim for.

instance Functor Forky where
    -- fmap :: (a -> b) -> Forky a -> Forky b
    fmap op (Tip a) = (Tip (op a))
    fmap op (Branch a b) = Branch (fmap op a) (fmap op b) 

instance Applicative Forky where
    -- pure :: a -> Forky a
    pure val = (Tip val)
    -- (<*>) :: Forky (a -> b) -> Forky a -> Forky b
    (<*>) (Tip op1) a = fmap op1 a
    (<*>) (Branch op1 op2) a = Branch ((<*>) op1 a) ((<*>) op2 a)