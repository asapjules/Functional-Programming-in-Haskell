module Lab09 where

import Lab09Def

instance Functor Forky where
    fmap f ta = ta >>= \a -> return (f a)

instance Applicative Forky where
    pure a = return a
    tf <*> ta = tf >>= \f -> ta >>= \a -> pure (f a)

-- Implement Monad's (>>=) for Forky. See the PDF for what to aim for.

instance Monad Forky where
    -- return :: a -> Forky a
    return a = Tip a
    -- (>>=) :: Forky a -> (a -> Forky b) -> Forky b
    (>>=) (Tip a) func = ((func a))
    (>>=) (Branch a b) func = (Branch ((>>=) a func) ((>>=) b func))
