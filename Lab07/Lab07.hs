module Lab07 where

import Lab07Def

{- [6 marks]

   Help me make Weight an instance of Num so I can do basic arithmetic.
   No need to do (-), default implementation uses (+) and negate.
-}
instance Num Weight where
    -- (+) :: Weight -> Weight -> Weight
    -- Corner case: anything + Inf = Inf.  The other order too.
    (+) (Fin i) Inf = Inf
    (+) Inf (Fin i) = Inf
    (+) (Fin n) (Fin i) = Fin (i + n)
    (+) Inf Inf = Inf

    -- negate :: Weight -> Weight
    -- Corner case done because unsupported:
    negate (Fin i) = (Fin (Prelude.negate i)) 
    negate Inf = error "negative infinity not supported"

    -- (*) :: Weight -> Weight -> Weight
    -- Corner cases:
    -- positive * Inf = Inf
    -- (zero or negative) * Inf is unsupported
    -- Don't forget the other order.
    (*) (Fin i) (Fin n) = Fin (i*n)
    (*) Inf Inf = Inf
    (*) (Fin i) Inf
      | i < 0 = error "Negative * Inf not supported"
      | i >= 0= Inf
    (*) Inf (Fin i)
      | i < 0 = error "Negative * Inf not supported"
      | i >= 0 = Inf

    --abs :: Weight -> Weight
    abs (Fin i) = (Fin(abs i))
    abs Inf = Inf
    --signum :: Weight -> Weight
    -- Corner case: signum Inf is positive one.
    signum Inf = (Fin 1)
    signum (Fin i)
        | i == 0 = 0
        | i < 0 = -1
        | i > 0 = 1
    
    -- fromInteger :: Integer -> Weight
    fromInteger i = (Fin i)