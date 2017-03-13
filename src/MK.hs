{-# LANGUAGE DeriveFunctor #-}
module MK where

data BotList a = Empty | Delay (BotList a) | Choice a (BotList a) deriving Functor

instance Monoid (BotList a) where
  mempty = Empty
  Empty `mappend` y = y
  Delay x `mappend` y = Delay $ x `mappend` y
  Choice a f `mappend` y = Choice a (y `mappend` f)

instance Applicative BotList where
  pure = flip Choice Empty
  Empty <*> _ = Empty
  Delay f <*> x = Delay $ f <*> x
  Choice a f <*> x = fmap a x `mappend` (f <*> x)

instance Monad BotList where
  Empty >>= _ = Empty
  Delay x >>= f = Delay (x >>= f)
  Choice a r >>= f = f a `mappend` (r >>= f)

class MK r where
