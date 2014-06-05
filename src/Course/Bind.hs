{-# LANGUAGE NoImplicitPrelude #-}

module Course.Bind(
  Bind(..)
, (>>=)
, join
, (<=<)
) where

import Course.Core
import Course.Functor
import Course.Apply(Apply)
import Course.Id
import Course.List
import Course.Optional
import qualified Prelude as P

class Apply f => Bind f where
  (=<<) ::
    (a -> f b)
    -> f a
    -> f b

infixr 1 =<<

-- | Witness that all things with (=<<) and (<$>) also have (<*>).
--
-- >>> Id (+10) <*> Id 8
-- Id 18
--
-- >>> (+1) :. (*2) :. Nil <*> 1 :. 2 :. 3 :. Nil
-- [2,3,4,2,4,6]
--
-- >>> Full (+8) <*> Full 7
-- Full 15
--
-- >>> Empty <*> Full 7
-- Empty
--
-- >>> Full (+8) <*> Empty
-- Empty
--
-- >>> ((+) <*> (+10)) 3
-- 16
--
-- >>> ((+) <*> (+5)) 3
-- 11
--
-- >>> ((+) <*> (+5)) 1
-- 7
--
-- >>> ((*) <*> (+10)) 3
-- 39
--
-- >>> ((*) <*> (+2)) 3
-- 15
(<*>) ::
  Bind f =>
  f (a -> b)
  -> f a
  -> f b
(<*>) f a = 
  (flip (<$>) a) =<< f
  -- undefined =<< f
  
  -- (a -> b) -> f b
  
infixl 4 <*>

-- | Binds a function on the Id monad.
--
-- >>> (\x -> Id(x+1)) =<< Id 2
-- Id 3
instance Bind Id where
  --(=<<) :: (a -> f b) -> f a -> f b
  --         (a -> Id b) -> Id a -> Id b
  f =<< Id a = f a

-- | Binds a function on a List.
--
-- >>> (\n -> n :. n :. Nil) =<< (1 :. 2 :. 3 :. Nil)
-- [1,1,2,2,3,3]
instance Bind List where
  (=<<) = flatMap

-- | Binds a function on an Optional.
--
-- >>> (\n -> Full (n + n)) =<< Full 7
-- Full 14
instance Bind Optional where
  (=<<) = bindOptional

-- | Binds a function on the reader ((->) t).
--
-- >>> ((*) =<< (+10)) 7
-- 119
instance Bind ((->) t) where
  (=<<) f f' t = f (f' t) t

-- | Flattens a combined structure to a single structure.
--
-- >>> join ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil)
-- [1,2,3,1,2]
--
-- >>> join (Full Empty)
-- Empty
--
-- >>> join (Full (Full 7))
-- Full 7
--
-- >>> join (+) 7
-- 14
join ::
  Bind f =>
  f (f a)
  -> f a
join = (=<<) id

-- | Implement a flipped version of @(=<<)@, however, use only
-- @join@ and @(<$>)@.
(>>=) ::
  Bind f =>
  f a
  -> (a -> f b)
  -> f b
a >>= f = join (f <$> a)

infixl 1 >>=

-- | Implement composition within the @Bind@ environment.
(<=<) ::
  Bind f =>
  (b -> f c)
  -> (a -> f b)
  -> a
  -> f c
f <=< g = \a -> g a >>= f

infixr 1 <=<

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Bind IO where
  (=<<) =
    (P.=<<)

instance Bind [] where
  (=<<) =
    (P.=<<)

instance Bind P.Maybe where
  (=<<) =
    (P.=<<)
