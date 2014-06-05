(<$>)::  (a -> b) -> f a -> f b	--class Functor f where
(<*>)::f (a -> b) -> f a -> f b --class Functor f => Apply f where

(=<<)::(a -> f b) -> f a -> f b --class Apply f => Bind f where
(>>=)::f a -> (a -> f b) -> f b

lift2 ::
  Apply f => (a -> b -> c) -> f a -> f b -> f c
  
  
{-
Zippers
=======
* Changing some elements in a cons list (implemented as a singly linked list) is usually very inefficient (need to traverse to the element that needs to be updated, change it, and rebuild the list prior to the updated thing (the tail can be shared))
* Zippers allow the singly linked list to point in the other direction, so that the elements prior to the focus element are pointing in the reverse direction to normal)
  * e.g 1 :. 2 :. 3 :. __ :. 4 :. nil
        1 -> 2 -> 3 -> __ -> 4 -> nil (usually)
        1 <- 2 <- 3 <- __ -> 4 -> nil (as a zipper)
-}