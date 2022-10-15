-- | A zipper with O(1) access to the cursor. All functions return Nothing when
-- they do not modify the zipper.

{-# LANGUAGE DeriveFunctor #-}

module Data.List.Zipper where

import Prelude hiding (length, reverse)
import Prelude qualified

data Zipper a = Zip ![a] ![a] deriving (Eq, Functor, Show)

empty :: Zipper a
empty = Zip [] []

fromList :: [a] -> Zipper a
fromList xs = Zip [] xs

before :: Zipper a -> [a]
before (Zip ls _) = Prelude.reverse ls

current :: Zipper a -> Maybe a
current (Zip _ []) = Nothing
current (Zip _ (r:rs)) = Just r

after :: Zipper a -> [a]
after (Zip _ []) = []
after (Zip _ (r:rs)) = rs

toList :: Zipper a -> [a]
toList (Zip ls rs) = Prelude.reverse ls ++ rs

insert :: a -> Zipper a -> Zipper a
insert x (Zip ls rs) = Zip ls (x:rs)

adjust :: (a -> a) -> Zipper a -> Maybe (Zipper a)
adjust f (Zip _ []) = Nothing
adjust f (Zip ls (r:rs)) = Just $ Zip ls (f r:rs)

delete :: Zipper a -> Maybe (Zipper a)
delete (Zip _ []) = Nothing
delete (Zip ls (r:rs)) = Just $ Zip ls rs

pop :: Zipper a -> Maybe (a, Zipper a)
pop (Zip _ []) = Nothing
pop (Zip ls (r:rs)) = Just (r, Zip ls rs)

toStart :: Zipper a -> Maybe (Zipper a)
toStart (Zip [] _) = Nothing
toStart (Zip ls rs) = Just $ Zip [] (Prelude.reverse ls ++ rs)

toEnd :: Zipper a -> Maybe (Zipper a)
toEnd (Zip _ []) = Nothing
toEnd (Zip ls rs) = Just $ Zip (ls ++ Prelude.reverse rs) []

toPrev :: Zipper a -> Maybe (Zipper a)
toPrev (Zip [] _) = Nothing
toPrev (Zip (x:ls) rs) = Just $ Zip ls (x:rs)

toNext :: Zipper a -> Maybe (Zipper a)
toNext (Zip _ []) = Nothing
toNext (Zip ls (x:rs)) = Just $ Zip (x:ls) rs

reverse :: Zipper a -> Maybe (Zipper a)
reverse (Zip [] []) = Nothing
reverse (Zip ls []) = Just $ Zip [] ls
reverse (Zip ls (x:rs)) = Just $ Zip rs (x:ls)

length :: Zipper a -> Int
length (Zip ls rs) = Prelude.length ls + Prelude.length rs
