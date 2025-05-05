-- Functor
-- Applicative

data Btree a = Empty | Node a (Btree a) (Btree a) deriving (Show)

leaf :: a -> Btree a
leaf a = Node a Empty Empty

instance Functor Btree where
  fmap _ Empty = Empty -- if there is no element on the right then ignore
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

instance Applicative Btree where
  pure a = Node a Empty Empty -- lift the element after pure to be of structure Btree
  (<*>) Empty _ = Empty -- if there is an empty node on the right for a corresponding left, just make it empty
  (<*>) _ Empty = Empty -- same as above but opposite
  (<*>) (Node la ll lr) (Node ra rl rr) = Node (la ra) (ll <*> rl) (lr <*> rr)

vtree = Node 3 (leaf 4) (leaf 5)

ftree = Node (* 2) (leaf (+ 1)) (leaf (+ 3))

-- ftree <*> vtree
-- Node 6 (leaf 5) (leaf 8)

-- pure (*2) <*> vtree
-- leaf 6

-- (*2) <$> vtree
-- Node 6 (leaf 8) (leaf 10)
