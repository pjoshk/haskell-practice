-- B Tree

main :: IO ()
main = do
  print tree
  print (treeSize tree)
  print (treeSum tree)
  print (treeMap (*2) tree)

data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

leaf :: a -> BTree a
leaf x = Node x Empty Empty

tree :: BTree Int
tree = Node 1
  (Node 2
    (leaf 8)
    Empty)
  Empty


treeSize :: BTree a -> Int
treeSize Empty = 0
treeSize (Node a l r) = 1 + treeSize l + treeSize r

treeSum :: BTree Int -> Int
treeSum Empty = 0
treeSum (Node a l r) = a + treeSum l + treeSum r

treeMap :: (a -> b) -> BTree a -> BTree b
treeMap _ Empty = Empty
treeMap f (Node a l r) = Node (f a) (treeMap f l) (treeMap f r)
