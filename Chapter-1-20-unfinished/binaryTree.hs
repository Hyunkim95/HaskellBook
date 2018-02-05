data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay = if mapTree (+1) testTree' == mapExpected then print "yup okay!" else error "test failed!"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = x ++ y ++ z
  where
    x = [a]
    y = preorder left
    z = preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = x ++ y ++ z
  where
    x = preorder left
    y = [a]
    z = preorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = x ++ y ++ z
  where
    x = postorder left
    y = postorder right
    z = [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree === [2,1,3]
  then putStrLn "Preorder fine"
  else putStrLn "Bad news bears"

testInorder :: IO ()
testInorder =
  if inorder testTree == [1,2,3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

testPostorder = IO ()
testPostorder =
    if postorder testTree == [1,3,2]
    then putStrLn "Postorder fine!"
    else putStrLn "postorder failed check"

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldtree _ z Leaf = z
foldTree f z (Node left a right) = bothFolded
  where
    leftFolded = f x (foldTree f z left)
    bothFolded = foldTree f leftFolded right
