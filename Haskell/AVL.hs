module AVL where

--AVLTree structure
--NAVL is the NULL AVL Tree
data AVLTree b a = NAVL | Node {value :: a 
                                ,leftSonHeigth :: b 
                                ,rightSonHeigth :: b 
                                ,leftSon :: (AVLTree b a) 
                                ,rightSon :: (AVLTree b a)} 
                                deriving (Eq)

--Inorder show instance
instance (Show a, Show b) => Show (AVLTree b a) where
    show NAVL = ""
    show (Node v _ _ lst rst) = (show lst) ++ " " ++ (show v) ++ " " ++ (show rst)

--ALTERNATIVE SHOWS

--Inorder
inorder :: (Show a, Show b) => (AVLTree b a) -> String   
inorder = show 

--Preorder
preorder :: (Show a, Show b) => (AVLTree b a) -> String
preorder NAVL = ""
preorder (Node v _ _ lst rst) = (show v) ++ " " ++ (preorder lst) ++ " " ++ (preorder rst)

--Postorder
postorder :: (Show a, Show b) => (AVLTree b a) -> String
postorder NAVL = ""
postorder (Node v _ _ lst rst) = (postorder lst) ++ " " ++ (postorder rst) ++ " " ++ (show v)

--Debug Inorder
debugInorder :: (Show a, Integral b, Show b) => (AVLTree b a) -> String
debugInorder NAVL = ""
debugInorder avltree@(Node v lstH rstH lst rst) = 
    (debugInorder lst) ++ " " ++ (show ((,) v (balanceFactor avltree))) ++ " " ++ (debugInorder rst)

--The local heigth of a tree is his higher subtree plus 1
heigthAVL :: (Integral b) => (AVLTree b a) -> b
heigthAVL NAVL = 0
heigthAVL (Node _ lstH rstH _ _) = (1+) $ max lstH rstH

--The balance factor of a tree is the 
--heigth of his rigth subtree minus the heigth of the left subtree
balanceFactor :: (Integral b) => (AVLTree b a) -> b
balanceFactor NAVL = 0
balanceFactor (Node _ lstH rstH _ _) = rstH - lstH

--Insertion operator
infixr 5 >:
(>:) :: (Ord a, Eq a, Integral b) => a -> AVLTree b a -> AVLTree b a
x >: avltree = insertAVL x avltree 

--Insert an element into the tree
insertAVL :: (Eq a, Ord a, Integral b) => a -> (AVLTree b a) -> (AVLTree b a)
insertAVL x NAVL = Node x 0 0 NAVL NAVL
insertAVL x (Node v lstH rstH lst rst)
    | x <= v = let 
        newlst = (insertAVL x lst)
        newlstH = heigthAVL newlst
        in balanceAVL (Node v newlstH rstH newlst rst)      
    | x > v  = let
        newrst = (insertAVL x rst)
        newrstH = heigthAVL newrst
        in balanceAVL (Node v lstH newrstH lst newrst) 

--Identify the rotation function to apply
balanceAVL :: (Eq a, Ord a, Integral b) => (AVLTree b a) -> (AVLTree b a)
balanceAVL NAVL = NAVL
balanceAVL avltree
    | balanceFactor avltree == (-2) = rightRotationSelection avltree
    | balanceFactor avltree == 2  = leftRotationSelection avltree
    | otherwise = avltree

--Choose between right rotation or left-right rotation
rightRotationSelection :: (Eq a, Ord a, Integral b) => (AVLTree b a) -> (AVLTree b a)
rightRotationSelection avltree@(Node v lstH rstH lst rst) 
    | balanceFactor lst == (-1) = rightRotation avltree
    | balanceFactor lst == 1 = leftRightRotation avltree

--Choose between left rotation or right-left rotation
leftRotationSelection :: (Eq a, Ord a, Integral b) => (AVLTree b a) -> (AVLTree b a)
leftRotationSelection avltree@(Node v lstH rstH lst rst) 
    | balanceFactor rst == 1 = leftRotation avltree
    | balanceFactor rst == (-1) = rightLeftRotation avltree

--ROTATIONS

--Right rotation
rightRotation :: (Eq a, Ord a, Integral b) => (AVLTree b a) -> (AVLTree b a)
rightRotation (Node v lstH rstH lst rst) = 
    (Node leftRoot t1H newrstH t1 newrst)
    where
        root = v
        leftRoot = value lst
        t1 = leftSon lst
        t1H = heigthAVL t1
        t2 = rightSon lst
        t2H = heigthAVL t2
        t3 = rst
        t3H = rstH
        newrst = (Node root t2H t3H t2 t3) 
        newrstH = heigthAVL newrst

--Left rotation
leftRotation :: (Eq a, Ord a, Integral b) => (AVLTree b a) -> (AVLTree b a)
leftRotation (Node v lstH rstH lst rst) = 
    (Node rightRoot newlstH t1H newlst t1)
    where
        root = v
        rightRoot = value rst
        t1 = rightSon rst
        t1H = heigthAVL t1
        t2 = leftSon rst
        t2H = heigthAVL t2
        t3 = lst
        t3H = lstH
        newlst = (Node root t3H t2H t3 t2) 
        newlstH = heigthAVL newlst

--Left-Right rotation
leftRightRotation :: (Eq a, Ord a, Integral b) => (AVLTree b a) -> (AVLTree b a)
leftRightRotation (Node v lstH rstH lst rst) = 
    rightRotation (Node v rotatedlstH rstH rotatedlst rst) where
        rotatedlst = leftRotation lst
        rotatedlstH = heigthAVL rotatedlst
    
--Right-Left rotation
rightLeftRotation :: (Eq a, Ord a, Integral b) => (AVLTree b a) -> (AVLTree b a)
rightLeftRotation (Node v lstH rstH lst rst) = 
    leftRotation (Node v lstH rotatedrstH lst rotatedrst) where
        rotatedrst = rightRotation rst
        rotatedrstH = heigthAVL rotatedrst

--Make an AVL from a list(last element is first insert)
fromListAVL :: (Eq a, Ord a, Integral b) => [a] -> (AVLTree b a)
fromListAVL [] = NAVL
fromListAVL (x:xs) = x >: (fromListAVL xs)

--A example tree
aTree = 1 >: 5 >: 2 >: 0 >: 4 >:3 >: 7 >: 5 >: 9 >:  8 >: 6 >: 0 >: NAVL