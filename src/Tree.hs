module Tree where

{-- snippet Tree --}
data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a))

{-- /snippet Tree --}

{-- snippet simpleTree --}
simpleTree = Node "parent" (Just (Node "left child" Nothing Nothing))
                           (Just (Node "right child" Nothing Nothing))
{-- /snippet simpleTree --}
