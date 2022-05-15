-- file: ch03/ListADT.hs

data List a = Cons a (List a)
            | Empty
              deriving (Show)

fromFakeList (Cons x xs) = x:(fromFakeList xs)
fromFakeList Empty = []

fakeList = Cons 1 (Cons 2 (Cons 3 Empty))

data Tree a = Tree a (Maybe (Tree a)) (Maybe (Tree a))
              deriving (Show)

fakeTree = Tree 1 (Nothing) (Nothing)

fakeTreeTwo = Tree 1
              (Just (Tree 2 (Nothing) (Nothing)))
              (Just (Tree 3 (Nothing) (Nothing)))

data RealTree a = RealTree a (RealTree a) (RealTree a)
                | Nada
                  deriving (Show)

realTree = RealTree 1 (RealTree 2 (Nada) (Nada)) (Nada)
