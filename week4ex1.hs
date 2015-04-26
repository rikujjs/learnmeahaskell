instance Functor ThreeTuple where
    fmap f (ThreeTuple x y z d)  = ThreeTuple (f x) (f y) (f z) (f d)

data ThreeTuple a  = ThreeTuple { x :: a
                          , y :: a
                          , z :: a
                          , d :: a
                          } deriving (Show)
