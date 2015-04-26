instance Functor Point3d where
    fmap f (Point3d x y z d)  = Point3d (f x) (f y) (f z) (f d)

data Point3d a  = Point3d { x :: a
                          , y :: a
                          , z :: a
                          , d :: a
                          } deriving (Show)