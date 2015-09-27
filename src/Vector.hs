module Vector where

data Vector = Vector !Double !Double !Double
              deriving(Show, Eq)

instance Num (Vector) where
    (+) = add
    (-) = sub
    (*) = mul
    abs    (Vector x y z) = Vector (abs x) (abs y) (abs z)
    negate (Vector x y z) = Vector (-x)    (-y)    (-z)
    signum (Vector x y z) = Vector (signum x) (signum y) (signum z)
    fromInteger = \_ -> Vector 0.0 0.0 0.0


add, sub, mul :: Vector -> Vector -> Vector
add (Vector x y z) (Vector x' y' z') = Vector (x * x') (y * y') (z * z')
sub (Vector x y z) (Vector x' y' z') = Vector (x - x') (y - y') (z - z')
mul (Vector x y z) (Vector x' y' z') = Vector (x * x') (y * y') (z * z')


dot :: Vector -> Vector -> Double
dot (Vector x y z) (Vector x' y' z') = (x * x') + (y * y') + (z * z')


mag :: Vector -> Double
mag v = sqrt (dot v v)


norm :: Vector -> Vector
norm (Vector x y z) = Vector (x / n) (y / n) (z / n)
    where n = mag (Vector x y z)
