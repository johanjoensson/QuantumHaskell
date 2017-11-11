module MathLib.AutomaticDifferentiation where
import           Data.Functor
-------------------------------------------------------------------------------
-- Basic data stucture of differentiable objects                             --
-- Either a constant or a function followed by its derivative                --
-------------------------------------------------------------------------------
data Dif a=
    C a
    | D a (Dif a)

class Diff a where
    df :: a -> a

instance Diff Double where
    df _ = 0

instance Num a => Diff (Dif a) where
    df (C _)    = 0
    df (D _ x') = x'

instance Functor Dif where
    fmap f (C x)    = C (f x)
    fmap f (D x x') = D (f x) (fmap f x')

instance Num a => Num (Dif a) where
    fromInteger n = C (fromInteger n)
    C x + C y = C (x + y)
    C x + D y y' = D (x + y) y'
    D x x' + C y = D (x + y) x'
    D x x' + D y y' = D (x + y) (x' + y')
    negate = fmap negate
    C x * C y = C (x*y)
    C x * D y y' = D (x*y) (fmap (x*) y')
    D x x' * C y = D (x*y) (fmap (y*) x')
    a@(D x x') * b@(D y y') = D (x*y) (x'*b + a*y')
    abs = fmap abs
    signum  = fmap signum

instance (Eq a,Fractional a) => Fractional (Dif a) where
    fromRational a = C (fromRational a)
    recip (C x)      = C (recip x)
    recip a@(D x x') = D (recip x) (negate x' * recip (a * a))
    C x / C y = C (x/y)
    x / C y = fmap (/y) x
    C x / y = fmap (x*) (recip y)
    a@(D x x')/b@(D y y')
     |x == 0, y == 0 = x'/y'
     |otherwise= D (x/y) ((x'*b -a*y')/b*b)

instance (Eq a,Floating a) => Floating (Dif a) where
    pi = C pi

    exp (C x)      = C (exp x)
    exp a@(D x x') = D (exp x) (x' * exp a)

    log (C x)      = C (log x)
    log a@(D x x') = D (log x) (x'/a)

    sin (C x)      = C (sin x)
    sin a@(D x x') = D (sin x) (x' * cos a)

    cos (C x)      = C (cos x)
    cos a@(D x x') = D (cos x) (negate x' * sin a)

    sinh (C x)      = C (sinh x)
    sinh a@(D x x') = D (sinh x) (x' * cosh a)

    cosh (C x)      = C (cosh x)
    cosh a@(D x x') = D (cosh x) (x' * sinh a)

    asin (C x)      = C (asin x)
    asin a@(D x x') = D (asin x) (x'/sqrt (1 - a*a))

    acos (C x)      = C (acos x)
    acos a@(D x x') = D (acos x) (negate x'/sqrt (1 - a*a))

    atan (C x)      = C (atan x)
    atan a@(D x x') = D (atan x) (x'/(1 + a*a))

    asinh (C x)      = C (asinh x)
    asinh a@(D x x') = D (asinh x) (x'/sqrt (a*a + 1))

    acosh (C x)      = C (acosh x)
    acosh a@(D x x') = D (acosh x) (x'/sqrt (a*a - 1))

    atanh (C x)      = C (atanh x)
    atanh a@(D x x') = D (atanh x) (x'/(1 - a*a))

    sqrt (C x)      = C (sqrt x)
    sqrt a@(D x x') = D (sqrt x) (0.5*x'/sqrt a)

instance (Num a, Eq a) => Eq (Dif a) where
    C x == C y = x == y
    C x == D y y' = x == y && y' == C 0
    D x x' == C y = x == y && x' == C 0
    D x x' == D y y' = x == y && x' == y'

instance (Num a, Eq a, Ord a) => Ord (Dif a) where
    compare (C x) (C y)       = compare x y
    compare (C x) (D y y')    = compare x y
    compare (D x x') (C y)    = compare x y
    compare (D x x') (D y y') = compare x y
