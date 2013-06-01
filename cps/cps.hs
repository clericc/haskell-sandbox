import Control.Monad.Cont
import Data.Functor.Identity (Identity)
-- newtype ContT r m a = ContT {runContT :: (a -> m r) -> m r}
-- type Cont r = ContT r Identity
-- ContT :: ((a -> m r) -> m r) -> ContT r m a
-- runCont :: Cont r a -> (a -> r) -> r
-- runContT :: ContT r m a -> (a -> m r) -> m r

-- instance Monad (Cont r) where
--   return n = Cont (\k -> k n)
--   m >>= f  = Cont (\k -> runCont m (\a -> runCont (f a) k))

foldl_cps :: (a -> b -> (a -> r) -> r) -> a -> [b] -> (a -> r) -> r
foldl_cps _ acc [] c = c acc
foldl_cps f acc (x:xs) c = f acc x (\e -> foldl_cps f e xs c)

--{-

foldl_cps1 :: (a -> b -> Cont r a) -> a -> [b] -> Cont r a
foldl_cps1 _ acc [] = return acc
foldl_cps1 f acc (x:xs) = f acc x >>= (\e -> foldl_cps1 f e xs) >>= return

foldl_cps2 :: (a -> b -> Cont r a) -> a -> [b] -> Cont r a
foldl_cps2 _ acc [] = return acc
foldl_cps2 f acc (x:xs) = cont $ \k -> runCont (f acc x) (\e -> runCont (foldl_cps2 f e xs) k)

foldl_cps3 :: (a -> b -> Cont r a) -> a -> [b] -> Cont r a
foldl_cps3 _ acc [] = return acc
foldl_cps3 f acc (x:xs) = do
  e <- f acc x
  res <- foldl_cps3 f e xs
  return res
  


--}

-- add_cps :: Num a => a -> a -> Cont r a
add_cps x y = return (x+y)

test1 = runCont (add_cps 5 11) id


qsort_cps :: Ord a => [a] -> ([a] -> r) -> r
qsort_cps [] c = c []
qsort_cps (x:xs) c = foldl_cps foldfunc ([],[]) xs
	$ \(lessX,greaterX) -> qsort_cps lessX
	$ \lsorted -> qsort_cps greaterX
	$ \rsorted -> c $ lsorted ++ (x:rsorted)
  	where
    foldfunc (l,r) e cont = cont $ if e < x then (e:l,r) else (l,e:r)


concat_cps :: [a] -> [a] -> ([a] -> r) -> r
concat_cps [] xs c = c xs
concat_cps [e] xs c = c (e:xs)
concat_cps (_:xs) l2 c = last_cps xs $ \e -> concat_cps xs (e:l2) c


last_cps :: [a] -> (a -> r) -> r
last_cps [x] c = c x
last_cps (_:xs) c = last_cps xs c
last_cps [] _ = error "empty list"

{-
test :: Int
test = foldl_cps add 0 [1,2,3,4,5] id


thrice_cps :: (o -> (o -> r) -> r) -> o -> (o -> r) -> r
thrice_cps f x c = f x $ \e -> f e $ \e1 -> f e1 c

double :: (Num o) => o -> (o -> r) -> r
double x c = c (2*x)

test2 :: Int
test2 = thrice_cps double 2 id



fak_cps :: (Integral a) => a -> (a -> r) -> r
fak_cps 0 k = k 1
fak_cps n k = fak_cps (n-1) $ \e -> k (n * e)

main = do
	putStrLn "enter a number to compute faculty:"
	number <- readLn
	fak_cps number print

-}