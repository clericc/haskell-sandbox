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

foldr_cps :: (b -> a -> (a -> r) -> r) -> a -> [b] -> (a -> r) -> r
foldr_cps _ e [] k = k e
foldr_cps f e (x:xs) k = foldr_cps f e xs (\e' -> f x e' k)



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

map_cps :: (a -> (b -> r) -> r) -> [a] -> ([b] -> r) -> r
map_cps f = foldr_cps (\ e acc k' -> f e $ \ e' -> k' $ e':acc) []


--zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
--zipWith _ [] _ = []
--zipWith _ _ [] = []
--zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys


zipWith_cps :: (a -> b -> (c -> r) -> r) -> [a] -> [b] -> ([c] -> r) -> r
zipWith_cps _ [] _ k = k []
zipWith_cps _ _ [] k = k []
zipWith_cps f (x:xs) (y:ys) k = f x y $ \ z -> zipWith_cps f xs ys $ \zs -> k (z:zs)

zip_cps :: [a] -> [b] -> ([(a,b)] -> r) -> r
zip_cps = zipWith_cps (\a b k -> k (a,b))


map f xs = foldr (\ e acc -> f e : acc) [] xs

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
concat_cps [] ys k = k ys
concat_cps (x:xs) ys k = concat_cps xs ys $ \zs -> k $ x : zs