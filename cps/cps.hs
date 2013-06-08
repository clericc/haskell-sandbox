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

--foldl_cps :: (a -> b -> (a -> r) -> r) -> a -> [b] -> (a -> r) -> r
--foldl_cps _ e [] k = k e
--foldl_cps f e (x:xs) k = f e x (\e' -> foldl_cps f e' xs k)

--foldr_cps :: (b -> a -> (a -> r) -> r) -> a -> [b] -> (a -> r) -> r
--foldr_cps _ e [] k = k e
--foldr_cps f e (x:xs) k = foldr_cps f e xs (\e' -> f x e' k)   -- foldr f e (x:xs) = f x (foldr f e xs)

foldl_cps1 :: (a -> b -> Cont r a) -> a -> [b] -> Cont r a
foldl_cps1 _ acc [] = return acc
foldl_cps1 f acc (x:xs) = f acc x >>= (\e -> foldl_cps1 f e xs)

foldl_cps2 :: (a -> b -> Cont r a) -> a -> [b] -> Cont r a
foldl_cps2 _ acc [] = return acc
foldl_cps2 f acc (x:xs) = cont $ \k -> runCont (f acc x) (\e -> runCont (foldl_cps2 f e xs) k)

foldl_cps3 :: (a -> b -> Cont r a) -> a -> [b] -> Cont r a
foldl_cps3 _ acc [] = return acc
foldl_cps3 f acc (x:xs) = do
  e <- f acc x
  res <- foldl_cps3 f e xs
  return res

-- add_cps :: Num a => a -> a -> Cont r a
add_cps x y = return (x+y)

test1 = runCont (add_cps 5 11) id


--qsort_cps :: Ord a => [a] -> ([a] -> r) -> r
--qsort_cps [] c = c []
--qsort_cps (x:xs) c = foldl_cps foldfunc ([],[]) xs
--	$ \(lessX,greaterX) -> qsort_cps lessX
--	$ \lsorted -> qsort_cps greaterX
--	$ \rsorted -> c $ lsorted ++ (x:rsorted)
--  	where
--    foldfunc (l,r) e cont = cont $ if e < x then (e:l,r) else (l,e:r)

foo :: Int -> Cont r String
foo n =
  callCC $ \k -> do
    when (n > 20) $ k "over twenty"
    return . show $ n

listCons :: [a] -> [a] -> ([a] -> r) -> r
listCons [] ys k = k ys
listCons (x:xs) ys k = listCons xs ys $ \ zs -> k (x:zs)

--cps1 f (x:xs) = f x >>= \x' -> map_cps1 f xs >>= \xs' -> return $ x':xs'

testfoo = runCont (foo 21) id

plus1 e f = f (e+1)

add :: Num a => a -> a -> Cont r a
add x y = return (x+y)

        
-- m >>= f  = Cont (\k -> runCont m (\v -> runCont (f v) k))

print1to4 = runCont (add 1 2 >>= add 3 >>= add 4) print

                      --add 1 2 >>= add 3
print1to3' = runCont ( cont (\k -> runCont (add 1 2) (\v -> runCont (add 3 v) k))) print



foldl_cps _ e [] = return e
foldl_cps f e (x:xs) = f e x >>= \e' -> foldl_cps f e' xs

partitionM :: (Monad m, Ord a) => a -> [a] -> m ([a],[a])
partitionM _ [] = return ([],[])
partitionM e (x:xs) = partitionM e xs >>= \ (l,r) -> return $ if x < e then (x:l, r) else (l, x:r)

partition_cps :: Ord a => a -> [a] -> (([a],[a]) -> r) -> r
partition_cps _ [] k = k ([],[])
partition_cps e (x:xs) k = partition_cps e xs $ \(l,r) -> k $ if x < e then (x:l, r) else (l, x:r)

qsortM []     = return []
qsortM (x:xs) = partitionM x xs
  >>= \ (left,right) -> qsortM left
  >>= \ lsorted      -> qsortM right
  >>= \ rsorted      -> return $ lsorted ++ [x] ++ rsorted