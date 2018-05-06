-- implements these functions described in the lecture
--

(*>) :: Applicative f => f a -> f b -> f b
fa *> fb = (const id) <$> fa <*> fb


mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA f = sequenceA . (map f)

sequenceA_ :: Applicative f => [f a] -> f [a]
sequenceA_ (x:xs) = (:) <$> x <*> sequenceA xs

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA n fa = replicate n <$> fa
