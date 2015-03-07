--reimplementing the class Arr(ow)
class Arr a where
  arr   :: (b -> c) -> a b c
  (>>>) :: a b c -> a c d -> a b d

  (<<<) :: a c d -> a b c -> a b d
  (<<<) = flip (>>>)

  first :: a b c -> a (b, d) (c, d)

  second :: Arr a => a b c -> a (d, b) (d, c)
  second f = arr swap >>> first f >>> arr swap
    where swap = \(x, y) -> (y, x)

  (***) :: Arr a => a b c -> a b' c' -> a (b, b') (c, c')
  f *** g = first f >>> second g

  (&&&) :: Arr a => a b c -> a b d -> a b (c, d)
  f &&& g = arr (\b -> (b, b)) >>> (f *** g)


instance Arr (->) where
  arr = id
  (>>>) = flip (.)
  first f = \(b, c) -> (f b, c)
