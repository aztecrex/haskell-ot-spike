module OperationalTransformation where


class Operation a where
  transform :: a -> a -> (a, a)
  compose :: a -> a -> a
