module Utils where

whileJust f = do
  f' <- f
  case f' of
    Just x -> x : whileJust f
    Nothing -> []
