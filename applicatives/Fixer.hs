module Fixer where

f :: Maybe String
f = const <$> Just "Hello" <*> pure "World"

y :: Maybe (Integer, Integer, [Char], [Integer])
y = (,,,) <$> Just 90
          <*> Just 10
          <*> Just "Tierness"
          <*> pure [1, 2, 3]
