module MaybeLibrary where

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _        = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ f (Just x) = f x
mayybee b _ Nothing  = b

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just a) = a
fromMaybe a Nothing  = a

listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr filterNothing []
    where filterNothing a b = case a of
                                Nothing -> b
                                Just x  -> x : b

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs = case go xs [] of
    []  -> Nothing
    xs' -> Just xs'
    where go (x:xs') r = case x of
                            Nothing -> []
                            Just x' -> if null xs' then r ++ [x'] else go xs' (r ++ [x'])
