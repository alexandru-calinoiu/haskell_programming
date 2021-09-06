module ReviewCurrying where

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy = flip cattyConny

appendCatty = cattyConny "woops"
frappe = flippy "haha"
