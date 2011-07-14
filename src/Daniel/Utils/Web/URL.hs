module Daniel.Utils.Web.URL (
    getParams
)
where

import Daniel.Utils.List

type URL = String

getParams :: URL -> [(String,String)]
getParams u = map (\[key,val] -> (key,val)) $ map (split '=') $ split '&' $ tail $ dropWhile (/='?') u


