module Daniel.Utils.Web.URL (
    getParams,
    getBaseURL,
    getTLD,
    getJavaOrgPackagePrefix
)
where

import Daniel.Utils.List
import Data.List (stripPrefix, intersperse, insert)

type URL = String

getParams :: URL -> [(String,String)]
getParams u = map (\[key,val] -> (key,val)) $ map (split '=') $ split '&' $ tail $ dropWhile (/='?') u

getBaseURL :: URL -> URL
getBaseURL u = case (stripPrefix "http://" u) of
                Just x -> takeWhile (/='/') x
                Nothing -> takeWhile (/='/') u

getTLD :: URL -> String
getTLD u = last $ split '.' $ getBaseURL u

-- ostentatious superficial corporate bullshit
getJavaOrgPackagePrefix :: URL -> String
getJavaOrgPackagePrefix u = case (stripPrefix "www." (getBaseURL u)) of
                                Just x ->  java x
                                Nothing -> java (getBaseURL u)
                                where
                                    java t = concat $ reverse $ insert "." $ reverse $ intersperse "." $ reverse $ split '.' t
