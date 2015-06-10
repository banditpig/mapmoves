{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-} -- force eval of a list
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}

module Config where
import System.IO
import Data.List.Split
import Data.Maybe()
import Data.IORef

type Key = String
type Val = String
type Props = [KeyVal] 
type AppVal = (Key, IORef String)
type AppValues = [AppVal]

data KeyVal = KeyVal { key :: Key, value :: Val} deriving (Show, Eq)


writePropToIORef :: Props -> Key -> IORef String -> IO ()
writePropToIORef config key ioRef = do
  let x = getVal config key
  case x of 
    Just a -> do writeIORef ioRef a
    Nothing -> error $ "Bad key supplied " ++ key


initOne :: AppVal -> Props  -> IO()
initOne appVal props = do
    writePropToIORef props k ioR  
    where 
        (k, ioR) = appVal

initSystem :: String -> AppValues -> IO ()
initSystem confPath appVals = do
    config  <- readProps confPath
    mapM_ (\v -> initOne v config) appVals

asIOString ::  IORef String -> IO String
asIOString ior = do
    let ioStr = readIORef ior
    str <- ioStr
    return $ str

-- Gets a value for a key if the key exists.
getVal :: Props -> Key -> Maybe Val
getVal p k =
    case f of 
        [] -> Nothing
        _  -> Just (value $ head f)

        where
            f = [ kv | kv <- p, ( key kv ) == k]


--  Gets a value for a key. If the key exists then return its value otherwise return the supplied 
--  default value.

getValDef :: Props -> Key -> Val -> Val
getValDef p k v = 
    case val of
        Just m  -> m
        Nothing -> v
    where
        val = getVal p k
   

makeKeyVal :: String -> KeyVal
makeKeyVal str = kv where
    [l, r ] =  splitOn "=" str
    kv = KeyVal (unwords $ words l)  (unwords $ words r)


readProps :: String -> IO (Props)
readProps path = do
    withFile path ReadMode (\h -> do
        contents <- hGetContents h
        let !props = [ makeKeyVal str | str <- (lines  $  contents)]
        return $ props
        )


    