
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}

import Parser
import Mail
import Config
import Data.Char (toLower)
import Data.UUID.V1
import Data.UUID
import Data.Maybe
import Text.Parsec
import Text.Email.Parser
import qualified Data.Text as T 
import Control.Concurrent (threadDelay)
import Control.Monad 

import System.Directory
-- ==============================================================================================================

monadicLength :: IO[a] -> IO(Int)
monadicLength = fmap length

getFoldersInPath :: FilePath -> IO[FilePath]
getFoldersInPath path = do
  c <- getDirectoryContents path
  -- getDirectoryContents will include '.' and '..'
  filterM doesDirectoryExist [path | path <- c, path /="." && path /= ".."]


getUUID :: IO (String)
getUUID = do
  a <- nextUUID
  return $ toString $ fromJust a

createGame :: Game -> InMail -> IO ()
createGame game mail = do
    invite <- readFile "gameInvite.txt"

    let Game title _ players = game
    id <- getUUID
    mapM_ (\plyr -> postMail (email plyr) (id ++ " Turn 1" ) invite)  players
    dir <- getAppUserDataDirectory "mapmoves"
    -- make folder for turn 1  inside a unique folder for this game
    createDirectory $ dir ++ "/" ++ id ++ "/" ++ "1"
    print $ dir ++ "/" ++ id ++ "/" ++ "1"

    --print players
-- email to each player
--   subj contains id of game
--   maybe create folder in file sys for this game?
--   might skip deployment to start with and go straight in with moves

handleMoves :: Moves -> InMail -> IO ()
handleMoves moves mail = do
  let parseSub  = parse subjectParser "Parsing subj" (mailSubj mail)
  case parseSub of
    Left msg -> handleError msg mail 
    Right subj -> do

      -- from could have format "Mike Houghton <mike_k_houghton@yahoo.co.uk>"
      let to =  mailFrom mail
      --  sub = (gameId subj ++ " Turn " ++ show (turnNum subj  + 1))
          sub = gameId subj ++ " Turn " ++ show (turnNum subj)
          ms = " OK! Received. "

      -- saveMoves in dir ++ "/" ++ id ++ "/" ++  turnNum subj with further subfolder name of extractAddr 'to' 
      -- does this folder now have two entries ??
      -- no - then just send the " OK! Received. " email
      -- yes - then load up both and do a path match...
      print to
      print sub
      print ms
      postMail to sub ms


-- persist moves to folder for this game for this turn
-- are there two persisted moves for this turn?
--  no - then wait for  both moves to be received for this turn
--  yes - load up both moves for this turn
--
-- ==============================================================================================================

handleHeader h mail cleanBody = do
    let strippedOfHeader = removeHeader cleanBody h
    
    case h of
        GameHdr    -> do
                       let parsedGame = parse gameParser "Parsing game" ( map toLower  strippedOfHeader)
                       case parsedGame of
                          Left  msg  -> handleError  msg mail -- h strippedOfHeader
                          Right game -> createGame game mail -- print game -- 

        DeployHdr  -> do 
                       let  parsedDeployment = parse deployParser "Parsing deployment" ( map toLower strippedOfHeader)
                       case parsedDeployment of
                          Left  msg  -> handleError  msg mail  -- h strippedOfHeader
                          Right dep  -> print dep -- 
        MoveHdr    -> do 
                       let  parsedMvs = parse movesParser "Parsing Moves" ( map toLower strippedOfHeader)
                       case parsedMvs of
                          Left  msg  -> handleError  msg mail -- bh strippedOfHeader
                          Right mvs  -> handleMoves mvs mail 

        RemoveHdr  -> do 
                       print h
                       print mail

        InfoHdr    -> do 
                       print h
                       print mail
                       
        QuitHdr    -> do 
                       print h
                       print mail
                       
        UnknownHdr -> do
                       print h
                       print mail

splitToEndString :: String -> String
splitToEndString str = T.unpack $ head $ T.splitOn (T.pack "end")  (T.pack str)


handleError errMsg  mail = do
    putStrLn "Error..."
    let to =  mailFrom mail
    let sub = "Incorrect map moves email..."
    postMail to sub (show errMsg)
    print errMsg
   

--gameLoop :: InMail
gameLoop inMail = do
    -- look at header in mailbody first
    case inMail of

        Just aMail  -> do
            -- cleanBody is everything upto the first "end" in the email body
            let cleanBody = splitToEndString $ mailBody aMail
            let hdr = parse headerParser "Parsing Header" ( cleanBody)
            print hdr
            case hdr of
                Left  err -> handleError err aMail -- hdr cleanBody
                Right h   -> handleHeader h aMail  cleanBody

        Nothing      -> print "No mail"

-- ==============================================================================================================


main = do
  let appSetup = [( "smtpServer", smtpServer), ("username", username), ("password", password), ("imapServer", imapServer)]::AppValues
  initSystem "email.cfg" appSetup
  postMail "mapmoves@gmail.com"  "test" "testing"
  getMail
  -- forever $  do
  --   m <- getMail
  --   gameLoop m
  --   threadDelay 1000000








