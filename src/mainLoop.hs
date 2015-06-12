
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
extractAddr :: String -> String
--  could have format "Mike Houghton <mike_k_houghton@yahoo.co.uk>"
extractAddr st = name ++ right where
  (left, right)  = (takeWhile (\c -> c /= '@') st, takeWhile (\c -> c /= '>') (dropWhile (\c -> c /= '@' ) st)   )
  name = reverse $ takeWhile (\c -> c /= ' ' &&  c /= '<') (reverse left)


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
    createDirectory $ dir ++ "/" ++ id
    print $ dir ++ "/"  ++ id

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
    Right subj    -> do
      -- from could have format "Mike Houghton <mike_k_houghton@yahoo.co.uk>"
      let to = extractAddr $ mailFrom mail
      let sub = (turnId subj ++ " Turn " ++ show (turnNum subj  + 1))
      let ms = " OK! Received."
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
    putStrLn "Bugger..."
    print errMsg
    print mail

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
  forever $  do
    m <- getMail
    gameLoop m
    threadDelay 1000000








