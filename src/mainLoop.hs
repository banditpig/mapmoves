
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-} -- force eval of a list

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}
module MainLoop where
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
import System.IO

import System.Directory
-- ==============================================================================================================


saveGame :: FilePath -> Game -> IO()
saveGame path game = do
  writeFile path (show game)

loadGame :: FilePath -> IO(Game)
loadGame path = do
  withFile path ReadMode (\h -> do
    -- lazy evalso force it (I think thats it!! with ! it won't work, or will if do print contents)
    !contents <- hGetContents h

    let game = read contents::Game
    return $ game
    )


saveMoves :: FilePath -> Moves -> IO()
saveMoves path moves = do
  writeFile path (show moves)
  

loadMoves :: FilePath -> IO(Moves)
loadMoves path = do
  withFile path ReadMode (\h -> do
    -- lazy evalso force it (I think thats it!! with ! it won't work, or will if do print contents)
    !contents <- hGetContents h

    let moves = read contents::Moves
    return $ moves
    )

monadicLength :: IO[a] -> IO(Int)
monadicLength = fmap length

getFilesInPath :: FilePath -> IO[FilePath]
getFilesInPath path = do
  c <- getDirectoryContents path
  -- getDirectoryContents will include '.' and '..'
  print path
  print c
  filterM doesFileExist [ path ++ p | p <- c, p /="." && p /= ".." && p /= ".DS_Store"]


getUUID :: IO (String)
getUUID = do
  a <- nextUUID
  return $ toString $ fromJust a

createGame :: Game -> InMail -> IO ()
createGame game mail = do
    invite <- readFile "gameInvite.txt"

    let Game title _ players = game
    id <- getUUID
    -- email players
    mapM_ (\plyr -> postMail (email plyr) (id ++ " Turn 1" ) invite)  players
    dir <- getAppUserDataDirectory "mapmoves"
    -- make folder for turn 1  inside a unique folder for this game
    createDirectoryIfMissing True $ dir ++ "/" ++ id ++ "/" ++ "1"
    print $ dir ++ "/" ++ id ++ "/" ++ "1"
    -- write game details 

    writeFile (dir ++ "/" ++ id ++ "/game") (show game)


--   might skip deployment to start with and go straight in with moves

handleMoves :: Moves -> InMail -> IO ()
handleMoves moves mail = do
  let parseSub  = parse subjectParser "Parsing subj" (mailSubj mail)
  case parseSub of
    Left msg -> handleError msg mail 
    Right subj -> do     
      userDir <- getAppUserDataDirectory "mapmoves"
      let to = extractAddr $ mailFrom mail
          id = gameId subj
          
      --  sub = (gameId subj ++ " Turn " ++ show (turnNum subj  + 1))
          sub = id ++ " Turn " ++ show (turnNum subj)
          
           -- to could have format "Mike Houghton <mike_k_houghton@yahoo.co.uk>"
          folderName =  userDir ++ "/" ++ id ++ "/" ++ show (turnNum subj) ++ "/" ++ to
          ms = " OK! Moves received. Please be patient.  I'm still waiting for your opponents moves"
               
      saveMoves folderName moves

      let rootF = userDir ++ "/" ++ id ++ "/" ++ show (turnNum subj) ++ "/"
      let fInPath = getFilesInPath rootF

     
      -- 2 files ?
      count' <- monadicLength $ fInPath
      case count' of
      	1 -> do
          -- just say ok -received your moves
      		postMail to sub ms
        2 -> do

        	fPath <- fInPath 
        	let (m1, m2) = (loadMoves $  (fPath !! 0), loadMoves $ (fPath !! 1))
        	print fPath
        	m1' <- m1
        	m2' <- m2
        	g   <- loadGame (userDir  ++ "/" ++ id ++ "/game")
          
          -- any common points?
        	let pathsX = pathsCross  (getAllTo m1' ) (getAllTo m2' )
        	print "/n"
        	print pathsX
          

        	-- if pathsX not empty then send match details and also  ask for next turn. Otherwise send email saying
        	-- no contact and ask for next turn,
        _ -> do print "error"
        		



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
  forever $  do
    m <- getMail
    gameLoop m
    threadDelay 1000000








