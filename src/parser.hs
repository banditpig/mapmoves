{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}
-- An email must start with one of
    -- game
    -- deploy
    -- move
    -- info
    -- quit

-- game
--  "a game"
--  player name, email
--  player name, email
--  -- 

-- deploy

--  unit(name,descr) -> (1,0)
--  unit('name','descr') -> (1,0)
--  unit('name','descr') -> (1,0)
-- move
--  (1,0,name) ->  (2,0)
--  (2,0,name) ->  (2,0)
--  (11,0,name) -> (2,0)
import Control.Concurrent (threadDelay)
import Control.Monad 
import Mail
import Config
import Data.Char (toLower)
import Data.UUID.V1
import Data.UUID
import Data.Maybe
import Text.Parsec
import Text.Parsec.String
import qualified Data.Text as T



-- just an x,y 
data Location = Location Int Int deriving (Show, Eq)
-- from and to
data Move = Move { from :: Location, to :: Location} deriving (Show)
-- data Moves = Moves [Move] deriving (Show)
type Moves = [Move]
-- a series of locations
type Path = [Location] 
-- a name at least and a location
data Unit = Unit { unitName :: String,
                   descr    :: String,
                   location :: Location
                 } deriving (Show)

type Deployment = [Unit] 

type Title  = String
data Player = Player { name  :: String,
                       email :: String } deriving (Show)
type GameId = String
data Game = Game Title GameId [Player] deriving (Show)

makeUniqueId :: IO (String)
makeUniqueId = undefined

pathsCross :: Path -> Path -> Path
pathsCross p1 p2 = [ l1 | l1 <- p1, _ <- p2, p1 == p2]

-- get the 'to' values from the Moves
endPoints :: [Move] -> Path
endPoints [] = []
endPoints (mv:[]) = [ to mv]
endPoints (mv:mvs) = [ to mv ] ++ endPoints  mvs

-- preamble in email - indicates what to expect in the email
data Header = GameHdr | DeployHdr | MoveHdr | RemoveHdr | InfoHdr | QuitHdr| UnknownHdr deriving (Show,Eq)
strToHeader :: String -> Header
strToHeader str 
     | str == "game"   = GameHdr
     | str == "deploy" = DeployHdr
     | str == "move"   = MoveHdr
     | str == "remove" = RemoveHdr
     | str == "info"   = InfoHdr
     | str == "quit"   = QuitHdr
     | otherwise       = UnknownHdr
headerToString :: Header -> String
headerToString hdr 
     | hdr == GameHdr    = "game"
     | hdr == DeployHdr  = "deploy"
     | hdr == MoveHdr    = "move"
     | hdr == RemoveHdr  = "remove"
     | hdr == InfoHdr    = "info"
     | hdr == QuitHdr    = "quit"
     | hdr == UnknownHdr = "unknown"
     | otherwise = "unknown"

removeHeader :: String -> Header -> String
removeHeader str hdr = hdxs
     where 
        (_:xs) = T.splitOn (T.pack (headerToString hdr ))  (T.pack str)
        hdxs = T.unpack $ head xs


headerParser :: Parser Header
headerParser = do
    spaces
    hdrStr <- many1 letter
    return $ strToHeader $ map toLower hdrStr


deployParser :: Parser Deployment
deployParser = do
    unit  <- unitParser
    units <- many unitParser
    return $ [unit] ++ units



moveParser :: Parser Move
moveParser = do
    spaces
    from <- locationParser
    spaces 
    string "->"
    spaces
    to <- locationParser
    spaces
    char ';'
    spaces
    return $ Move from to

movesParser :: Parser [Move]
movesParser = do
        move <- moveParser
        moves <- many  moveParser
        return $  ([move] ++ moves)

locationParser :: Parser Location -- (1,0)
locationParser = do
    spaces
    char '('
    spaces 
    x <- many1 digit
    spaces
    char ','
    spaces 
    y <- many1 digit
    spaces
    char ')'
    return $ Location  (read x)  (read y)

letterDigUndrSp :: Parser String
letterDigUndrSp = do
   many1 (letter <|> digit <|> char '_' <|> char ' ')

unitParser :: Parser Unit
unitParser = do
    spaces
    string "unit("
    unitName <- letterDigUndrSp
    char ','
    char ' '
    spaces
    descr <- letterDigUndrSp
    spaces
    char ')' 
    spaces
    location <- locationParser
    spaces
    return $ Unit unitName descr location

letterDigUndrAmpDot :: Parser String
letterDigUndrAmpDot = do
    many1 (letter <|> digit <|> char '_' <|> char '@' <|> char '.')


playerParser :: Parser Player
playerParser = do
    spaces
    ( string "Player" <|> string "player" )
    spaces
    name <- letterDigUndrAmpDot 
    spaces
    email <- letterDigUndrAmpDot 
    spaces
    return $ Player name email

playersParser :: Parser [Player]
playersParser = do
    -- player  <- playerParser
    players <- many1 playerParser
    return  $ players

gameParser :: Parser Game
gameParser = do
    -- spaces
    -- string "game"
    spaces
    char '\''
    title <- many1 (letter <|> digit <|> char '_' <|> char ' ')
    char '\''
    spaces 
    players <- many1 playerParser 
    spaces
    
    let id = "idplaceHolder"
    return $ Game id title players

-- ==============================================================================================================

getUUID :: IO (String)
getUUID = do
  a <- nextUUID
  return $ toString $ fromJust a


--createGame :: Game -> InMail -> IO ()
createGame game mail = do
    invite <- readFile "gameInvite.txt"

    let Game title _ players = game
    id <- getUUID
    mapM_ (\plyr -> postMail (email plyr) id invite)  players
    --print players
-- email to each player
--   subj contains id of game
--   maybe create folder in file sys for this game?
--   might skip deployment to start with and go straight in with moves

handleMoves = undefined

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
                          Left  msg  -> handleError  msg mail h strippedOfHeader
                          Right game -> createGame game mail -- print game -- 

        DeployHdr  -> do 
                       let  parsedDeployment = parse deployParser "Parsing deployment" ( map toLower strippedOfHeader)
                       case parsedDeployment of
                          Left  msg  -> handleError  msg mail  h strippedOfHeader
                          Right dep  -> print dep -- 
        MoveHdr    -> do 
                       let  parsedMvs = parse movesParser "Parsing Moves" ( map toLower strippedOfHeader)
                       case parsedMvs of
                          Left  msg  -> handleError  msg mail h strippedOfHeader
                          Right mvs  -> print mvs -- handleMoves mvs  mail 

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


handleError errMsg  mail hdr cleanBody = do
    putStrLn "Bugger..."
    print errMsg
    -- print mail
    putStrLn cleanBody

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
                Left  err -> handleError err aMail hdr cleanBody
                Right h   -> handleHeader h aMail  cleanBody

        Nothing      -> print "No mail"

-- ==============================================================================================================

testBodyParser  :: IO ()
testBodyParser = do
    b <- readFile "body.txt"
    print $ T.splitOn "end" (T.pack b)
    -- let parseBody = parse toEndParser "Parsing body" b
    -- return $ case parseBody of
    --  Left msg ->  show (msg)
    --  Right  v ->  show (  v)


testHeaderParser :: String -> IO (String)
testHeaderParser h = do
    let parsedHd = parse headerParser "Parsing header" h
    return $ case parsedHd of
        Left msg ->  show (msg)
        Right  v ->  show (  v)

testMovesParser :: IO (String)
testMovesParser = do
    mvs <- readFile "moves.txt"
    let  parsedMvs = parse movesParser "Parsing Moves" mvs
    return $ case parsedMvs of
        Left msg ->  show msg
        Right  v ->  show ( endPoints v)

testDeployParser :: IO (String)
testDeployParser = do
    deployment <- readFile "deploy.txt"
    let  parsedDeployment = parse deployParser "Parsing deployment" deployment
    return $ case parsedDeployment of
        Left msg ->  show msg
        Right  v ->  show  v

testGameParser :: IO (String)
testGameParser = do
    game <- readFile "game.txt"
    let  parsedGame = parse gameParser "Parsing game" game
    return $ case parsedGame of
        Left msg ->  show msg
        Right  v ->  show  v

testPlayersParser :: IO (String)
testPlayersParser = do
    players <- readFile "players.txt"
    let  parsedPlayers = parse playersParser "Parsing players" players
    return $ case parsedPlayers of
        Left msg ->  show msg
        Right  v ->  show v
-- data InMail = InMail {mailFrom :: String, mailSubj :: String, mailBody :: String} deriving(Show)

-- main = do testGameParser

-- writePropToIORef config "smtpServer"  smtpServer
-- --   writePropToIORef config "username"    username
-- --   writePropToIORef config "password"    password
-- --   writePropToIORef config "imapServer"  imapServer
main = do
  let appSetup = [( "smtpServer", smtpServer), ("username", username), ("password", password), ("imapServer", imapServer)]::AppValues
  -- setupMailing "email.cfg"
  initSystem "email.cfg" appSetup
  postMail "mapmoves@gmail.com"  "test" "testing"
  getMail
  forever $  do
    m <- getMail
    gameLoop m
    threadDelay 1000000
-- username = "mapmoves@gmail.com"
--   password   = "kate1234!" 

--   smtpServer = "smtp.gmail.com" 
--     imapServer 
-- mailConfig :: IORef [MailConfig]

-- main =  forever $  do
--  m <- getMail
--  gameLoop m
--  threadDelay 1000000

    -- let m = InMail "mike@yahoo.com" "test" "Game 'My test game for two' player mike mike@x.y player joe joe@x.y player fred fred@xy"

    -- keep getting emails and hand off to gameLoop
    -- let list = ["quit", "move", "game", "DepLOY", "Info", "remoVe", "moves"]

    -- mapM testHeaderParser list
    --main
    -- testPlayersParser
    -- testGameParser
    -- testDeployParser
    -- -- testMovesParser


