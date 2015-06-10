{-# LANGUAGE OverloadedStrings #-}

module Mail where
import Network.HaskellNet.IMAP
import Control.Monad

import Network.HaskellNet.SMTP.SSL
import Network.HaskellNet.SSL
import Codec.MIME.Parse (parseMIMEMessage)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy hiding (head)
import Codec.MIME.Parse
import Codec.MIME.Type
import System.Process

import Network.HaskellNet.IMAP.SSL
import Network.HaskellNet.SMTP.SSL as SMTP
import Network.HaskellNet.Auth (AuthType(LOGIN))
import Data.String


import System.IO.Unsafe
import Data.IORef
import Config

data InMail = InMail {mailFrom :: String, mailSubj :: String, mailBody :: String} deriving(Show)
data MailConfig = MailConfig {smtp :: String, imap :: String, uname :: String, pwd :: String} deriving(Show)


-- app config values to be set from external config file
smtpServer :: IORef String
smtpServer = unsafePerformIO $ newIORef ""
 
username :: IORef String
username = unsafePerformIO $ newIORef ""

password :: IORef String
password = unsafePerformIO $ newIORef ""

imapServer :: IORef String
imapServer = unsafePerformIO $ newIORef ""




postMail :: String -> String -> String -> IO ()
postMail to subj message = do
  smtpServer' <- (asIOString smtpServer)
  username'   <- (asIOString username)
  password'   <- (asIOString password)

  c <- connectSMTPSTARTTLS  smtpServer'
  authSucceed <- SMTP.authenticate LOGIN username' password' c
  print authSucceed

  if authSucceed
      then sendPlainTextMail to username' subj (fromString message) c 
      else print "Authentication error."
  


getMail :: IO  (Maybe InMail)
getMail = do

  username'   <- (asIOString username)
  password'   <- (asIOString password)
  imapServer' <- (asIOString imapServer)

  con <- connectIMAPSSLWithSettings imapServer' defaultSettingsIMAPSSL
  login con username' password'
  mboxes <- list con
  select con "INBOX"

  ids <- search con [UNFLAG Seen] -- NEWs
  case ids of
      [] -> do
              logout con
              return Nothing
      _  -> do
              let id = head ids
              msg <-  fetch con id 
              logout con

              let mval = parseMIMEMessage (decodeUtf8 msg)
              
              let params = mime_val_headers mval
              let subjParam = s where s = head [ p | p <- params, (paramName p) == "subject"]
              let fromParam = f where f = head [ p | p <- params, (paramName p) == "from"] 

              let txtBody = mime_val_content mval
              case txtBody of
                                 
                Multi (v:vs) -> do
                  let Single txt = mime_val_content  v -- tVals
                  return $ Just (InMail  ( unpack $ fromStrict (paramValue fromParam)) (unpack $ fromStrict (paramValue subjParam))   (unpack $ fromStrict txt))

                Single txt -> do
                  return $ Just (InMail  ( unpack $ fromStrict (paramValue fromParam)) (unpack $ fromStrict (paramValue subjParam))   (unpack $ fromStrict txt))


