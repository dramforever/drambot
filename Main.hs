{-# LANGUAGE OverloadedStrings #-}

import Network
import System.IO
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Monoid
import Control.Arrow (second)
import Control.Monad.Reader
import Control.Exception
import Data.IORef

data Bot = Bot { botServer :: HostName
               , botPort :: PortNumber
               , botChan :: B.ByteString
               , botNick :: B.ByteString
               , botPrefix :: B.ByteString
               , botSock  :: Handle
               , botState :: IORef BotState
               }

data BotState = BotState {
  bsCounter :: Integer
}

type Net = ReaderT Bot IO

askSock :: Net Handle
askSock = botSock `fmap` ask

askState :: Net (IORef BotState)
askState = botState `fmap` ask

modifyState :: (BotState -> BotState) -> Net ()
modifyState f = do
  st <- askState
  liftIO $ modifyIORef st f

readState :: Net BotState
readState = askState >>=  liftIO . readIORef

initialBotState :: BotState
initialBotState = BotState {
  bsCounter = 0
}

connect :: IO Bot
connect = do
  let server = "irc.freenode.org"
      port   = 6666
      chan   = "#illumer"
      nick   = "drambot"
      prefix = "!"
  h <- connectTo server (PortNumber port)
  r <- newIORef initialBotState
  hSetBuffering h NoBuffering
  return Bot { botServer = server
             , botPort = port
             , botChan = chan
             , botNick = nick
             , botPrefix = prefix
             , botSock  = h
             , botState = r
             }

auth :: Net ()
auth = do
  bot <- ask
  let nick = botNick bot
      chan = botChan bot
  
  writeCmd "NICK" nick Nothing
  writeCmd "USER" (nick <> " 0 *") (Just nick)
  writeCmd "JOIN" chan Nothing

writeCmd :: B.ByteString
         -> B.ByteString
         -> Maybe B.ByteString
         -> Net ()

writeCmd cmd arg msg = do
  liftIO $ B.putStrLn $ "> " <> s
  sock <- askSock
  liftIO $ B.hPut sock $ s <> "\r\n"
  where s = cmd <> " " <> arg <> rest
        rest = case msg of
          Just m -> " :" <> m
          Nothing -> mempty

privmsg :: B.ByteString -> B.ByteString -> Net ()
privmsg to str = writeCmd "PRIVMSG" to (Just str)

runLine :: B.ByteString -> Net ()
runLine str
  | str == "" = return ()
  | "PING :" `B.isPrefixOf` str = writeCmd "PONG" "" (Just $ B.drop 6 str)
  | otherwise = do
      bot <- ask
      let nick = botNick bot
          chan = botChan bot
          splitPrefix = second (B.drop 1) . B.break (== ' ') . B.drop 1
          (pre, str1) = splitPrefix str
          fromNick = B.takeWhile (/= '!') pre
          cleanChannel = B.drop 1 . B.dropWhile (/= ':')
          str2 = cleanChannel str1
          action
            | ("PRIVMSG " <> chan) `B.isPrefixOf` str1 = runMsg chan str2
            | ("PRIVMSG " <> nick) `B.isPrefixOf` str1 = runMsg fromNick str2
            | otherwise = return ()
        in action

logAndRunLine :: B.ByteString -> Net ()
logAndRunLine str = do
  liftIO $ B.putStrLn $ "# " <> str
  runLine str

runMsg :: B.ByteString -> B.ByteString -> Net ()
runMsg to s = do
  prefix <- botPrefix `fmap` ask
  let action =
        let s' = B.drop (B.length prefix) s
            (cmd, arg) = second B.tail (B.break (== ' ') s')
        in runCmd cmd arg >>= privmsg to
  if prefix `B.isPrefixOf` s then action else return ()

runCmd :: B.ByteString -> B.ByteString -> Net B.ByteString
runCmd "hi" _ = return "Hello"
runCmd "echo" a = return a

runCmd "+1" _ = do
  modifyState (\s -> s { bsCounter = bsCounter s + 1 })
  st <- readState
  return $ B.pack . show $ (bsCounter st)

runCmd "help" "hi" = return "Just replys hello"
runCmd "help" "echo" = return "Just echo"
runCmd "help" "+1" = return "A basic counter"
runCmd "help" _ = return "Commands: help hi echo +1"

runCmd s _= return $ "Unknown command " <> s <> ", try !help"

getMsgs :: Bot -> IO [B.ByteString]
getMsgs bot = (map B.init . filter (not . B.null))
              `fmap` B.split '\n'
              `fmap` (B.hGetContents $ botSock bot)

main' :: Bot -> IO ()
main' bot = do
  runReaderT auth bot
  msgs <- getMsgs bot
  runReaderT (mapM_ logAndRunLine msgs) bot

main :: IO ()
main = bracket connect (\_ -> putStrLn "* Stopped!") main'
