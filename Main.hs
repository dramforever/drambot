{-# LANGUAGE OverloadedStrings #-}

import Network
import System.IO
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Monoid
import Control.Arrow (second)
import Control.Monad.Reader
import Control.Exception
import Data.IORef

data Bot = Bot { botChan :: B.ByteString
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

connect :: Handle
        -> B.ByteString
        -> B.ByteString
        -> B.ByteString
        -> IO Bot

connect han chan nick pre = do
  ref <- newIORef initialBotState
  hSetBuffering han NoBuffering
  return Bot { botChan = chan
             , botNick = nick
             , botPrefix = pre
             , botSock  = han
             , botState = ref
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
  bot <- ask
  let prefix = botPrefix bot
      nick = botNick bot
      s' = B.drop (B.length prefix) s
      (cmd, arg) = second
                   (\x -> if B.null x then x else B.tail x)
                   (B.break (== ' ') s')
      action
        | nick `B.isPrefixOf` s = runCmd "%mention" "" >>= privmsg to
        | prefix `B.isPrefixOf` s = runCmd cmd arg >>= privmsg to
        | otherwise = return ()
  action

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

runCmd "%mention" _ = return "Hi, I'm drambot. Try !help"

runCmd s _= return $ "Unknown command " <> s <> ", try !help"

getMsgs :: Net [B.ByteString]
getMsgs = (map B.init . filter (not . B.null))
          `fmap` B.split '\n'
          `fmap` (askSock >>= liftIO . B.hGetContents)

main' :: Net ()
main' = do
  auth
  getMsgs >>= mapM_ logAndRunLine

main :: IO ()
main = do
  han <- connectTo "irc.freenode.org" (PortNumber 6666)
  bracket (connect han "#illumer" "drambot" "!")
    (\_ -> putStrLn "* Stopped!")
    (runReaderT main')

