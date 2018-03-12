{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Countdown where

import Network.HTTP.Client       (newManager, Manager)
import Network.HTTP.Client.TLS   (tlsManagerSettings)
import Web.Telegram.API.Bot
import Data.Time (UTCTime)
import Data.Time.Format (readTime, defaultTimeLocale)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.List.Utils (replace)
import qualified Data.Text as T
import Safe (atMay)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Timer (TimerIO, repeatedTimer)
import Control.Concurrent.Suspend.Lifted (hDelay, mDelay, sDelay)
import Control.Concurrent.PooledIO.Independent (run)
import Token

data Countdown = Countdown
    { _chatId    :: ChatId
    , _time      :: UTCTime
    , _message   :: T.Text
    , _intervall :: Intervall
    , _place     :: Place
    , _mode      :: Mode
    } deriving (Show)

data Place = Title | Msg
    deriving (Read, Show)

data Intervall = Day | Hour | Minute | Second
    deriving (Read, Show, Eq)

data Mode = RemindMe | Count
    deriving (Read, Show)

toCountdown :: ChatId -> T.Text -> Maybe Countdown
toCountdown chatId msg = Countdown <$> pure chatId
                                   <*> time
                                   <*> message
                                   <*> intervall
                                   <*> place
                                   <*> mode
    where pieces = T.lines msg
          time   = readTime defaultTimeLocale "%F %T" <$> 
              T.unpack <$> pieces `atMay` 0-- FIXME: readTime depraced
          intervall = read . T.unpack <$> pieces `atMay` 1
          place = read . T.unpack <$> pieces `atMay` 2
          mode = read . T.unpack <$> pieces `atMay` 3
          message = pieces `atMay` 4

parseMessage :: Countdown -> IO T.Text
parseMessage c = do
    d <- diff
    let ev = replace "%d" (show d) . replace "%t" (show $ _time c) . T.unpack $ _message c
    return $ T.pack ev
        where 
            diff = do
                d <- diffUTCTime (_time c) <$> getCurrentTime
                case _intervall c of
                    Day -> return . floor $ d / (60 * 60 * 24)
                    Hour -> return . floor $ d / (60 * 60)
                    Minute -> return . floor $ d / 60
                    Second -> return $ floor d

executeCountdown :: Token -> Manager -> Countdown -> IO ()
executeCountdown t m c@Countdown{..} = do
    msg <- parseMessage c
    case _place of
        Title -> do
            _ <- runTelegramClient t m $ setChatTitleM msg Nothing
            putStrLn "Set title"
            return ()
        Msg -> do
            _ <- sendMessage t (SendMessageRequest _chatId msg Nothing Nothing Nothing Nothing Nothing) m
            return ()

dispatchTimers :: Token -> Manager -> [Countdown] -> IO (TimerIO, TimerIO, TimerIO, TimerIO)
dispatchTimers t m c = do
    day <- repeatedTimer (action Day) (hDelay 24)
    hour <- repeatedTimer (action Hour) (hDelay 1)
    minute <- repeatedTimer (action Minute) (mDelay 1)
    second <- repeatedTimer (action Second) (sDelay 1)
    return (day, hour, minute, second)
        where action i = run . map (executeCountdown t m) . filter (\c -> _intervall c == i) $ c

main' :: IO ()
main' = do
    let chatId = ChatId (-165520488)
    manager <- newManager tlsManagerSettings
    _ <- dispatchTimers token manager [Countdown chatId (readTime defaultTimeLocale "%F" "2019-02-24")
                                       "vöhringen ist in %d min 🎉" Minute Msg Count]
    forever $ threadDelay 100000000
