module Main (main) where
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Data.Random.Dice
import Data.Either
import System.Environment
import System.Exit
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import UnliftIO
import UnliftIO.Concurrent

import Discord
import Discord.Types
import qualified Discord.Requests as R

-- Allows this code to be an executable. See discord-haskell.cabal
main :: IO ()
main = pingpongExample

-- | Replies "pong" to every message that starts with "ping"
pingpongExample :: IO ()
pingpongExample = do
  tok <- TIO.readFile "./auth-token.secret"

  -- open ghci and run  [[ :info RunDiscordOpts ]] to see available fields
  t <- runDiscord $ def { discordToken = tok
                        , discordOnStart = startHandler
                        , discordOnEnd = liftIO $ putStrLn "Ended"
                        , discordOnEvent = eventHandler
                        , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
                        }
  threadDelay (1 `div` 10 * 10^(6 :: Int))
  TIO.putStrLn t

-- If the start handler throws an exception, discord-haskell will gracefully shutdown
--     Use place to execute commands you know you want to complete
startHandler :: DiscordHandler ()
startHandler = do
  Right partialGuilds <- restCall R.GetCurrentUserGuilds

  let activity = Activity { activityName = "rolling in the deep"
                          , activityType = ActivityTypeGame
                          , activityUrl = Nothing
                          }
  let opts = UpdateStatusOpts { updateStatusOptsSince = Nothing
                              , updateStatusOptsGame = Just activity
                              , updateStatusOptsNewStatus = UpdateStatusOnline
                              , updateStatusOptsAFK = False
                              }
  sendCommand (UpdateStatus opts)

  forM_ partialGuilds $ \pg -> do
    Right guild <- restCall $ R.GetGuild (partialGuildId pg)
    Right chans <- restCall $ R.GetGuildChannels (guildId guild)
    pure ()

-- we can't use this unless the type of discordOnEnd changes
endHandler :: DiscordHandler ()
endHandler = do
  Right partialGuilds <- restCall R.GetCurrentUserGuilds

  let activity = Activity { activityName = "rolling in the deep"
                          , activityType = ActivityTypeGame
                          , activityUrl = Nothing
                          }

  let opts = UpdateStatusOpts { updateStatusOptsSince = Nothing
                              , updateStatusOptsGame = Just activity
                              , updateStatusOptsNewStatus = UpdateStatusOffline
                              , updateStatusOptsAFK = False
                              }
  sendCommand (UpdateStatus opts)

  forM_ partialGuilds $ \pg -> do
    Right guild <- restCall $ R.GetGuild (partialGuildId pg)
    Right chans <- restCall $ R.GetGuildChannels (guildId guild)
    pure ()

-- If an event handler throws an exception, discord-haskell will continue to run
eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
      MessageCreate m -> when (not (fromBot m) && forBot m) $ do
        _ <- restCall (R.CreateReaction (messageChannel m, messageId m) "eyes")
        threadDelay (4 * 10^(6 :: Int))
        diceroll <- liftIO $ (T.pack <$>) <$> rollEm (unwords . drop 1 . words . T.unpack $ messageText m)
        _ <- restCall (R.CreateMessage (messageChannel m) (rollmessage m diceroll))
        pure ()
      _ -> pure ()

isTextChannel :: Channel -> Bool
isTextChannel ChannelText {} = True
isTextChannel _ = False

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

forBot :: Message -> Bool
forBot = ("/r" `T.isPrefixOf`) . T.toLower . messageText

rollmessage :: Message -> Either a T.Text -> T.Text
rollmessage m diceroll =
  case diceroll of
    Left _ -> T.unlines usage
    Right rolled -> (userName . messageAuthor $ m) <> " rolled a " <> rolled

usage :: [T.Text]
usage =
    [ "Usage:"
    , " /r <expr>"
    , ""
    , "  where <expr> is a simple mathematical expression involving"
    , "  integers and die rolling primitives of the form [<n>]d<s>."
    , "  <n> is the number of times to roll (default is 1) and <s> is"
    , "  the number of sides on the die to roll."
    , ""
    , "  For example:"
    , "  $ dice \"2d10 + 2 * (d100 / d6)\""
    ]
