module Main (main) where
import Control.Applicative
import Control.Monad
import Data.Random.Dice
import System.Environment
import System.Exit
import Control.Monad (when, forM_)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import UnliftIO (liftIO)
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

  let activity = Activity { activityName = "ping-pong"
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
    case filter isTextChannel chans of
      (c:_) -> do _ <- restCall $ R.CreateMessage (channelId c) "Hello! I will reply to pings with pongs"
                  pure ()
      _ -> pure ()

-- If an event handler throws an exception, discord-haskell will continue to run
eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
      MessageCreate m -> when (not (fromBot m) && isPing m) $ do
        _ <- restCall (R.CreateReaction (messageChannel m, messageId m) "eyes")
        threadDelay (4 * 10^(6 :: Int))
        _ <- restCall (R.CreateMessage (messageChannel m) "Pong!")
        pure ()
      _ -> pure ()

isTextChannel :: Channel -> Bool
isTextChannel (ChannelText {}) = True
isTextChannel _ = False

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isPing :: Message -> Bool
isPing = ("ping" `T.isPrefixOf`) . T.toLower . messageText

usage =
    [ "Usage:"
    , "  dice <expr>"
    , ""
    , "  where <expr> is a simple mathematical expression involving"
    , "  integers and die rolling primitives of the form [<n>]d<s>."
    , "  <n> is the number of times to roll (default is 1) and <s> is"
    , "  the number of sides on the die to roll."
    , ""
    , "  For example:"
    , "  $ dice \"2d10 + 2 * (d100 / d6)\""
    ]


main2 = do
    expr <- concat <$> getArgs
    when (null expr) exitWithUsage
    result <- rollEm expr
    either exitWithErr putStrLn result

printUsage = mapM_ putStrLn usage

exitWithUsage = do
    printUsage
    exitWith (ExitFailure 1)

printErr e = do
    print e
    putStrLn ""
    printUsage

exitWithErr e = do
    printErr e
    exitWith (ExitFailure 2)
