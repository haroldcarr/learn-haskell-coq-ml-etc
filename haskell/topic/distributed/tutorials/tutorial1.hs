
-- one
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Network.Transport.TCP (createTransport, defaultTCPParameters)

-- two
import Control.Concurrent (threadDelay)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Monad (forever)
import Network.Transport.TCP (createTransport, defaultTCPParameters)

-- misc
import System.Environment
import System.Exit

main :: IO ()
main = getArgs >>= run

run ["1"] = one
run ["2"] = two
run _     = one

one :: IO ()
one = do
    Right transport <- createTransport "127.0.0.1" "10501" defaultTCPParameters

    -- start a running local node
    node <- newLocalNode transport initRemoteTable

    -- start new process
    _ <- forkProcess node $ do

       self <- getSelfPid
       send self "hello world"            -- send message to ourself

       hello <- expect :: Process String  -- receive the message
       liftIO $ putStrLn hello            -- print the message to the console
    return ()

replyBack :: (ProcessId, String) -> Process ()
replyBack (sender, msg) = send sender msg

logMessage :: String -> Process ()
logMessage msg = say $ "handling " ++ msg

two :: IO ()
two = do
    Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
    node <- newLocalNode t initRemoteTable
    -- Spawn a new process on a local node
    forkProcess node $ do
        -- Spawn worker inside one more process on the local node
        echoPid <- spawnLocal $ forever $ do
            -- Test the matches in order against each message in the queue
            receiveWait [match logMessage, match replyBack]

        -- `say` sends a message to the process registered as logger.
        -- By default, this process simply sends the string to stderr.
        say "send some messages"
        send echoPid "hello"
        self <- getSelfPid
        send echoPid (self, "hello world")
        -- like `expect` (waits for a message), but with timeout
        m <- expectTimeout 1000000
        case m of
            -- Die immediately - throws a ProcessExitException with the given reason.
            Nothing  -> die "nothing came back!"
            (Just s) -> say $ "got back " ++ s
        return ()

    -- A 1 second wait. Otherwise the main thread can terminate before
    -- our messages reach the logging process or get flushed to stdio
    liftIO $ threadDelay (1*1000000)
    return ()
