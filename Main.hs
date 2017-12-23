import Network.SSH.Client.LibSSH2
import Network.SSH.Client.LibSSH2.Foreign
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as C
import System.Environment
import System.FilePath
import Control.Concurrent.Async

main :: IO ()
main = do
    -- initialize True >>= print
    home <- getEnv "HOME"
    let known_hosts = home </> ".ssh" </> "known_hosts"
        public = home </> ".ssh" </> "id_rsa.pub"
        private = home </> ".ssh" </> "id_rsa"
    print known_hosts
    status <- withSSH2 known_hosts public private "" "root" "ams1" 22 sessionActions
    print status

sessionActions :: Session -> IO (Int, ())
sessionActions s = do
    print "got session"
    withChannelBy
        (directTcpIpEx s "localhost" 8081 "" 0)
        id
        channelActions

channelActions :: Channel -> IO ()
channelActions ch = do
    print "got channel"
    writer <- async $ do
        writeChannel ch $ C.pack "world\n"
        channelSendEOF ch
    out <- readAllChannel ch
    B.putStr out
    wait writer

