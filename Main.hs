{-# LANGUAGE OverloadedStrings #-}

import Network.SSH.Client.LibSSH2
import Network.SSH.Client.LibSSH2.Foreign
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as C
import System.Environment
import System.FilePath
import Control.Concurrent.Async
import Control.Concurrent
import Network.Socket
import qualified Network.Socket.ByteString.Lazy as LS
import Prelude hiding (getContents, log, hGetContents)
import Control.Logging
-- import Data.Text
import System.IO

main :: IO ()
main = withStderrLogging $ do
    sock <- socket AF_INET Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    bind sock $ SockAddrInet 8081 iNADDR_ANY
    listen sock 5
    log "listening"
    let acceptOne = do
        conn <- accept sock
        log "accepted"
        forkIO $ serve conn
        acceptOne
    acceptOne

serve :: (Socket, SockAddr) -> IO ()
serve conn = do
    home <- getEnv "HOME"
    let known_hosts = home </> ".ssh" </> "known_hosts"
        public = home </> ".ssh" </> "id_rsa.pub"
        private = home </> ".ssh" </> "id_rsa"
    status <- withSSH2 known_hosts public private "" "root" "ams1" 22 $ sessionActions conn
    print status

sessionActions :: (Socket, SockAddr) -> Session -> IO (Int, ())
sessionActions (sock, addr) sess = do
    print "got session"
    (shost, sport) <- case addr of
        SockAddrInet p n -> do
            a <- inet_ntoa n
            return (a, p)
        _ -> error $ show addr
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    args <- getArgs
    let port = read (head args) :: Int
    ret <- withChannelBy
        (directTcpIpEx sess "localhost" port shost (fromIntegral sport))
        id
        (channelActions hdl)
    hClose hdl
    return ret

channelActions :: Handle -> Channel -> IO ()
channelActions hdl ch = do
    writer <- async $ do
        contents <- BSL.hGetContents hdl
        writeAllChannel ch contents
        channelSendEOF ch
        log "sent eof"
    print "getting inbound"
    read <- readChannelToHandle ch hdl 0x100000
    print read
    -- inbound <- readAllChannelNonBlocking ch
    -- print "writing inbound"
    -- LS.sendAll sock inbound
    -- print "waiting for writer"
    wait writer

