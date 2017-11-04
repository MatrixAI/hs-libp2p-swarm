{-# LANGUAGE GADTs #-}

module Network.LibP2P.TCP where

import qualified Data.ByteString           as BS
import qualified Data.IP                   as IP
import qualified Data.Multiaddr            as MA
import qualified Data.Multiaddr.Port       as MA
import qualified Data.MultiaddrPart        as MA
import           Network.LibP2P.Swarm      (Conn, HasConnType(..))
import qualified Network.Socket            as NS
import           System.IO.Streams         (InputStream, OutputStream)
import qualified System.IO.Streams.Network as Streams

data TCP a where
  TCPConn ::  NS.Socket -> MA.Multiaddr -> MA.Multiaddr -> TCP Conn

instance HasConnType TCP where
  acceptConn = acceptConn'
  dialConn   = dialConn'
  closeConn  = closeConn'
  localAddr  = localAddr'
  remoteAddr = remoteAddr'
  rawStream  = rawStream'

acceptConn' :: MA.Multiaddr -> IO (TCP Conn)
acceptConn' ma = do
  lsock <- listen ma
  conn  <- accept lsock
  return conn
    where
      listen :: MA.Multiaddr -> IO NS.Socket
      listen ma = do
        let sa = multiAddrToSockAddr ma
        s <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
        NS.bind s sa
        NS.listen s 5
        return s
      accept :: NS.Socket -> IO (TCP Conn)
      accept s = do
        (ns, ra) <- NS.accept s
        la <- NS.getSocketName s
        return $ TCPConn ns (sockAddrToMultiAddr la) (sockAddrToMultiAddr ra)

dialConn' :: MA.Multiaddr -> IO (TCP Conn)
dialConn' ma = do
  let ra = multiAddrToSockAddr ma
  s <- case ra of
            NS.SockAddrInet _ _ -> NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
            --NS.SockAddrInet6 _ _ _ _ -> NS.socket NS.AF_INET6 NS.Stream NS.defaultProtocol
  la <-NS.getSocketName s
  NS.connect s ra
  return $ TCPConn s (sockAddrToMultiAddr la) ma

closeConn' :: TCP Conn -> IO ()
closeConn' (TCPConn s _ _) = NS.close s

localAddr' :: TCP Conn -> MA.Multiaddr
localAddr' (TCPConn s la ra) = la

remoteAddr' :: TCP Conn -> MA.Multiaddr
remoteAddr' (TCPConn s la ra) = ra

rawStream' :: TCP Conn -> IO (InputStream BS.ByteString, OutputStream BS.ByteString)
rawStream' (TCPConn s _ _) = do
  (is, os) <- Streams.socketToStreams s
  return $ (Streams.lockingInputStream is, Streams.lockingOutputStream os)

sockAddrToMultiAddr :: NS.SockAddr -> MA.Multiaddr
sockAddrToMultiAddr (NS.SockAddrInet p ha)
  = MA.Multiaddr [MA.IP4m ip4Addr, MA.TCPm portAddr]
    where
      ip4Addr = IP.fromHostAddress ha
      Just portAddr = MA.toPort p

sockAddrToMultiAddr (NS.SockAddrInet6 p _ ha _)
  = MA.Multiaddr [MA.IP6m ip6Addr, MA.TCPm portAddr]
    where
      ip6Addr = IP.fromHostAddress6 ha
      Just portAddr = MA.toPort p

multiAddrToSockAddr ma
  = case MA.parts ma of
         [MA.IP4m ip4, MA.TCPm port] -> let ha = IP.toHostAddress ip4
                                            p = MA.fromPort port
                                        in NS.SockAddrInet (fromInteger p) ha
         [MA.IP6m ip6, MA.TCPm port] -> let ha = IP.toHostAddress6 ip6
                                            p = MA.fromPort port
                                        in NS.SockAddrInet6 (fromInteger 0) (fromInteger p) ha (fromInteger 0)

