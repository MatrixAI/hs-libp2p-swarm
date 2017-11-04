{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.LibP2P.Swarm where

import qualified Data.IP                   as IP
import qualified Data.Multiaddr            as MA
import qualified Data.Multiaddr.Port       as MA
import qualified Data.MultiaddrPart        as MA
import qualified Network.Socket            as NS
import qualified System.IO.Streams         as Streams

import           Data.ByteString           as BS
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import           Network.Socket.ByteString
import           System.IO.Streams         (InputStream, OutputStream)

data Conn
data Config

type ByteStream = (InputStream BS.ByteString, OutputStream BS.ByteString)
type RawStream = ByteStream
newtype Protocol = Protocol [Text] deriving (Show, Eq)
-- TODO: instance Serialize Protocol where

class HasConnType tpt where
  acceptConn  :: MA.Multiaddr -> IO (tpt Conn)
  dialConn    :: MA.Multiaddr -> IO (tpt Conn)
  closeConn   :: tpt Conn -> IO ()
  localAddr   :: tpt Conn -> MA.Multiaddr
  remoteAddr  :: tpt Conn -> MA.Multiaddr
  rawStream   :: tpt Conn -> IO RawStream

data ServiceId = SIDText Text
               | SIDMultiaddr MA.Multiaddr

-- TODO: is there an instance of Monoid for MuxedConnections? (where mappend creates a new muxed stream on the raw stream, and mempty is the rawstream itself with no muxstreams)
class Muxer m where
   muxingProtocol  :: m -> Protocol
   readMuxedStream  :: m -> InputStream BS.ByteString -> IO (ServiceId, BS.ByteString)
   writeMuxedStream :: m -> (ServiceId, BS.ByteString) -> OutputStream BS.ByteString -> IO ()

class (HasConnType tpt, Muxer m) => MuxedConnection tpt m where
  openMuxedStream :: tpt Conn -> m -> ServiceId -> IO ByteStream
  recv :: tpt Conn -> m -> IO (ServiceId, IO ByteStream) 


-- class Demuxer m where
--   demuxMuxedStream :: (m MuxedStream) -> Services -> IO [(IO ServiceHandler)]

-- data Swarm {
--   registeredServices :: [Service],
--   peers
--   discovery

-- data Service {
--   serviceName :: Text
--   runService  :: ByteStream -> IO ()
-- }

-- -- both of these services are on the host
-- service1 :: IO (m Conn) -> IO Service
-- service1 getConn = do
--   ...
--   conn <- getConn
--   stream1 <- newMuxedStream conn
--   write stream2

-- service2 :: IO ()
--   ...
--   --- this gets the same connection
--   conn <- getConn
--   stream2 <- newMuxedStream conn
--   write stream2

-- run these services in parallel

-- muxer :: ByteStream -> IO (ByteStream)


-- data Node = Node {
--     conn :: m Conn
--     listener :: m Listener
--     someComposition' :: (LibP2P m) =>  m Conn -> m Listener -> IO ()
-- }

-- data Cons = (A B) (Int -> Int) | (B X) (Int -> Int)

-- f (l(\(c "Hello") -> 1) A')
