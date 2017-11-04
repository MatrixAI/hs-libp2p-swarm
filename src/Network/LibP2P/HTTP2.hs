module Network.LibP2P.HTTP2 where

import qualified Data.ByteString as BS
import qualified Network.HTTP2     as H2
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Builder as Streams

data Frame = Frame H2.FrameHeader (Maybe H2.FramePayload)

type Stream = (InputStream BS.ByteString, OutputStream BS.ByteString)

data RawStream
data ClientPrefaceSent
data PeerPrefaceReceived
data StreamReady

-- data StreamRunner m where
--   RawStream   :: IO Stream -> StreamRunner RawStream
--   StartStream :: IO Stream -> StreamRunner ClientPrefaceSent
--   RecvPreface :: IO Stream -> StreamRunner ClientPrefaceSent -> StreamRunner PeerPrefaceReceived
--   AckPreface  :: IO Stream -> StreamRunner PeerPrefaceReceived -> StreamRunner StreamReady


data StreamRunner m where
  RawStream   :: IO Stream -> StreamRunner RawStream
  StartStream :: IO Stream -> StreamRunner ClientPrefaceSent
  RecvPreface :: IO Stream -> StreamRunner ClientPrefaceSent -> StreamRunner PeerPrefaceReceived
  AckPreface  :: IO Stream -> StreamRunner PeerPrefaceReceived -> StreamRunner StreamReady

instance Monoid (StreamRunner m) where
  mempty = RawStream 

initStream :: Stream -> StreamRunner m -> IO Stream
initStream s sr = 
 nstance Monoid (StreamRunner m) where
  mempty = RawStream 

initStream :: Stream -> StreamRunner m -> IO Stream
initStream s sr = 
  
startStream :: Stream -> IO 
startStream (is, os) = do
  
  out <- Streams.builderStream os
  Streams.write (Just $ BS.byteString H2.connectionPreface) out
  Streams.write (Just $ BS.byteString settings) out
  Streams.write (Just $ BS.flush) out
  -- 
    where
      settings
        = H2.encodeFrameHeader H2.FrameSettings 
        $ H2.FrameHeader 0 H2.defaultFlags 0


acceptStream :: Stream -> IO ()
acceptStream = do
  undefined

initStreamHandler :: Stream 
