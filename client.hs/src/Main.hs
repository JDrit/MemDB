import qualified Messages.GetRequest as Get
import qualified Messages.PutRequest as Put
import Messages.Type
import qualified Messages.ClientRequest as ClientRequest
import qualified Messages.ClientResponse as ClientResponse
import Text.ProtocolBuffers.Header (uFromString)
import Text.ProtocolBuffers.WireMessage (messageGet, messagePut)

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv)

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B

import Control.Monad.Random
import Control.Monad
import qualified Data.Char as Char

import Control.Concurrent
import Control.Exception (finally)

sampleGet :: IO ClientRequest.ClientRequest
sampleGet = do key <- randStr 10
               return ClientRequest.ClientRequest {
                        ClientRequest.type' = GET,
                        ClientRequest.get = Just Get.GetRequest { Get.key = uFromString key },
                        ClientRequest.put = Nothing,
                        ClientRequest.remove = Nothing 
                      }

samplePut :: IO ClientRequest.ClientRequest
samplePut = do key   <- randStr 10
               value <- randStr 200
               return ClientRequest.ClientRequest {
                          ClientRequest.type' = PUT,
                          ClientRequest.get = Nothing,
                          ClientRequest.put = Just Put.PutRequest {
                                                       Put.key = uFromString key,
                                                       Put.value = uFromString value
                                                     },
                          ClientRequest.remove = Nothing
                        }

randStr :: Int -> IO String
randStr l = evalRandIO $ replicateM l rand >>= \values -> return $ map Char.chr values
  where rand = getRandomR (65, 90)

toStrict :: BL.ByteString -> B.ByteString
toStrict = B.concat . BL.toChunks

toLazy :: B.ByteString -> BL.ByteString
toLazy bs = BL.fromChunks [bs]

sendGet :: Socket -> IO ClientResponse.ClientResponse
sendGet sock = do
  request <- samplePut
  send sock $ toStrict (messagePut request)
  response <- recv sock 4096
  case messageGet (toLazy response) of 
    Left err          -> error err
    Right (msg, buff) -> sendGet sock >> return msg

client :: String -> Int -> IO ()
client host port = withSocketsDo $ do
  addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
  let serverAddr = head addrInfo
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  connect sock (addrAddress serverAddr)
  sendGet sock
  sClose sock


forkClient :: IO () -> IO (MVar ())
forkClient io = do
  mvar <- newEmptyMVar
  forkIO (io `finally` putMVar mvar ())
  return mvar

forkLoop :: IO () -> Int -> IO (MVar ())
forkLoop io 1      = forkClient io
forkLoop io count = forkClient io >> forkLoop io (count - 1)

main :: IO ()
main = do 
  mvar <- forkLoop (client "localhost" 4000) 1000
  takeMVar mvar
