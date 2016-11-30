import Data.Map.Strict(empty, Map)
import qualified Data.Map.Strict as Map
import Text.Read(readMaybe)
import Data.List(isPrefixOf)
import Data.Foldable(forM_, foldlM)

data NatMap = NatMap {
    outbound :: Map Request Port
  , inbound :: Map Request Address
  , nextOpenPort :: Int
} deriving Show

data Address = Address {
      addressIP :: String
    , addressPort :: Port
  } deriving (Show, Eq, Ord)

data Request = Local Address Address
              | Outbound Address Address
              | Inbound Address Address deriving (Show, Eq, Ord)

type Port = Int

source :: Request -> Address
source (Local addr _ ) = addr
source (Outbound addr _ ) = addr
source (Inbound  addr _ ) = addr

destination :: Request -> Address
destination (Local _ addr) = addr
destination (Outbound _ addr) = addr
destination (Inbound  _ addr) = addr

address :: Address -> String
address  (Address ip port)  =  ip ++ ":" ++ show port

--Reimplement splitOn since I can only use base
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _  [] = [[]]
splitOn a xs = let elem = dropWhile (a ==) $ takeWhile (a /=) xs
                   rest = dropWhile (a ==) $ dropWhile (a /=) xs in
                   elem:(splitOn a rest)

makeRequest :: String -> Int -> String -> Int -> Request
makeRequest sourceIP srcPort destinationIP destPort
  | "10" `isPrefixOf` sourceIP  && "10" `isPrefixOf` destinationIP = Local (Address (sourceIP) srcPort) (Address (destinationIP) destPort)
  | "10" `isPrefixOf` sourceIP = Outbound (Address (sourceIP) srcPort) (Address (destinationIP) destPort)
  | otherwise = Inbound (Address (sourceIP) srcPort) (Address (destinationIP) destPort)

parseIPPacket :: String -> Maybe Request
parseIPPacket sourcePacket =
  case splitOn ' ' sourcePacket of
    (sourceIP:sourcePort:destIP:destPort:_) -> do
      sPort <- readMaybe sourcePort
      dPort <- readMaybe destPort
      return (makeRequest sourceIP sPort destIP dPort)
    _ -> Nothing

routerIP :: String
routerIP = "53.0.0.1"

mapLogStatement :: Address -> Address -> Int -> String
mapLogStatement src dest port = "Mapped " ++ address src ++ " to " ++ address dest ++ " assigned " ++ show port

acceptLogStatement :: Address -> Address -> Address -> String
acceptLogStatement src dest internalAddress = "Accept " ++ address src  ++ " " ++ address dest ++ " redirect " ++  address internalAddress

rejectLogStatement :: Address -> Address -> String
rejectLogStatement src dest = "Reject " ++ address src ++ " " ++ address dest

localLogStatement :: Address -> Address -> String
localLogStatement src dest = address src ++  " " ++ address dest ++ " local"

processRequest :: NatMap -> (Maybe Request) -> IO NatMap
processRequest  natMap  Nothing  = putStrLn "Malformed packet, moving on" >> return natMap
processRequest  natMap  (Just req) = do
    case req of
      Local src dest                      -> do
          putStrLn $ localLogStatement src dest
          return natMap
      Outbound src (Address "53.0.0.1" _) -> return natMap
      Outbound src dest                   ->
        case Map.lookup req (outbound natMap) of
            Just a  -> return natMap
            Nothing -> do
              putStrLn $ mapLogStatement src dest (nextOpenPort natMap)
              return $ addToMap req natMap
      Inbound src dest                    ->
        case Map.lookup req (inbound natMap) of
            Just internalAddress -> do
              putStrLn $ acceptLogStatement src dest internalAddress
              return natMap
            Nothing      -> do
              putStrLn $ rejectLogStatement src dest
              return natMap

addToMap :: Request -> NatMap -> NatMap
addToMap req (NatMap outb inb openPort) =
  let outboundRequestMap     = Map.insert req openPort outb
      inboundRequest         = Inbound (destination req) (Address routerIP openPort)
      inboundRequestMap      = Map.insert inboundRequest (source req) inb in
      NatMap outboundRequestMap inboundRequestMap (openPort + 1)

emptyNatMap :: NatMap
emptyNatMap = NatMap empty empty 1025

main :: IO ()
main = do
  input <- fmap lines (readFile "input.txt")
  let requests = fmap parseIPPacket input
  foldlM processRequest emptyNatMap requests
  return ()
