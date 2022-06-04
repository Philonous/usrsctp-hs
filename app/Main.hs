import qualified Sctp
import Network.Socket


main :: IO ()
main = do
  _ <- Sctp.test1
  return ()
