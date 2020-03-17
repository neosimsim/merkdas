import           Control.Concurrent      (forkIO)
import           Control.Exception       (handle, Exception)
import           Control.Monad           (forever)
import qualified Data.ByteString.Lazy    as L

-- I just tried this today GHC 6.10.3
--
-- I get the following error:
-- Compressor.hs:15:6:
-- Ambiguous type variable `e' in the constraint:
-- `GHC.Exception.Exception e'
-- arising from a use of `handle' at Compressor.hs:15:6-17
-- Probable fix: add a type signature that fixes these type variable(s)
-- Failed, modules loaded: none.

-- import Control.OldException instead of Control.Exception
-- (Exception handling has changed in GHC 6.10(.2?), as I learned from another comment from some chapters ago.) 
-- It should work then.

-- It's better to switch to the new exception-library. Simply write:
-- ... handle (\(e::SomeException) -> print e) $ do ...

-- and also to fix the handle line:
--
-- _____handle ((\e -> print e) :: IOError -> IO ()) $ do

import           Codec.Compression.GZip  (compress)

main = do
  putStr "Enter a file to compress> "
  maybeLine <- getLine
  case maybeLine of
    "" -> return ()
    name -> do
      handle $ do
        content <- L.readFile name
        forkIO (compressFile name content)
        return ()
      main
  where
    compressFile path = L.writeFile (path ++ ".gz") . compress
