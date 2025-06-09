module SoundLib (
    WaveData,
    loadWav,
    playWavFromMemory
    ) where


import Data.Bits
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.C.Types (CInt(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdin)

data WaveData = WaveData {bytes :: BS.ByteString}

-- Windows PlaySound API from winmm.dll
foreign import stdcall unsafe "windows.h PlaySoundA"
  c_PlaySound :: Ptr a -> Ptr () -> CInt -> IO Bool

-- Constants for sound flags
sND_ASYNC, sND_MEMORY :: CInt
sND_ASYNC  = 0x0001
sND_MEMORY = 0x0004

-- Play a sound from memory using ByteString
playWavFromMemory :: WaveData -> IO ()
playWavFromMemory (WaveData bs) =
  BSU.unsafeUseAsCString bs $ \ptr -> do
    _ <- c_PlaySound ptr nullPtr (sND_ASYNC .|. sND_MEMORY)
    return ()

loadWav :: String -> IO WaveData
loadWav file = do
  hSetBuffering stdin NoBuffering
  BS.readFile file >>= return . WaveData
  
