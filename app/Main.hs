{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- import Data.Word
import Control.DeepSeq
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import GHC.Generics
import Foreign.C
import Foreign
import Graphics.Rendering.FreeType.Internal
import qualified Graphics.Rendering.FreeType.Internal.GlyphSlot as G
import qualified Graphics.Rendering.FreeType.Internal.GlyphMetrics as M
import qualified Graphics.Rendering.FreeType.Internal.Vector as V
import Graphics.Rendering.FreeType.Internal.PrimitiveTypes
import Graphics.Rendering.FreeType.Internal.Face hiding (height)
import Graphics.Rendering.FreeType.Internal.Bitmap hiding (width)
import Control.Monad
import Data.Binary
import Codec.Picture
import Codec.Picture.Types
import Data.Binary.Put
import Data.Vector.Storable (toList, Vector)

main :: IO ()
main = do
  -- xs <- mapM writeRGBA [("LuckiestGuy.ttf", 36)]
  -- print xs
  toRGBA "ship.png"
  
toRGBA fn0 = do
  ImageRGBA8 (img :: Image PixelRGBA8) <- either error id <$> readImage fn0
  let fn = fn0 ++ ".rgba"
  encodeFile fn $ B.pack $ fmap packPixel $ toList $ imageData img
  print (fn, (imageWidth img, imageHeight img))

writeRGBA (fn,sz) = do
  font <- loadTTF (fn,sz)
  let outFn ch = fn ++ "." ++ show sz ++ "." ++ show (fromEnum $ chChar ch) ++ ".rgba"
  let xs = [ (a,b,c,d,(outFn ch,(e,f))) | (ch@(Ch a b c d e f), bs) <- font ]
  sequence_ [ write (outFn ch) b | (ch, b) <- font ]
  return ((fn,sz), xs)
  
exitor s f = f >>= \i -> when (i /= 0) $ error $ "error:" ++ s

-- writing out full rgba value
write fn b = do
  encodeFile fn $ runPut $ sequence_
    [ let w = B.index b i in put w >> put w >> put w >> put w
    | i <- [ 0 .. B.length b - 1 ] ]

data Ch = Ch
  { chChar :: !Char
  , chLeft :: !Int
  , chTop :: !Int
  , chAdv :: !Int
  , chWidth :: !Int
  , chHeight :: !Int
  } deriving (Show, Eq, Ord, Generic)

instance NFData Ch

loadTTF :: (FilePath, Word) -> IO [(Ch, ByteString)]
loadTTF (fn, sz) =  do
  putStrLn $ "loading:" ++ show (fn, sz)
  alloca $ \plib ->
    alloca $ \pface -> do
    exitor "ft_Init_FreeType" $ ft_Init_FreeType plib
    pfn <- newCString fn
    lib <- peek plib
    exitor "ft_New_Face" $ ft_New_Face lib pfn 0 pface
    face <- peek pface
    exitor "ft_Set_Char_Size" $ ft_Set_Char_Size face 0 (fromIntegral sz * 64) 96 0
    let
      loadTTFChar ch = do
        exitor "ft_Load_Char" $
          ft_Load_Char face (fromIntegral $ fromEnum ch) ft_LOAD_RENDER
        gs <- peek $ glyph face
        m <- peek $ G.metrics gs
        top <- fromIntegral <$> peek (G.bitmap_top gs)
        left <- fromIntegral <$> peek (G.bitmap_left gs)
        adv <- (fromIntegral . flip div 64 . V.x) <$> peek (G.advance gs)
        let w = fromIntegral (M.width m `div` 64)
        let h = fromIntegral (M.height m `div` 64)
        buf <- buffer <$> (peek =<< (G.bitmap <$> peek (glyph face)))
        bs <- B.packCStringLen (buf, w * h)
        return (Ch
                { chChar       = ch
                , chWidth      = w
                , chHeight     = h
                , chLeft       = left
                , chTop        = top
                , chAdv        = max adv (left + w)
                }, bs)
    
    xs <- mapM loadTTFChar [' ' .. '~']
    let maxt = maximum $ fmap (chTop . fst) xs
    let xs' = fmap (\(a,b) -> (a{ chTop = maxt - chTop a - 1 }, b)) xs
    deepseq xs' $ return xs'
