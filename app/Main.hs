module Main where

import Bmp.Bmp
import Vec3

import Render

import Control.Parallel.Strategies

import qualified Data.ByteString.Builder as BSB
import qualified System.Environment as ENV
import System.IO

procArgs :: [String] -> (String, Int, Int)
procArgs (name : widthS : heightS :  _) = 
  let width = read widthS :: Int
      height = read heightS :: Int
  in
    (name, width, height)
procArgs _ = ("render.bmp", 800, 450)



generateScene :: Handle -> Int -> Int -> IO ()
generateScene hFile width height =
  BSB.hPutBuilder hFile $ createBitmap img where
    img = renderScene width height


main :: IO ()
main = do
  args <- ENV.getArgs
  let (fileName, width, height) = procArgs args
  hFile <- openBinaryFile fileName WriteMode
  generateScene hFile width height
  hFlush hFile
  hClose hFile
