module Main where

import Bmp.Bmp
import Vec3

import Render

import Control.Parallel.Strategies

import qualified Data.ByteString.Builder as BSB
import qualified System.Environment as ENV
import System.IO
import Random

procArgs :: [String] -> (String, Int, Int, Int)
procArgs (name : widthS : heightS : nrSamplesS : _) = 
  let width = read widthS :: Int
      height = read heightS :: Int
      nrSamples = read nrSamplesS :: Int
  in
    (name, width, height, nrSamples)
procArgs _ = ("render.bmp", 800, 450, 100)



generateScene :: Handle -> Int -> Int -> Int -> IO ()
generateScene hFile width height nrSamples =
  BSB.hPutBuilder hFile $ createBitmap img where
    img = renderScene width height (newRandom 0x238912723) nrSamples


main :: IO ()
main = do
  args <- ENV.getArgs
  let (fileName, width, height, nrSamples) = procArgs args
  hFile <- openBinaryFile fileName WriteMode
  generateScene hFile width height nrSamples
  hFlush hFile
  hClose hFile
