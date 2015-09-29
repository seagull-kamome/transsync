{-# LANGUAGE OverloadedStrings #-}

import Data.List (sortBy, groupBy)
import Data.Bool (bool)
import Data.Function (on)
import System.FilePath
import System.Directory
import Control.Monad
import System.Process

sync :: String -> String -> IO ()
sync src dst = do
  dstfs <- getDirectoryContents dst
  (dirs, fs) <- getDirectoryContents src
                >>= return . filter (not . flip elem [".", ".."])
                >>= foldM (\(x, x') y  -> doesDirectoryExist (src </> y)
                                          >>= return . bool (x, y:x') (y:x, x') ) ([],[])
  let trs = filter (not . null . snd)
            $ map (\xs -> (fst (head xs),
                           filter (\(x, _, _) -> elem x (map snd xs)) transcoders) )
            $ groupBy ((==) `on` fst)
            $ sortBy (compare `on` fst)
            $ map (fmap (drop 1) . splitExtension) fs
  forM_ trs $ \(tr, (fromExt, toExt, transcode):_) ->
               unless (elem (tr <.> toExt) dstfs) $ transcode tr
  forM_ dirs $ \x -> do
    doesDirectoryExist (dst </> x) >>= flip unless (createDirectory (dst </> x))
    sync (src </> x) (dst </> x)
  where
    transcoders = [ (x, x, \y -> copyFile (src </> y <.> x) (dst </> y <.> x))
                  | x <- ["mp3", "aac", "ogg", "m4a" ] ]
                  ++ [ (x, "m4a", transcodeToAAC x) | x <- ["flac", "wav", "wma" ] ]
    transcodeToAAC x y = void $ rawSystem "ffmpeg" [
      "-y",
      "-threads", "0",
      "-i", (src </> y <.> x),
      "-vn",
      "-acodec", "libfdk_aac",
      "-ac", "2",
      "-ar", "44100",
      "-ab", "128k",
      (dst </> y <.> "m4a") ]








