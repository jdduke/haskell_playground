import Control.Arrow
import System.Environment
import System.Directory
import System.FilePath
import System.IO
import Data.List

-------------------------------

type Playlist = [FilePath]

main = do
  (command:playlist:args) <- getArgs
  let (Just action) = lookup command dispatch
  playlistSongs <- songs playlist
  putStrLn playlist
  action playlistSongs args

-------------------------------

copyPL   :: Playlist -> [String] -> IO ()
copyPL playlist [dstPath] = do
  putStrLn ("Copying to " ++ normalise dstPath)
  songMap (\song -> copyFile song $ songPath dstPath song) playlist

movePL   :: Playlist -> [String] -> IO ()
movePL playlist [dstPath] = do
  putStrLn ("Moving to " ++ normalise dstPath)
  songMap (\song -> do copyFile   song $ songPath dstPath song
                       removeFile song) playlist

deletePL :: Playlist -> [String] -> IO ()
deletePL playlist _ = do
  putStrLn "Deleting "
  songMap removeFile playlist

showPL   :: Playlist -> [String] -> IO ()
showPL playlist _ = do
  putStrLn $ unlines playlist

-------------------------------

curry2 :: (a -> b) -> (c -> d) -> a -> c -> (b,d)
curry2 f g a c = (f a, g c)

songPath :: FilePath -> FilePath -> FilePath
--songPath = uncurry.combine $ (curry2 normalise takeFileName)
--songPath = uncurry combine $ arr (\x y -> (x,y)) >>> normalise *** takeFileName
--songPath = arr (\x y -> (x,y)) >>> (first normalise) >>> (second takeFileName) >>> arr (uncurry combine)
songPath = (\src dst -> combine (normalise src) (takeFileName dst))

songsM3U :: String -> Playlist
songsM3U contents = filter songFilter $ map normalise $ lines contents
  where
songFilter str | null str        = False
               | str !! 0 == '#' = False
               | otherwise       = True

dispatch :: [(String, Playlist -> [String] -> IO ())]
dispatch =  [  ("move",   movePL)
             , ("delete", deletePL)
             , ("show",   showPL)
             , ("copy",   copyPL) ]

dispatchExt :: [(String, String -> [FilePath])]
dispatchExt = [ (".m3u", songsM3U) ]

songs :: String -> IO [FilePath]
songs plName = do
  plContents <- readFile plName
  let (Just plParse) = lookup (takeExtension plName) dispatchExt
  return (plParse plContents)

songMap :: (FilePath -> IO()) -> [FilePath] -> IO ()
songMap f ss = sequence_ $ map f' ss
  where
    showSong song result = putStrLn $ " ... " ++ (takeFileName song) ++ " ... " ++ result
    f' song = catch ((f song) >>= (\_ -> showSong song "success"))
                                  (\_ -> showSong song "failure")
