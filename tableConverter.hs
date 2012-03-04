
module Main where

import Data.Int 
import Data.List
import Data.Array
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import Text.Regex.TDFA    

main = do
  contents <- L.readFile "../data/dbo.AgentActivityLog.sql.clean.unix"
  let n = head $ L.unpack $ C.singleton '\n'
  let r = head $ L.unpack $ C.singleton '\r'
  let pv = head $ L.unpack $ C.singleton ';'
      
  let myLines = L.split n contents
  let myParsedLines = map parseLine2 myLines
  
  let myParsedEndedLines = map (\bs -> L.snoc (L.snoc bs pv) n ) myParsedLines
  let result = L.concat  myParsedEndedLines
  
  L.writeFile "../data/dbo.AgentActivityLog.sql.clean.unix.dated" result
    
parseLine2 :: L.ByteString -> L.ByteString    
parseLine2 line =
  -- pattern of type '20110629 05:00:25:000'   213 6 51 01 76 22
  let timestampPattern = "'[0-9]{8} ([0-9]{2}:){3}[0-9]{3}'"
      lOffLen = getAllMatches (line =~ timestampPattern :: AllMatches [] (Int, Int)) --[(MatchOffset, MatchLength)]
  -- for each timestamp that matches in a line,  insert -
  -- we start from the last pattern that match to be able to modify the line using all 
  -- the (off, len) (if we start from begin, the (off,len) are nor more good after 1st replace)
  in  fst $ mapAccumL replacePatternInLine line (reverse lOffLen)
  
replacePatternInLine :: L.ByteString -> (Int, Int) -> (L.ByteString, (Int, Int))      
replacePatternInLine line (off_, len_) = 
  let tiret =  C.singleton '-' 
      off = fromIntegral off_
      slice from to l =  L.take (to-from+1) (L.drop from l)
  -- insert tiret in 2011-06-29
  in (L.concat [L.take (off+5) line, tiret, slice (off+5) (off+6) line, tiret, L.drop (off+7) line], (off_, len_))
  
parseLine :: L.ByteString -> L.ByteString  
parseLine line = 
  let virgule = head $ L.unpack $ C.singleton ',' 
      words = L.split virgule line
      slice from to l =  take (to-from+1) (drop from l)
      len = length words
      words2 = if (length words) == 39
               then   (take 20 words) ++ [parseWord $ words !! 20, parseWord $ words !! 21] ++
               (slice 22 31 words) ++ [parseWord $ words !! 31, parseWord $ words !! 32] ++
               (slice 33 34 words) ++ [parseWord $ words !! 34] ++ (drop 35 words) 
               else take 2 words
  in L.intercalate (L.singleton virgule) words2
  
parseWord :: L.ByteString -> L.ByteString     
parseWord word =
  let (offset1,length1) = word =~ "[0-9]{8} " :: (Int, Int)
      offset = fromIntegral offset1
      length = fromIntegral length
      slice from to l =  L.take (to-from+1) (L.drop from l) 
      tiret = C.singleton '-'
  in if offset >= 0 
     -- we found a timestamp of the form 20110912   
     -- we rewrite in in 2011-09-12
     then L.concat [L.take offset word, slice offset (offset+3) word, tiret, slice (offset+4) (offset+5) word, tiret, L.drop (offset+6)  word] 
     else  word
  
-- parseDate :: L.ByteString -> IO ()
--parseDates contents = do
--  let myLines = L.split (L.pack '\n') contents  
--  putStrLn "Here are the lines:"
--  mapM_ L.putStrLn  myLines
  