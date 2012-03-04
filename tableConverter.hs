
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
  
