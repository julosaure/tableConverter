
module Main where

import Data.Char
import Data.Int 
import Data.List
import Data.Array
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import Text.Regex.TDFA    

main = do
  --contents <- L.readFile "../data/dbo.AgentActivityLog.sql.clean.unix"
  contents <- L.readFile "../data/dbo.CallDetail.clean"
  let n = head $ L.unpack $ C.singleton '\n'
  let r = head $ L.unpack $ C.singleton '\r'
  let pv = head $ L.unpack $ C.singleton ';'
      
  --let myLines = L.split n contents
  let myLines = splitCrlf [] contents
  let myParsedLines = map parseLine2 myLines
  
  let myParsedEndedLines = map (\bs -> L.snoc (L.snoc bs pv) n ) myParsedLines
  let result = L.concat  myParsedEndedLines
  
  --L.writeFile "../data/dbo.AgentActivityLog.sql.clean.unix.dated" result
  L.writeFile "../data/dbo.CallDetail.clean.dated.sql" result  
  
splitCrlf :: [L.ByteString] -> L.ByteString -> [L.ByteString]  
splitCrlf acc line = 
  let pos = searchCrlf line
  in if pos == -1 
     then acc ++ [line]
     else if pos == 0 
          then splitCrlf (acc ++ [L.empty]) (L.drop 2 line)
          else splitCrlf (acc ++ [L.take pos line]) (L.drop (pos+2) line)
          
               
searchCrlf :: L.ByteString -> Int64
searchCrlf line = searchCrlf_ 0 line

searchCrlf_ :: Int64 -> L.ByteString -> Int64
searchCrlf_ cpt line =      
  let cr = fromIntegral (ord '\r')
      lf = fromIntegral (ord '\n')
      posCr =  L.elemIndex cr line
  in case posCr of Nothing -> -1
                   Just pos -> if (L.index line (pos+1)) == lf
                               then pos
                               else searchCrlf_ (cpt+pos) (L.drop pos line) 

parseLine2 :: L.ByteString -> L.ByteString    
parseLine2 line =
  -- pattern of type '20110629 05:00:25:000'   213 6 51 01 76 22
  let timestampPattern = "'[0-9]{8} ([0-9]{2}:){3}[0-9]{3}'"
      lOffLen = getAllMatches (line =~ timestampPattern :: AllMatches [] (Int, Int))
  -- for each timestamp that matches in a line,  insert -
  -- we start from the last pattern that match to be able to modify the line using all 
  -- the (off, len) (if we start from begin, the (off,len) are nor more good after 1st replace)
  in  fst $ mapAccumL replacePatternInLine line (reverse lOffLen)
  
replacePatternInLine :: L.ByteString -> (Int, Int) -> (L.ByteString, (Int, Int))      
replacePatternInLine line (off_, len_) = 
  let tiret =  C.singleton '-' 
      off = fromIntegral off_
      slice from to l =  L.take (to-from+1) (L.drop from l)
  -- insert tiret in 2011-06-29, and remove milliseconds :000
  in (L.concat [L.take (off+5) line, tiret, slice (off+5) (off+6) line, tiret, slice (off+7) (off+17) line, L.drop (off+22) line], (off_, len_))
  
