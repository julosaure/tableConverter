
module Main where

import Data.Char
import Data.Int 
import Data.List
import Data.Array
import qualified Data.ByteString as L
import qualified Data.ByteString.Char8 as C
import Text.Regex.TDFA    
import System.IO
--import Data.IO

main = do
  --contents <- L.readFile "../data/dbo.AgentActivityLog.sql.clean.unix"
  hIn <- openFile "../data/dbo.CallDetail.clean.ungo" ReadMode
  hOut <- openFile "../data/dbo.CallDetail.clean.ungo.dated.sql" WriteMode
  
  --hSetNewlineMode hIn NewlineMode { inputNL  = CRLF, outputNL = CRLF }
  --hSetNewlineMode hOut NewlineMode { inputNL  = CRLF, outputNL = CRLF }

  parseFile hIn hOut empty
  
  hClose hIn
  hClose hOut
         
parseFile :: Handle -> Handle -> String -> IO ()
parseFile inFile outFile acc = 
  do inEof <- hIsEOF inFile
     if inEof
       then return () 
       else do line <- hGetLine inFile
               if (isPrefixOf "INSERT" line)
                 then 
                 do
                   let acc_ = acc ++ ";"
                   let lineToParse = C.pack acc_
                   let parsedLine = C.unpack $ parseLine lineToParse
                   --putStrLn parsedLine 
                   hPutStrLn outFile parsedLine
                   parseFile inFile outFile line  
                 else
                   parseFile inFile outFile (acc ++ "\n" ++ line)
            
      
parseLine :: L.ByteString -> L.ByteString    
parseLine line =
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
  
