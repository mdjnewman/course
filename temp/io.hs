import System.Environment
import Control.Applicative
import Control.Monad

main :: IO ()
main = getArgs >>= \args -> 
    case args of 
      [filename] -> run filename
      _          -> putStrLn "Usage: ..."
            
run :: String -> IO ()
--run filename = 
  -- readFile filename >>= \content -> 
  -- getFiles (lines content) >>= \results ->
  ----could put these in here, this is just a pattern
  ----getChar >>= \c ->
  ----print c >>= \_ ->
  -- printFiles results
  --OR:
run filename = do
  content <- readFile filename
  results  <- getFiles (lines content)
  printFiles results
  
getFiles :: [String] -> IO [(String, String)]
getFiles = mapM getFile

getFile :: String -> IO (String, String)
--getFile filename = (\content -> (filename, content)) <$> readFile filename
--OR
getFile filename = (,) filename <$> readFile filename

printFiles :: [(String, String)] -> IO()
printFiles = mapM_ $ uncurry printFile

printFile :: FilePath -> String -> IO ()
printFile f c = 
  --putStrLn ("-----" ++ f ++ ":") >>= \_ ->
  --putStrLn c
  --OR:
  mapM_ putStrLn ["-----" ++ f ++ ":", c]