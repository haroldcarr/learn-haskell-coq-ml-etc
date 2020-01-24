import System.Environment
import BC

main :: IO ()
main = do
  args <- getArgs
  run args
