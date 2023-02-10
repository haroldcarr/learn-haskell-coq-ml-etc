{-# LANGUAGE ForeignFunctionInterface #-}

module Lib where

------------------------------------------------------------------------------
import           Data.Word
import           Foreign.C.String   (CString, newCString, peekCString)
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Marshal.Array
------------------------------------------------------------------------------

------------------------------------------------------------------------------

main :: IO ()
main  = do
  putStrLn "\n--------------------------------------------------"
  factMain
  populationMain
  stringVecMain
  putStrLn "--------------------------------------------------\n"

------------------------------------------------------------------------------

-- Wrapping the fact.rs module...
foreign import ccall "fact"
  c_fact :: CULong -> CULong

fact :: Int -> Int
fact  = fromIntegral . c_fact . fromIntegral

factMain :: IO ()
factMain  = do
  putStr "fact(5) = "
  print (fact 5)

------------------------------------------------------------------------------

foreign import ccall unsafe "get_strings"
  get_strings :: {-CIntPtr ->-} IO (Ptr CString)

foreign import ccall unsafe "free_string_array"
  free_string_array :: Ptr CString -> CInt -> IO ()

stringVecMain :: IO ()
stringVecMain  = do
  --let len@(CIntPtr l) = CIntPtr 0
  s <- get_strings --len
  [p1,p2,p3] <- peekArray 3 s
  putStr "Vec = "
  print (p1, p2, p3)
  peekCString p1 >>= print
  peekCString p2 >>= print
  peekCString p3 >>= print
  free_string_array s 3

------------------------------------------------------------------------------

data ZipCodeDatabase

foreign import ccall unsafe "zip_code_database_new"
  zip_code_database_new :: IO (Ptr ZipCodeDatabase)

foreign import ccall unsafe "&zip_code_database_free"
  zip_code_database_free :: FunPtr (Ptr ZipCodeDatabase -> IO ())

foreign import ccall unsafe "zip_code_database_populate"
  zip_code_database_populate :: Ptr ZipCodeDatabase -> IO ()

foreign import ccall unsafe "zip_code_database_population_of"
  zip_code_database_population_of :: Ptr ZipCodeDatabase -> CString -> Word32

createDatabase :: IO (Maybe (ForeignPtr ZipCodeDatabase))
createDatabase  = do
  ptr <- zip_code_database_new
  if ptr /= nullPtr
    then do
      foreignPtr <- newForeignPtr zip_code_database_free ptr
      return (Just foreignPtr)
    else
      return Nothing

populate :: Ptr ZipCodeDatabase -> IO ()
populate  = zip_code_database_populate

populationOf :: Ptr ZipCodeDatabase -> String -> IO Word32
populationOf db zip0 = do
  zip_str <- newCString zip0
  return (zip_code_database_population_of db zip_str)

populationMain :: IO ()
populationMain  = do
  db <- createDatabase
  case db of
    Nothing -> putStrLn "Unable to create database"
    Just ptr -> withForeignPtr ptr $ \database -> do
        populate database
        pop1 <- populationOf database "90210"
        pop2 <- populationOf database "20500"
        putStr "population = "
        print (pop1, pop2, pop1 - pop2)

