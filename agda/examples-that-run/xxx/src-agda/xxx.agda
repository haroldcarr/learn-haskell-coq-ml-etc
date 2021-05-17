module xxx where

open import Agda.Builtin.Unit using (⊤)
open import IO                as IO hiding (_>>=_; _>>_)
import      IO.Primitive      as Primitive
open import hcio              as HCIO

main : Primitive.IO ⊤
main = run do
  IO.putStrLn "enter file name:"
  f ← HCIO.getLine

  IO.putStrLn "enter file contents"
  c ← HCIO.getLine

  IO.writeFile f c
  c' ← IO.readFiniteFile f
  IO.putStrLn c'

  std ← IO.readFiniteFile "spacetrack-data-2021-05-31.txt"
  IO.putStrLn std

  IO.putStrLn "Bye"
