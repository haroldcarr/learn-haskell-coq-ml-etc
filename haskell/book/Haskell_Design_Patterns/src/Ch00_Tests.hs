module Ch00_Tests where

import Ch05_02_rank_n_types
import Ch05_03_universal_quantification
import Ch05_04_existential_quantification
import Ch05_05_phantom_types
import Ch05_06_gadt
import Ch05_07_typecase
import Ch05_08_dynamic_types_01

main = do
  runTests_Ch05_02
  runTests_Ch05_03
  runTests_Ch05_04
  runTests_Ch05_05
  runTests_Ch05_06
  runTests_Ch05_07
  runTests_Ch05_08_01
