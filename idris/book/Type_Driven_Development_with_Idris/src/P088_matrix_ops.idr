module Main

import Data.Vect

%default total

addRow :  Num numType
       => (x : Vect cols numType)
       -> (y : Vect cols numType)
       -> Vect cols numType
addRow       []        []  = []
addRow (x :: xs) (y :: ys) = x + y :: addRow xs ys

addMatrix :  Num numType
          => Vect rows (Vect cols numType)
          -> Vect rows (Vect cols numType)
          -> Vect rows (Vect cols numType)
addMatrix       []        []  = []
addMatrix (x :: xs) (y :: ys) = addRow x y :: addMatrix xs ys

tam : Vect 3 (Vect 2 Int)
tam = addMatrix [[ 1, 2]
                ,[ 3, 4]
                ,[ 5, 6]]
                [[ 7, 8]
                ,[ 9,10]
                ,[11,12]]

createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

transposeHelper :  (x : Vect n elem)
                -> (xsTrans : Vect n (Vect len elem))
                -> Vect n (Vect (S len) elem)
transposeHelper       []        []  = []
transposeHelper (x :: xs) (y :: ys) = (x :: y) :: transposeHelper xs ys

transposeMat :  Vect m (Vect n elem)
             -> Vect n (Vect m elem)
transposeMat       []  = createEmpties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in (transposeHelper x xsTrans)

ttm1 : Vect 2 (Vect 3 Integer)
ttm1 = transposeMat [ [1,2]
                    , [3,4]
                    , [5,6] ]

ttm2 : Vect 4 (Vect 3 Integer)
ttm2 = transposeMat [ [1, 2, 3, 4]
                    , [5, 6, 7, 8]
                    , [9,10,11,12] ]

tmmil : Vect 3 (Vect 2 Integer)
tmmil =        [[ 1,  2]
               ,[ 3,  4]
               ,[ 5,  6]]

tmmir : Vect 2 (Vect 4 Integer)
tmmir = [[ 7, 8, 9,10]
        ,[11,12,13,14]]

ttm3 : Vect 4 (Vect 2 Integer)
ttm3 = transposeMat tmmir

ttm4 : Bool
ttm4 = ttm3    == [[ 7, 11]
                  ,[ 8, 12]
                  ,[ 9, 13]
                  ,[10, 14]]

multRow' :  Num numType
         => (x : Vect m numType)
         -> (y : Vect m numType)
         -> numType
multRow'       []        []  = 0
multRow' (x :: xs) (y :: ys) = x * y + multRow' xs ys

multRow :  Num numType
        => (x : Vect m numType)
        -> (ysTrans : Vect p (Vect m numType))
        -> Vect p numType
multRow x       []  = []
multRow x (y :: ys) = multRow' x y :: multRow x ys

multMatrix :  Num numType
           => Vect n (Vect m numType)
           -> Vect m (Vect p numType)
           -> Vect n (Vect p numType)
multMatrix       []  ys = []
multMatrix (x :: xs) ys =
  let ysTrans = transposeMat ys
  in ((multRow x ysTrans) :: multMatrix xs ys)

tmmr : Vect 3 (Vect 4 Integer)
tmmr = [[ 29, 32, 35, 38]
       ,[ 65, 72, 79, 86]
       ,[101,112,123,134]]

tmm : Bool
tmm = multMatrix tmmil tmmir == tmmr
