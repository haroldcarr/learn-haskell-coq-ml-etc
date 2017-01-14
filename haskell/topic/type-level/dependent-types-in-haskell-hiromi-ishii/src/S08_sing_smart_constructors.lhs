> module S08_sing_smart_constructors where

Smart constructors
------------------

`singletons` generates *smart constructor* for singletons

> sZ :: SNat Z
> sZ = SZ -- same as the SZ

> sS :: SNat n -> SNat (S n)
> sS n = case singInstances n of SingInstance -> SS n

Using `sZ` and `sS` instead of `SZ`and `SS`
- sometimes reduces number of calling `singInstance` function.


On the efficiency of `singInstance`
----------------------------------

`singInstance k` constructs instance dictionary by recuring on k
- runtime cost is linear in `k`
