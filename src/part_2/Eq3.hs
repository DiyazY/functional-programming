{-
Below you see a definition for 3-valued Boolean. Notice how the constructers (False3,Unk3,True3) are exported together with Bool3.
```
module Bool3 (Bool3(False3,Unk3,True3), (&&&), (|||), not3) where

data Bool3 = False3 | Unk3 | True3 deriving (Eq,Show,Read) 

(&&&) :: Bool3 -> Bool3 -> Bool3
(&&&) x y
 | x == True3 && y == True3 = True3
 | x == False3 || y == False3 = False3
 |otherwise = Unk3

(|||) :: Bool3 -> Bool3 -> Bool3
(|||) x y
 | x == True3 || y == True3 = True3
 | x == False3 && y == False3 = False3
 |otherwise = Unk3

not3 :: Bool3 -> Bool3
not3 x
 | x == True3 = False3
 | x == False3 = True3
 |otherwise = Unk3
```
Here is a definition for MaybeNull which is supposed to represent a database null: either there is a value or a Null. Notice that this is similar to Maybe.
```
module MaybeNull (MaybeNull(JustVal,Null)) where
-- MaybeNull:
data MaybeNull a = JustVal a | Null deriving (Eq,Show,Read)

```

Your task is to define a typeclass Eq3 that requires that the function (===) is defined. The rules of (===) are such that anytthing compared with Unk3 gives an Unk3, otherwise for True3 and False3 you can use normal equality of two values.
```
module Eq3 (Eq3,(===)) where
```
Further, make Bool3 and MaybeNull a members of Eq3. MaybeNull a can only be a member of Eq3 if a is a member of Eq3 and when there are values to be compared, they are compared with (===). Comparing with Null always gives Unk3.
-}

module Eq3 (Eq3,(===)) where

import Bool3 (Bool3(False3,Unk3,True3))
import MaybeNull (MaybeNull(JustVal,Null))

class Eq3 a where
  (===) :: a -> a -> Bool3

instance Eq3 Bool3 where
  (===) False3 False3 = True3
  (===) True3 True3 = True3
  (===) _ _ = Unk3

instance Eq3 a => Eq3 (MaybeNull a) where
  (===) Null Null = Unk3
  (===) (JustVal x) (JustVal y) = x === y
  (===) _ _ = Unk3