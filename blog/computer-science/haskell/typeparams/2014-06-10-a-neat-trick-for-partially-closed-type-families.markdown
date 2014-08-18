---
title: A neat trick for partially closed type families
---

This post covers a pretty neat trick with [closed type families](http://www.haskell.org/haskellwiki/GHC/Type_families#Closed_family_simplification). Normal type families are "open" because any file can add new instances of the type. Closed type families, however, must be defined in a single file. This lets the type checker make more assumptions, and so the closed families are more powerful. In this post, we will circumvent this restriction and define certain closed type families over many files.

<!-- more -->

We only need these two language extensions for the technique:

    
    > {-# LANGUAGE TypeFamilies #-}
    > {-# LANGUAGE UndecidableInstances #-}


But for our motivating example, we'll also use these extensions and some basic imports:

    
    > {-# LANGUAGE KindSignatures #-}
    > {-# LANGUAGE MultiParamTypeClasses #-}
    > {-# LANGUAGE ConstraintKinds #-}
    
    > import Data.Proxy
    > import GHC.Exts


Let's begin.

Consider the classes:

    
    > class Param_a (p :: * -> Constraint) t
    > class Param_b (p :: * -> Constraint) t
    > class Param_c (p :: * -> Constraint) t
    > class Base t


These classes can be chained together like so:

    
    > type Telescope_abc = Param_a (Param_b (Param_c Base))


It is easy to write a type family that returns the "head" of this list. On a telescope, the lens closest to you is called the eye piece, so that's what we'll call our type family:

    
    > type family EyePiece ( p :: * -> Constraint ) :: * -> Constraint
    
    > type instance EyePiece (Param_a p) = Param_a Base
    > type instance EyePiece (Param_b p) = Param_b Base
    > type instance EyePiece (Param_c p) = Param_c Base


Again, this type family is "open" because new instances can be defined in any file.

We might use this EyePiece type family as:

    
    ghci> :t Proxy :: Proxy (EyePiece Telescope_abc)
      :: Proxy (Param_a Base)


Now, let's try to write a type class that does the opposite. Instead of extracting the first element in the chain, it will extract the last. On a telescope the lens farthest away from you is called the objective, so that's what we'll call our type family. We'll also need to define it as a closed type family:

    
    type family Objective (lens :: * -> Constraint) :: * -> Constraint where
      Objective (Param_a p) = Objective p
      Objective (Param_b p) = Objective p
      Objective (Param_c p) = Objective p
      Objective (Param_a Base) = Param_a Base
      Objective (Param_b Base) = Param_b Base
      Objective (Param_c Base) = Param_c Base


We can use the Objective family like:

    
    ghci> :t Proxy :: Proxy (Objective Telescope_abc)
      :: Proxy (Param_c Base)


The Objective family must be closed. This is because the only way to identify when we are at the end of the telescope is by checking if the p parmaeter is the Base class. If it is, then we're done. If not, we must keep moving down the telescope recusively. Without a closed type family, we would have to explicitly list all of the recursive paths. This means $O(n^2)$ type instances whenever we want to add a new Param_xxx class. That's nasty and error prone.

Again, the downside of closed type families is that they must be defined all in one place. We can work around this limitation by "factoring" the closed type family into a collection of closed and open type families. In the example above, this works out to be:

``` 
> type family Objective (lens :: * -> Constraint) :: * -> Constraint
> type instance Objective (Param_a p) = Objective_Param_a (Param_a p)
> type instance Objective (Param_b p) = Objective_Param_b (Param_b p)
> type instance Objective (Param_c p) = Objective_Param_c (Param_c p)
> type instance Objective Base = Base

> type family Objective_Param_a (lens :: * -> Constraint) :: * -> Constraint where
>   Objective_Param_a (Param_a Base) = Param_a Base
>   Objective_Param_a (Param_a p) = Objective p

> type family Objective_Param_b (lens :: * -> Constraint) :: * -> Constraint where
>   Objective_Param_b (Param_b Base) = Param_b Base
>   Objective_Param_b (Param_b p) = Objective p

> type family Objective_Param_c (lens :: * -> Constraint) :: * -> Constraint where
>   Objective_Param_c (Param_c Base) = Param_c Base
>   Objective_Param_c (Param_c p) = Objective p
```


``` 
ghci> :t Proxy :: Proxy (Objective Telescope_abc)
  :: Proxy (Param_c Base)
```

With this factoring, we are able to define the Objective instance for each Param_xxx in separate files and retain the benefits of closed type families.

Here is another example. The RemoveObjective family acts like the init function from the Prelude:

```    
> type family RemoveObjective (lens :: * -> Constraint) :: * -> Constraint
> type instance RemoveObjective (Param_a p) = RemoveObjective_Param_a (Param_a p)
> type instance RemoveObjective (Param_b p) = RemoveObjective_Param_b (Param_b p)
> type instance RemoveObjective (Param_c p) = RemoveObjective_Param_c (Param_c p)

> type family RemoveObjective_Param_a (lens :: * -> Constraint) :: * -> Constraint where
>   RemoveObjective_Param_a (Param_a Base) = Base
>   RemoveObjective_Param_a (Param_a p) = Param_a (RemoveObjective p)

> type family RemoveObjective_Param_b (lens :: * -> Constraint) :: * -> Constraint where
>   RemoveObjective_Param_b (Param_b Base) = Base
>   RemoveObjective_Param_b (Param_b p) = Param_b (RemoveObjective p)

> type family RemoveObjective_Param_c (lens :: * -> Constraint) :: * -> Constraint where
>   RemoveObjective_Param_c (Param_c Base) = Base
>   RemoveObjective_Param_c (Param_c p) = Param_b (RemoveObjective p)
```

```
ghci> :t Proxy :: Proxy (RemoveObjective Telescope_abc)
  :: Proxy (Param_a (Param_b Base))
```

Of course, you can't do this trick with every closed type family. For example, the RemoveObjective_Param_c family above cannot be factored any smaller. But if you find yourself wanting the benefits of both closed and open type families, then your type probably has the needed structure.
