---
title: Tying the type knot gives us theorems for cheap
---

In today's adventure, our hero ghc faces its final challenge: granting parametricity to our lensified [Functor](http://izbicki.me/blog/i-got-lenses-in-my-functors), [Applicative](http://izbicki.me/blog/lens-you-an-applicative-for-great-haskell), and [Monad](http://izbicki.me/blog/do-your-lenses-even-do-notation) classes. Parametricity is the key to [finishing the task of simplifying our type signatures](http://izbicki.me/blog/the-type-lens-laws) that we started two days ago. At the end we'll still have some loose ends left untied, but they'll have to wait for another haskell binge.

<!-- more -->


### summon cthulhu



    
    > {-# LANGUAGE KindSignatures #-}
    > {-# LANGUAGE TypeFamilies #-}
    > {-# LANGUAGE MultiParamTypeClasses #-}
    > {-# LANGUAGE UndecidableInstances #-}
    > {-# LANGUAGE FlexibleInstances #-}
    > {-# LANGUAGE LiberalTypeSynonyms #-}
    > {-# LANGUAGE ImpredicativeTypes #-}
    > {-# LANGUAGE ConstraintKinds #-}
    
    > import Control.Category
    > import Prelude hiding ( (.), id, Functor(..), Applicative(..), Monad(..) )
    
    > import Data.Params
    > import Data.Params.Functor
    > import Data.Params.Applicative 
    > import Data.Params.Monad




### tying the value knot


At the value level, [tying the knot](http://www.haskell.org/haskellwiki/Tying_the_Knot) is a classic technique in Haskell data structures. It let's us build circular datastructures using self-refrence and lazy evaluation. The classic example is the cycle:

    
    > cycle = x where
    >   x = 0 : y
    >   y = 1 : x


But there can be no direct analogy between tying the knot at the value and type level. This is because tying the knot requires lazy evaluation, which doesn't make sense for types.

_(Idea! Maybe we should just start calling Python a lazily typed language!)_

But let's check out a new type level technique... and if you look at the right angle... and squint just the right amount... then it sorta kinda looks like tying the knot.


### tying the type knot


Remember all that fun we had with lensified [Functors](http://izbicki.me/blog/i-got-lenses-in-my-functors), [Applicatives](http://izbicki.me/blog/lens-you-an-applicative-for-great-haskell), and [Monads](http://izbicki.me/blog/do-your-lenses-even-do-notation)? Our new classes are powerful, but we paid a major price for that power: We gave up [parametricity](https://www.fpcomplete.com/school/starting-with-haskell/introduction-to-haskell/5-type-classes). Parametricity is one of the main properties that makes Haskell code so easy to use and fun to write. Giving it up would be a disaster.

So let's get it back.

First, we'll modify our type classes. We'll need to pass a third parameter to each of them. We'll call it b. This parameter represents the type at the lens position of our Functor/Applicative/Monad.

    
    > class b ~ GetParam lens tb => Functor' lens tb b where
    >   fmap' :: TypeLens p lens -> (a -> b) -> SetParam lens a tb -> tb
    
    > class Functor' lens tb b => Applicative' lens tb b where
    >   pure :: GetParam lens tb -> TypeLens Base lens -> tb
    >
    >   ap :: ( tf ~ SetParam lens (a -> b) tb
    >         , ta ~ SetParam lens a tb
    >         , a ~ GetParam lens ta
    >         )
    >      => TypeLens Base lens -> tf -> ta -> tb
    
    > class Applicative' lens tfb b => Monad' lens tfb b where
    >   join :: tffb ~ CoJoin lens tfb
    >        => TypeLens Base lens -> tffb -> tfb


Now we can guarantee parametricity when we declare instances of the classes. All we have to do is make sure that the b parameter is a variable and not a type constructor. For example, this is parametric:

    
    instance Functor' (Param_a Base) (Either a b) a


but this is not:

    
    instance Functor' (Param_a Base) (Either Int b) Int


Making our instances parametric is not enough for the type checker. We must prove that all instances will always be parametric. These higher-rank type constraints assert this fact:

    
    > type Functor''     p t = forall a t'. ( t' ~ SetParam p a t, Functor'     p t' a )
    > type Applicative'' p t = forall a t'. ( t' ~ SetParam p a t, Applicative' p t' a )
    > type Monad''       p t = forall a t'. ( t' ~ SetParam p a t, Monad'       p t' a )


These type synonym constraints are what I'm calling "tying the type knot." The foralled a and t' let us represent an "infinite" number of constraints with a finite size, just like tying the knot lets us represent an infinite data structure with finite memory.

This same technique also works for the [constrained](http://www.reddit.com/r/haskell/comments/24ansh/constrained_monad_overview_for_the_naive/) [monad](http://www.ittc.ku.edu/csdl/fpg/files/Sculthorpe-13-ConstrainedMonad.pdf) [problem](https://github.com/mikeizbicki/ConstraintKinds):

    
    > type CnstFunctor     p c t = forall a t'.  ( t' ~ SetParam p a t, Functor'     p t' a, c a )
    > type CnstApplicative p c t = forall a t'.  ( t' ~ SetParam p a t, Applicative' p t' a, c a )
    > type CnstMonad       p c t = forall a t'.  ( t' ~ SetParam p a t, Monad'       p t' a, c a )




### simpler type signatures


So what, exactly, do these new parametric constraints buy us?

Remember how we used the idea of type-level rewrite rules to simplify the type of our applicative sequencing operator (*>)? Now we can simplify it even further.

This is the type that we came up with [two posts ago](http://izbicki.me/blog/the-type-lens-laws):

    
    (*>) :: 
      ( Applicative lens ( SetParam lens ( a -> b -> b ) tb ) 
      , Applicative lens ( SetParam lens (      b -> b ) tb )
      , Applicative lens tb 
    
      , tb ~ SetParam lens b tb
    
      ) => SetParam lens a tb 
        -> tb -> TypeLens Base lens -> tb


It's pretty clean except for the multiple Applicative constraints. But if we use the type-knotted constraints, we can combine all the Applicatives into one:

    
    (*>) :: 
      ( Applicative'' lens tb
      , tb ~ SetParam lens b tb
      ) => SetParam lens a tb 
        -> tb -> TypeLens Base lens -> tb


Unfortunately, we can't test these parametric constraints today because of a [ghc bug/missing feature](https://ghc.haskell.org/trac/ghc/ticket/9195).


### giving ghc diabetes


With a heavy dose of sugar, we can use our type lenses to create a type level record syntax. This will make our (*>) operator's type even clearer... almost like original.

The sugaring rules are pretty simple. Just replace any type expression of the form:

    
    t { lens = a }


with a call to the SetParam type function:

    
    SetParam lens a t


And that's it!

Now, the lensified and standard versions of (*>) are pretty similar looking. Here they are in a side-by-side comparison:

    
    original   (*>) :: Applicative      t => t a            -> t b            -> t b
    
    lensified  (*>) :: Applicative lens t => t { lens = a } -> t { lens = b } -> t { lens = b }


Sweet!


### tying another *kind* of knot


Next time, we'll see how to promote everything we've done to the kind level.

...

...

...

...

Just kidding!

I'm actually [getting married this weekend](http://mikeandkristen.izbicki.me/)! I wanted to share my excitement with all you haskellers, so I put together this bad little tying the knot pun! Thanks for putting up with me! Yay!

_(disclaimer: there's probably lot's of little mistakes floating around in these posts... type theory isn't really my area... and I just thought of this idea last week... and... now my brain hurts...)_
