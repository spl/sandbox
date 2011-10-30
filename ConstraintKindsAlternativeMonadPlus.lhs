2011-10-30: Initial

Introduction
------------

I wanted to see what it would take to lift the similarities of Alternative and
MonadPlus to the constraint level using the new ConstraintKinds extension in
GHC.

\begin{code}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module ConstraintKindsAlternativeMonadPlus where

import Control.Applicative
import Control.Monad
\end{code}

Attempt 1
---------

The first experiment uses a simple superclass constraint. This appears to allow
defining the |ctx| later.

\begin{code}
class (ctx f) => Plus1 ctx f where
  zero1 :: f a
  plus1 :: f a -> f a -> f a

instance (ctx []) => Plus1 ctx [] where
  zero1 = []
  plus1 = (++)
\end{code}

I discovered that |Plus1| is treated like a multiparameter type class. That
means that |ctx| cannot be inferred in many cases. For example, the following
definition of |msum1| cannot be given.

\begin{spec}
msum1 = foldr plus1 zero1
\end{spec}

In GHCi, we see the following type, and we have no way to instantiate |ctx|:

\begin{verbatim}
*ConstraintKindsAlternativeMonad> :t foldr plus1 zero1
foldr plus1 zero1 :: (Plus1 ctx1 f, Plus1 ctx f) => [f a] -> f a
\end{verbatim}

Attempt 2
---------

My second experiment uses an associated constraint synonym to ensure that the
constraint |PlusCtx2 f| is unique for a given |f|.

\begin{code}
class Plus2 f where
  type PlusCtx2 f :: Constraint
  zero2 :: (PlusCtx2 f) => f a
  plus2 :: (PlusCtx2 f) => f a -> f a -> f a

instance Plus2 [] where
  type PlusCtx2 [] = Applicative []
  zero2 = []
  plus2 = (++)
\end{code}

Now, we can define |msum2| (but watch out for the scary error that results from
the monomorphism restriction but doesn't mention it):

\begin{code}
msum2 :: (PlusCtx2 f, Plus2 f) => [f a] -> f a
msum2 = foldr plus2 zero2
\end{code}

Unfortunately, with |Plus2| we do not have the option of reusing the list
instance for |MonadPlus|. Thus, this approach is not very useful.

Attempt 3
---------

The third (and final) experiment returns to the superclass approach of |Plus1|
but adds a proxy with a phantom type to allow us uniquely determine the |ctx|
constraint.

\begin{code}
data Proxy (ctx :: (* -> *) -> Constraint) = Proxy

class (ctx f) => Plus3 ctx f where
  zero3 :: Proxy ctx -> f a
  plus3 :: Proxy ctx -> f a -> f a -> f a

instance (ctx []) => Plus3 ctx [] where
  zero3 _ = []
  plus3 _ = (++)
\end{code}

With |Plus3| we do not have to instantiate the |ctx| in the instance. We also do
not have to instantiate when defining functions such as |msum3|:

\begin{code}
msum3 :: (Plus3 ctx f) => Proxy ctx -> [f a] -> f a
msum3 p = foldr (plus3 p) (zero3 p)
\end{code}

However, we do have to use the same |Proxy| type in these functions. Note what
happens if we do not:

\begin{verbatim}
*ConstraintKindsAlternativeMonadPlus> :t foldr (plus3 Proxy) (zero3 Proxy)
foldr (plus3 Proxy) (zero3 Proxy) :: (Plus3 ctx1 f, Plus3 ctx f) => [f a] -> f a
\end{verbatim}

We can define proxy values to instantiate the constraint:

\begin{code}
alt :: Proxy Alternative
alt = Proxy

mon :: Proxy Monad
mon = Proxy
\end{code}

Then, we can use them in our application:

\begin{code}
abc = [[], "a", [], "bc"]

test_concat_alt_abc = msum3 alt abc == "abc"
test_concat_mon_abc = msum3 mon abc == "abc"
\end{code}

Reflection
----------

It's unfortunate that both approaches 1 and 3 require UndecidableInstances. I
would rather be able to say that the constraint will always be reducible. Can
this be done by defining the collection of constraints that are allowed?

The error messages for constraint kinds seem worse than the normal type and
class errors. It's still new, so that's no real surprise. If the error is due to
the monomorphism restriction, I would expect to see the error message reflect
that.

