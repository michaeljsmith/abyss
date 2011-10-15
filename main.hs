{-#LANGUAGE GADTs, EmptyDataDecls, TypeSynonymInstances, TypeOperators, StandaloneDeriving, FlexibleContexts #-}

import Data.Typeable
import Text.Show.Functions

-- Lambda terms using Haskell functions to represent functionals
--
-- * We don't care about exotic terms here, and hence don't pull the recursive
--   term structure out.
-- * The `Typeable' contexts and the `Tag' variant are in preparation for
--   being able to convert to a de Bruijn representation.
--
data Term t where
  Tag :: Typeable t      -- for conversion to de Bruijn
      => Int -> Term t    
         -- environment size at defining occurrence

  Con :: (Typeable t, Show t)
      => t                       -> Term t
  Lam :: (Typeable s, Typeable t,
          Show s, Show t)
      => (Term s -> Term t)      -> Term (s -> t)
  App :: (Typeable s, Typeable t,
          Show s, Show t)
      => Term (s -> t) -> Term s -> Term t

deriving instance Show (Term t)

-- A term interpreter for closed terms
--
intp :: Show t => Term t -> t
intp (Tag ix)      = error "HOAS.intp: Tag is only for conversion"
intp (Con v)       = v
intp (Lam fun)     = intp . fun . Con
intp (App fun arg) = (intp fun) (intp arg)

foo :: (Show a, Typeable a) => Term (a -> [Char])
foo = Lam f where
  f x = Con (show x ++ show x)

bar :: Term [Char]
bar = App foo foo

main = print "hello"
