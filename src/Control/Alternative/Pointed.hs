module Control.Alternative.Pointed
    ( PointedAlternative
    , someLazy
    , manyLazy
    , ascertain
    , ascertainA
    , (<|!>)
    , (<!|>)
    , desperately
    ) where

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Identity
import qualified Data.List.NonEmpty as NE
import Control.Monad.State

import Data.Coerce

-- | An alternative functor and something without its empty.
--
-- @coerceToNonempty . @embed == id
--
-- @coerceToNonempty empty == _|_
--
-- 'someLazy' and 'manyLazy' should be the greatest lower bound of the maximally defined fixpoints of the following equations:
--
-- * @someLazy v = (:) '<$>' v '<*>' @manyLazy v@
--
-- * @manyLazy v = @someLazy v '<|>' 'pure' []@
class Alternative f => PointedAlternative f g | f -> g, g -> f where
    -- | Promise that the argument is not empty and embed the rest of f into g. This is used by manyLazy to reflect the fact that the maximum chain of applications that does not become empty does not become empty.
    coerceToNonempty :: f a -> g a

    embed :: g a -> f a

    -- | As many as possible, but not none.
    someLazy :: f a -> f (NE.NonEmpty a)
    someLazy v = coerceToNonempty <$> some_v
      where
        many_v = ascertain [] some_v
        some_v = (:) <$> v <*> embed many_v

    -- | As many as possible.
    manyLazy :: f a -> g [a]
    manyLazy v = many_v
      where
        many_v = ascertain [] some_v
        some_v = (:) <$> v <*> embed many_v

ascertain :: PointedAlternative f g => a -> f a -> g a
ascertain x = coerceToNonempty . (<|> pure x)

-- = flip (<|!>) = fromMaybeT
ascertainA :: PointedAlternative f g => g a -> f a -> g a
ascertainA x = coerceToNonempty . (<|> embed x)

(<!|>) :: PointedAlternative f g => g a -> f a -> g a
x <!|> y = coerceToNonempty $ embed x <|> y

(<|!>) :: PointedAlternative f g => f a -> g a -> g a
x <|!> y = coerceToNonempty $ x <|> embed y

-- This one actually turns [] into a stream, as opposed to NonEmpty. Hmm.
desperately :: PointedAlternative f g => f a -> g a
desperately = coerceToNonempty . asum . repeat

instance PointedAlternative Maybe Identity where
  coerceToNonempty = Identity . fromJust
  embed = Just . runIdentity

instance PointedAlternative [] NE.NonEmpty where
  coerceToNonempty = uncurry (NE.:|) . fromJust . uncons
  embed = NE.toList

-- Using IdentityT feels more mathematically correct and would allow the fundep g -> f, but introduces lots of wraps. Hmm.
instance Monad m => PointedAlternative (MaybeT m) (IdentityT m) where
  coerceToNonempty = IdentityT . liftM fromJust . runMaybeT
  embed = MaybeT . liftM Just . runIdentityT

{-
-- Requires @letlpaste 5640768261382471680
instance (Monad f, Traversable f, Monad g, Traversable g, PointedAlternative f g, Functor m, Alternative (TraversableT f m)) => PointedAlternative (TraversableT f m) (TraversableT g m) where
  coerceToNonempty = iso runTraversableT TraversableT %~ fmap coerceToNonempty
  embed            = iso runTraversableT TraversableT %~ fmap embed
-}

instance (PointedAlternative f g, MonadPlus f) => PointedAlternative (StateT s f) (StateT s g) where
  coerceToNonempty = coerce $ (.) coerceToNonempty
  embed            = coerce $ (.) embed