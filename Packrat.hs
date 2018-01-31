{-# LANGUAGE GADTs, RankNTypes #-}
module Packrat where

import Control.Applicative
import Control.Monad
import Control.Monad.Fail as Fail
import Control.Monad.Fix
import Data.Semigroup
import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.Unsafe as Unsafe
import Data.Vector(Vector)
import qualified Data.Vector as Vector

type Position = Int

newtype Parser a = Parser
  { unparser
    :: forall r
    .  (a -> Position -> r)
    -> (Position -> r)
    -> Position
    -> Text
    -> r
  }

instance Semigroup a => Semigroup (Parser a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Parser a) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s e -> p (s . f) e

instance Applicative Parser where
  pure a = Parser $ \s _e pos _inp -> s a pos
  (<*>) = ap

instance Alternative Parser where
  empty = Parser $ \_s e pos _inp -> e pos
  Parser p <|> Parser q = Parser
    $ \s e pos inp -> p
      s
      (\_pos' -> q s e pos inp)
      pos
      inp

instance Monad Parser where
  return = pure
  Parser p >>= f = Parser
    $ \s e pos inp -> p
      (\a pos' -> unparser (f a) s e pos' inp)
      e
      pos
      inp
  fail = Fail.fail

instance MonadFail Parser where
  fail _x = Parser $ \_s e pos _inp -> e pos

instance MonadPlus Parser where
  mzero = empty
  mplus = (<|>)

data Grammar a where
  RuleBind :: Parser a -> (Parser a -> Grammar b) -> Grammar b
  FixBind :: (a -> Grammar a) -> (a -> Grammar b) -> Grammar b
  Return :: a -> Grammar a

instance Functor Grammar where
  fmap f (RuleBind ps h) = RuleBind ps (fmap f . h)
  fmap f (FixBind g h) = FixBind g (fmap f . h)
  fmap f (Return x) = Return $ f x

instance Applicative Grammar where
  pure  = return
  (<*>) = ap

instance Monad Grammar where
  return = Return
  RuleBind ps f >>= k = RuleBind ps (f >=> k)
  FixBind f g >>= k = FixBind f (g >=> k)
  Return x >>= k = k x

instance MonadFix Grammar where
  mfix f = FixBind f return

-- | Create a new non-terminal by giving its production.
rule :: Parser a -> Grammar (Parser a)
rule p = RuleBind p return

-- | Run a grammar, given an action to perform on productions to be turned into
-- non-terminals.
runGrammar
  :: (forall a. Parser a -> Parser a)
  -> Grammar b
  -> b
runGrammar r grammar = case grammar of
  RuleBind p k -> runGrammar r $ k (r p)
  Return a -> a
  FixBind f k -> do
    let a = fix $ runGrammar r <$> f
    runGrammar r $ k a

data Result a
  = Success a
  | Failure Position

-- | Memoise the given grammar's @rule@s using the packrat algorithm and turn
-- it into a parser. Note that the memo tables are recreated every time this
-- production is encountered, so it might make sense to do this on the
-- outermost level of the parser.
memoising :: Grammar (Parser a) -> Parser a
memoising grammar = Parser $ \s e pos inp ->
  let
    len = Unsafe.lengthWord16 inp - pos

    memoise :: Parser a -> Parser a
    memoise parser
      = Parser
      $ \s' e' pos' _inp -> unmemoEntry (results Vector.! (pos' - pos)) s' e'
      where
        results
          = Vector.generate len
          $ \i -> unparser parser
            (\a pos' -> MemoEntry $ \s _e -> s a pos')
            (\pos' -> MemoEntry $ \_s e -> e pos')
            i
            inp
  in
    unparser
      (runGrammar memoise grammar)
      s
      e
      pos
      inp

newtype MemoEntry a = MemoEntry { unmemoEntry :: forall r. (a -> Position -> r) -> (Position -> r) -> r }
