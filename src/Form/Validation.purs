module Soltan.Form.Validation where

import Prelude

import Soltan.Data.Username (Username)
import Soltan.Data.Username as Username
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Data.String as String

data FormError
  = Required
  | TooShort
  | TooLong
  | InvalidUsername

errorToString :: FormError -> String
errorToString = case _ of
  Required -> "This field is required."
  TooShort -> "Not enough characters entered"
  TooLong -> "Too many characters entered"
  InvalidUsername -> "Invalid username"

required :: forall a. Eq a => Monoid a => a -> Either FormError a
required = check (_ /= mempty) Required

-- | Ensure that an input string is longer than the provided lower limit.
minLength :: Int -> String -> Either FormError String
minLength n = check (\str -> String.length str > n) TooShort

-- | Ensure that an input string is shorter than the provided upper limit.
maxLength :: Int -> String -> Either FormError String
maxLength n = check (\str -> String.length str <= n) TooLong


-- | Ensure that an input string is a valid username. Usernames in Conduit use
-- | the smart constructor pattern, so we can't construct a username directly --
-- | we'll need to defer to the `parse` helper function exported by
-- | `Conduit.Data.Username`. Since that function returns a `Maybe` value, we'll
-- | use the `note` helper from `Data.Either` to turn the `Nothing` case into
-- | an error.
usernameFormat :: String -> Either FormError Username
usernameFormat = note InvalidUsername <<< Username.parse


-- | A small helper function for writing validation functions that rely on a
-- | true/false predicate.
check :: forall a. (a -> Boolean) -> FormError -> a -> Either FormError a
check f err a
  | f a = Right a
  | otherwise = Left err

-- | Sometimes we'd like to validate an input only if it isn't empty. This is useful for optional
-- | fields: if you've provided a value, we'll validate it, but if you haven't, then you should
-- | still be able to submit the form without error. For instance, we might allow a user to
-- | optionally provide an email address, but if they do, it must be valid.
-- |
-- | This helper function lets us transform a set of validation rules so that they only apply when
-- | the input is not empty. It isn't used in this module, but is used in the various forms.
toOptional
  :: forall a b
   . Monoid a
  => Eq a
  => (a -> Either FormError b)
  -> (a -> Either FormError (Maybe b))
toOptional k = \value ->
  if value == mempty then
    Right Nothing
  else
    map Just $ k value
