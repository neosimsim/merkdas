{-# LANGUAGE DeriveFunctor #-}

module Main
  ( main
  ) where

import           Control.Applicative.Free
import           Data.Bifunctor           (first)
import           Data.Either.Combinators  (maybeToRight)
import           Data.Either.Validation
import           Data.List.NonEmpty       (NonEmpty, toList)
import           Data.Maybe               (fromMaybe, mapMaybe)
import           System.Environment
import           Text.Read                (readMaybe)

-- | The parsing result for a single variable
data ParsingError
  = Missing
  | ParsingError String
  deriving (Show)

-- | A parsing error, contextualized with the
-- variable description
data EnvVarError =
  EnvVarError
    { varDesc :: EnvVarDesc
    , error   :: ParsingError
    }
  deriving (Show)

-- | Parser for a single env variable
-- All the metadata is separated from the parsing function
-- to make it monomorphic and more easily usable
data EnvVarParser a =
  EnvVarParser
    { parser :: Maybe String -> Either ParsingError a
  -- ^ the input string is a Maybe to allow optional values
    , desc   :: EnvVarDesc
    }
  deriving (Functor)

-- | Making this a separate type allows to directly return a list of `EnvVarDesc` from `runAp_`
data EnvVarDesc =
  EnvVarDesc
    { name       :: String
    , isOptional :: Bool
  -- ^ This is purely for documentation purposes,
  -- it is not used during parsing
    }
  deriving (Show)

type EnvParser = Ap EnvVarParser

-- yeah, I know, lenses.
modifyDesc :: (EnvVarDesc -> EnvVarDesc) -> EnvVarParser a -> EnvVarParser a
modifyDesc f p = p {desc = f (desc p)}

-- | Create a parser for a single environment
-- variable given its name and a parsing function
mkParser :: (String -> Either String a) -> String -> EnvVarParser a
mkParser p name =
  let parser Nothing  = Left Missing
      parser (Just v) = first ParsingError $ p v
   in EnvVarParser
        {parser = parser, desc = EnvVarDesc {name = name, isOptional = False}}

-- | Turn a single environment variable parser
-- into a composable `EnvParser`
required :: EnvVarParser a -> EnvParser a
required = liftAp

-- | Turn a single environment variable parser
-- into a composable `EnvParser`, making the
-- variable optional
optional :: EnvVarParser a -> EnvParser (Maybe a)
optional (EnvVarParser parser desc) =
  let optionalParser Nothing = Right Nothing
      optionalParser v       = Just <$> parser v
   in liftAp $
      EnvVarParser {parser = optionalParser, desc = desc {isOptional = True}}

-- | Turn a single environment variable parser
-- into a composable `EnvParser`, making the
-- variable optional
optionalWithDefault :: a -> EnvVarParser a -> EnvParser a
optionalWithDefault def = fmap (fromMaybe def) . optional

-- | Parse a string from an environment variable
str :: String -> EnvVarParser String
str = mkParser Right

-- | Parse an integer from an environment variable
int :: String -> EnvVarParser Integer
int =
  let parseInt = maybeToRight "not an integer" . readMaybe
   in mkParser parseInt

-- | Add a prefix to environment variables names
prefix :: String -> EnvParser a -> EnvParser a
prefix prefix =
  let addPrefix = modifyDesc $ \d -> d {name = prefix <> name d}
   in hoistAp addPrefix

-- | List the needed environment variables
getVars :: EnvParser a -> [String]
getVars = runAp_ (pure . name . desc)

renderDoc :: EnvParser a -> String
renderDoc p =
  let descs = runAp_ (pure . desc) p
      mkLn d =
        name d <>
        if isOptional d
          then " (optional)"
          else ""
   in unlines . fmap mkLn $ descs

-- | Given the available env variables, parse
-- a single environment variable
parseEnvVar ::
     [(String, String)] -> EnvVarParser a -> Validation (NonEmpty EnvVarError) a
parseEnvVar env (EnvVarParser p desc) =
  let v = lookup (name desc) env
      toError = pure . EnvVarError desc
   in eitherToValidation . first toError $ p v

-- | Given the available env variables, parse
-- a composite value
parseEnv ::
     [(String, String)] -> EnvParser a -> Validation (NonEmpty EnvVarError) a
parseEnv env = runAp (parseEnvVar env)

-- | Parse a composite variable from the environment
readFromEnv :: EnvParser a -> IO a
readFromEnv p = do
  env <- readNeededEnv p
  case parseEnv env p of
    Failure f -> fail (renderErrors f)
    Success a -> pure a

-- | Read only the needed variables from
-- the environment
readNeededEnv :: EnvParser a -> IO [(String, String)]
readNeededEnv p =
  let names = getVars p
      values = traverse lookupEnv names
      removeEmpty = mapMaybe sequenceA
   in removeEmpty . zip names <$> values

-- | Render the errors encountered when trying
-- to parse a composite value
renderErrors :: NonEmpty EnvVarError -> String
renderErrors errors =
  let ls = mkLine <$> toList errors
      mkLine (EnvVarError desc Missing) = name desc <> " is missing"
      mkLine (EnvVarError desc (ParsingError e)) =
        name desc <> " could not be parsed: " <> e
   in "The following errors were encountered\n" <> unlines ls

-- now, the example
data PG =
  PG
    { port     :: Integer
    , host     :: String
    , user     :: String
    , password :: String
    , db       :: String
    }
  deriving (Show)

newtype Macaroon =
  Macaroon
    { secret :: String
    }
  deriving (Show)

data Config =
  Config
    { pg       :: PG
    , macaroon :: Macaroon
    , toto     :: String
    }
  deriving (Show)

getPg :: EnvParser PG
getPg =
  prefix "PG_" $
  PG <$> optionalWithDefault 5432 (int "PORT") <*> required (str "HOST") <*>
  required (str "USER") <*>
  required (str "PASSWORD") <*>
  required (str "DB")

getMacaroon :: EnvParser Macaroon
getMacaroon = Macaroon <$> required (str "MACAROON_SECRET")

getConfig :: EnvParser Config
getConfig = Config <$> getPg <*> getMacaroon <*> required (str "TOTO")

main :: IO ()
main = do
  putStrLn $ renderDoc getConfig
  readFromEnv getConfig >>= print
