module PureScript.CST
  ( RecoveredParserResult(..)
  , PartialModule(..)
  , parseModule
  , parsePartialModule
  , parseImportDecl
  , parseDecl
  , parseExpr
  , parseType
  , parseBinder
  , printModule
  , toRecovered
  , parseAndEncode
  ) where

import Prelude
import Prim hiding (Type)

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Lazy as Z
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Foreign.Class (encode)
import PureScript.CST.Lexer (lex, lexModule)
import PureScript.CST.Parser (Recovered, parseModuleBody, parseModuleHeader)
import PureScript.CST.Parser as Parser
import PureScript.CST.Parser.Monad (Parser, ParserResult(..), PositionedError, fromParserResult, initialParserState, runParser, runParser')
import PureScript.CST.Print as Print
import PureScript.CST.Range (class TokensOf, tokensOf)
import PureScript.CST.Range.TokenList as TokenList
import PureScript.CST.TokenStream (TokenStream)
import PureScript.CST.Types (Binder, Declaration, Expr, ImportDecl, Module(..), ModuleHeader, Type)
import Unsafe.Coerce (unsafeCoerce)
import Foreign as F
import Foreign.Generic(encode)

data RecoveredParserResult f
  = ParseSucceeded (f Void)
  | ParseSucceededWithErrors (Recovered f) (NonEmptyArray PositionedError)
  | ParseFailed PositionedError

toRecoveredParserResult
  :: forall f
   . Either PositionedError (Tuple (Recovered f) (Array PositionedError))
  -> RecoveredParserResult f
toRecoveredParserResult = case _ of
  Right (Tuple res errors)
    | Just nea <- NonEmptyArray.fromArray errors ->
        ParseSucceededWithErrors res nea
    | otherwise ->
        ParseSucceeded ((unsafeCoerce :: Recovered f -> f Void) res)
  Left err ->
    ParseFailed err

toRecovered :: forall f. f Void -> Recovered f
toRecovered = unsafeCoerce

runRecoveredParser :: forall a. Parser (Recovered a) -> TokenStream -> RecoveredParserResult a
runRecoveredParser p = toRecoveredParserResult <<< flip runParser p

parseModule :: String -> RecoveredParserResult Module
parseModule = runRecoveredParser Parser.parseModule <<< lexModule

parseImportDecl :: String -> RecoveredParserResult ImportDecl
parseImportDecl = runRecoveredParser Parser.parseImportDecl <<< lex

parseDecl :: String -> RecoveredParserResult Declaration
parseDecl = runRecoveredParser Parser.parseDecl <<< lex

parseExpr :: String -> RecoveredParserResult Expr
parseExpr = runRecoveredParser Parser.parseExpr <<< lex

parseType :: String -> RecoveredParserResult Type
parseType = runRecoveredParser Parser.parseType <<< lex

parseBinder :: String -> RecoveredParserResult Binder
parseBinder = runRecoveredParser Parser.parseBinder <<< lex

newtype PartialModule e = PartialModule
  { header :: ModuleHeader e
  , full :: Z.Lazy (RecoveredParserResult Module)
  }

parsePartialModule :: String -> RecoveredParserResult PartialModule
parsePartialModule src =
  toRecoveredParserResult $ case runParser' (initialParserState (lexModule src)) parseModuleHeader of
    ParseSucc header state -> do
      let
        res = PartialModule
          { header
          , full: Z.defer \_ ->
              toRecoveredParserResult $ fromParserResult $ runParser' state do
                body <- parseModuleBody
                pure $ Module { header, body }
          }
      Right $ Tuple res state.errors
    ParseFail error _ ->
      Left error

parseAndEncode :: String -> F.Foreign
parseAndEncode fileContent = case parseModule fileContent of 
          ParseSucceeded mod -> encode mod
          otherwise -> encode {}


printModule :: forall e. TokensOf e => Module e -> String
printModule mod =
  foldMap Print.printSourceToken (TokenList.toArray (tokensOf mod))
    <> foldMap (Print.printComment Print.printLineFeed) (unwrap (unwrap mod).body).trailingComments
