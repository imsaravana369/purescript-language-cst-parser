module PureScript.CST.Types where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
import Prim hiding (Row, Type)
import Foreign as F
import Foreign.Generic.Class(class Encode, encode)

newtype ModuleName = ModuleName String

derive newtype instance eqModuleName :: Eq ModuleName
derive newtype instance ordModuleName :: Ord ModuleName
derive instance newtypeModuleName :: Newtype ModuleName _

type SourcePos =
  { line :: Int
  , column :: Int
  }

type SourceRange =
  { start :: SourcePos
  , end :: SourcePos
  }

data Comment l
  = Comment String
  | Space Int
  | Line l Int

data LineFeed
  = LF
  | CRLF

data SourceStyle
  = ASCII
  | Unicode

derive instance eqSourceStyle :: Eq SourceStyle

data IntValue
  = SmallInt Int
  | BigInt String
  | BigHex String

derive instance eqIntValue :: Eq IntValue

data Token
  = TokLeftParen
  | TokRightParen
  | TokLeftBrace
  | TokRightBrace
  | TokLeftSquare
  | TokRightSquare
  | TokLeftArrow SourceStyle
  | TokRightArrow SourceStyle
  | TokRightFatArrow SourceStyle
  | TokDoubleColon SourceStyle
  | TokForall SourceStyle
  | TokEquals
  | TokPipe
  | TokTick
  | TokDot
  | TokComma
  | TokUnderscore
  | TokBackslash
  | TokAt
  | TokLowerName (Maybe ModuleName) String
  | TokUpperName (Maybe ModuleName) String
  | TokOperator (Maybe ModuleName) String
  | TokSymbolName (Maybe ModuleName) String
  | TokSymbolArrow SourceStyle
  | TokHole String
  | TokChar String Char
  | TokString String String
  | TokRawString String
  | TokInt String IntValue
  | TokNumber String Number
  | TokLayoutStart Int
  | TokLayoutSep Int
  | TokLayoutEnd Int

derive instance eqToken :: Eq Token

type SourceToken =
  { range :: SourceRange
  , leadingComments :: Array (Comment LineFeed)
  , trailingComments :: Array (Comment Void)
  , value :: Token
  }

newtype Ident = Ident String

derive newtype instance eqIdent :: Eq Ident
derive newtype instance ordIdent :: Ord Ident
derive instance newtypeIdent :: Newtype Ident _

newtype Proper = Proper String

derive newtype instance eqProper :: Eq Proper
derive newtype instance ordProper :: Ord Proper
derive instance newtypeProper :: Newtype Proper _

newtype Label = Label String

derive newtype instance eqLabel :: Eq Label
derive newtype instance ordLabel :: Ord Label
derive instance newtypeLabel :: Newtype Label _

newtype Operator = Operator String

derive newtype instance eqOperator :: Eq Operator
derive newtype instance ordOperator :: Ord Operator
derive instance newtypeOperator :: Newtype Operator _

newtype Name a = Name
  { token :: SourceToken
  , name :: a
  }

derive instance newtypeName :: Newtype (Name a) _

newtype QualifiedName a = QualifiedName
  { token :: SourceToken
  , module :: Maybe ModuleName
  , name :: a
  }

derive instance newtypeQualifiedName :: Newtype (QualifiedName a) _

newtype Wrapped a = Wrapped
  { open :: SourceToken
  , value :: a
  , close :: SourceToken
  }

derive instance newtypeWrapped :: Newtype (Wrapped a) _

newtype Separated a = Separated
  { head :: a
  , tail :: Array (Tuple SourceToken a)
  }

derive instance newtypeSeparated :: Newtype (Separated a) _

newtype Labeled a b = Labeled
  { label :: a
  , separator :: SourceToken
  , value :: b
  }

derive instance newtypeLabeled :: Newtype (Labeled a b) _

newtype Prefixed a = Prefixed
  { prefix :: Maybe SourceToken
  , value :: a
  }

derive instance newtypePrefixed :: Newtype (Prefixed a) _

type Delimited a = Wrapped (Maybe (Separated a))
type DelimitedNonEmpty a = Wrapped (Separated a)

data OneOrDelimited a
  = One a
  | Many (DelimitedNonEmpty a)

data Type e
  = TypeVar (Name Ident)
  | TypeConstructor (QualifiedName Proper)
  | TypeWildcard SourceToken
  | TypeHole (Name Ident)
  | TypeString SourceToken String
  | TypeInt (Maybe SourceToken) SourceToken IntValue
  | TypeRow (Wrapped (Row e))
  | TypeRecord (Wrapped (Row e))
  | TypeForall SourceToken (NonEmptyArray (TypeVarBinding (Prefixed (Name Ident)) e)) SourceToken (Type e)
  | TypeKinded (Type e) SourceToken (Type e)
  | TypeApp (Type e) (NonEmptyArray (Type e))
  | TypeOp (Type e) (NonEmptyArray (Tuple (QualifiedName Operator) (Type e)))
  | TypeOpName (QualifiedName Operator)
  | TypeArrow (Type e) SourceToken (Type e)
  | TypeArrowName SourceToken
  | TypeConstrained (Type e) SourceToken (Type e)
  | TypeParens (Wrapped (Type e))
  | TypeError e

data TypeVarBinding a e
  = TypeVarKinded (Wrapped (Labeled a (Type e)))
  | TypeVarName a

newtype Row e = Row
  { labels :: Maybe (Separated (Labeled (Name Label) (Type e)))
  , tail :: Maybe (Tuple SourceToken (Type e))
  }

derive instance newtypeRow :: Newtype (Row e) _

newtype Module e = Module
  { header :: ModuleHeader e
  , body :: ModuleBody e
  }

derive instance newtypeModule :: Newtype (Module e) _

newtype ModuleHeader e = ModuleHeader
  { keyword :: SourceToken
  , name :: Name ModuleName
  , exports :: Maybe (DelimitedNonEmpty (Export e))
  , where :: SourceToken
  , imports :: Array (ImportDecl e)
  }

derive instance newtypeModuleHeader :: Newtype (ModuleHeader e) _

newtype ModuleBody e = ModuleBody
  { decls :: Array (Declaration e)
  , trailingComments :: Array (Comment LineFeed)
  , end :: SourcePos
  }

derive instance newtypeModuleBody :: Newtype (ModuleBody e) _

data Export e
  = ExportValue (Name Ident)
  | ExportOp (Name Operator)
  | ExportType (Name Proper) (Maybe DataMembers)
  | ExportTypeOp SourceToken (Name Operator)
  | ExportClass SourceToken (Name Proper)
  | ExportModule SourceToken (Name ModuleName)
  | ExportError e

data DataMembers
  = DataAll SourceToken
  | DataEnumerated (Delimited (Name Proper))

data Declaration e
  = DeclData (DataHead e) (Maybe (Tuple SourceToken (Separated (DataCtor e))))
  | DeclType (DataHead e) SourceToken (Type e)
  | DeclNewtype (DataHead e) SourceToken (Name Proper) (Type e)
  | DeclClass (ClassHead e) (Maybe (Tuple SourceToken (NonEmptyArray (Labeled (Name Ident) (Type e)))))
  | DeclInstanceChain (Separated (Instance e))
  | DeclDerive SourceToken (Maybe SourceToken) (InstanceHead e)
  | DeclKindSignature SourceToken (Labeled (Name Proper) (Type e))
  | DeclSignature (Labeled (Name Ident) (Type e))
  | DeclValue (ValueBindingFields e)
  | DeclFixity FixityFields
  | DeclForeign SourceToken SourceToken (Foreign e)
  | DeclRole SourceToken SourceToken (Name Proper) (NonEmptyArray (Tuple SourceToken Role))
  | DeclError e

newtype Instance e = Instance
  { head :: InstanceHead e
  , body :: Maybe (Tuple SourceToken (NonEmptyArray (InstanceBinding e)))
  }

derive instance newtypeInstance :: Newtype (Instance e) _

data InstanceBinding e
  = InstanceBindingSignature (Labeled (Name Ident) (Type e))
  | InstanceBindingName (ValueBindingFields e)

newtype ImportDecl e = ImportDecl
  { keyword :: SourceToken
  , module :: Name ModuleName
  , names :: Maybe (Tuple (Maybe SourceToken) (DelimitedNonEmpty (Import e)))
  , qualified :: Maybe (Tuple SourceToken (Name ModuleName))
  }

derive instance newtypeImportDecl :: Newtype (ImportDecl e) _

data Import e
  = ImportValue (Name Ident)
  | ImportOp (Name Operator)
  | ImportType (Name Proper) (Maybe DataMembers)
  | ImportTypeOp SourceToken (Name Operator)
  | ImportClass SourceToken (Name Proper)
  | ImportError e

type DataHead e =
  { keyword :: SourceToken
  , name :: Name Proper
  , vars :: Array (TypeVarBinding (Name Ident) e)
  }

newtype DataCtor e = DataCtor
  { name :: Name Proper
  , fields :: Array (Type e)
  }

derive instance newtypeDataCtor :: Newtype (DataCtor e) _

type ClassHead e =
  { keyword :: SourceToken
  , super :: Maybe (Tuple (OneOrDelimited (Type e)) SourceToken)
  , name :: Name Proper
  , vars :: Array (TypeVarBinding (Name Ident) e)
  , fundeps :: Maybe (Tuple SourceToken (Separated ClassFundep))
  }

data ClassFundep
  = FundepDetermined SourceToken (NonEmptyArray (Name Ident))
  | FundepDetermines (NonEmptyArray (Name Ident)) SourceToken (NonEmptyArray (Name Ident))

type InstanceHead e =
  { keyword :: SourceToken
  , name :: Maybe (Tuple (Name Ident) SourceToken)
  , constraints :: Maybe (Tuple (OneOrDelimited (Type e)) SourceToken)
  , className :: QualifiedName Proper
  , types :: Array (Type e)
  }

data Fixity
  = Infix
  | Infixl
  | Infixr

data FixityOp
  = FixityValue (QualifiedName (Either Ident Proper)) SourceToken (Name Operator)
  | FixityType SourceToken (QualifiedName Proper) SourceToken (Name Operator)

type FixityFields =
  { keyword :: Tuple SourceToken Fixity
  , prec :: Tuple SourceToken Int
  , operator :: FixityOp
  }

type ValueBindingFields e =
  { name :: Name Ident
  , binders :: Array (Binder e)
  , guarded :: Guarded e
  }

data Guarded e
  = Unconditional SourceToken (Where e)
  | Guarded (NonEmptyArray (GuardedExpr e))

newtype GuardedExpr e = GuardedExpr
  { bar :: SourceToken
  , patterns :: Separated (PatternGuard e)
  , separator :: SourceToken
  , where :: Where e
  }

derive instance newtypeGuardedExpr :: Newtype (GuardedExpr e) _

newtype PatternGuard e = PatternGuard
  { binder :: Maybe (Tuple (Binder e) SourceToken)
  , expr :: Expr e
  }

derive instance newtypePatternGuard :: Newtype (PatternGuard e) _

data Foreign e
  = ForeignValue (Labeled (Name Ident) (Type e))
  | ForeignData SourceToken (Labeled (Name Proper) (Type e))
  | ForeignKind SourceToken (Name Proper)

data Role
  = Nominal
  | Representational
  | Phantom

data Expr e
  = ExprHole (Name Ident)
  | ExprSection SourceToken
  | ExprIdent (QualifiedName Ident)
  | ExprConstructor (QualifiedName Proper)
  | ExprBoolean SourceToken Boolean
  | ExprChar SourceToken Char
  | ExprString SourceToken String
  | ExprInt SourceToken IntValue
  | ExprNumber SourceToken Number
  | ExprArray (Delimited (Expr e))
  | ExprRecord (Delimited (RecordLabeled (Expr e)))
  | ExprParens (Wrapped (Expr e))
  | ExprTyped (Expr e) SourceToken (Type e)
  | ExprInfix (Expr e) (NonEmptyArray (Tuple (Wrapped (Expr e)) (Expr e)))
  | ExprOp (Expr e) (NonEmptyArray (Tuple (QualifiedName Operator) (Expr e)))
  | ExprOpName (QualifiedName Operator)
  | ExprNegate SourceToken (Expr e)
  | ExprRecordAccessor (RecordAccessor e)
  | ExprRecordUpdate (Expr e) (DelimitedNonEmpty (RecordUpdate e))
  | ExprApp (Expr e) (NonEmptyArray (AppSpine Expr e))
  | ExprLambda (Lambda e)
  | ExprIf (IfThenElse e)
  | ExprCase (CaseOf e)
  | ExprLet (LetIn e)
  | ExprDo (DoBlock e)
  | ExprAdo (AdoBlock e)
  | ExprError e

data AppSpine f e
  = AppType SourceToken (Type e)
  | AppTerm (f e)

data RecordLabeled a
  = RecordPun (Name Ident)
  | RecordField (Name Label) SourceToken a

data RecordUpdate e
  = RecordUpdateLeaf (Name Label) SourceToken (Expr e)
  | RecordUpdateBranch (Name Label) (DelimitedNonEmpty (RecordUpdate e))

type RecordAccessor e =
  { expr :: Expr e
  , dot :: SourceToken
  , path :: Separated (Name Label)
  }

type Lambda e =
  { symbol :: SourceToken
  , binders :: NonEmptyArray (Binder e)
  , arrow :: SourceToken
  , body :: Expr e
  }

type IfThenElse e =
  { keyword :: SourceToken
  , cond :: Expr e
  , then :: SourceToken
  , true :: Expr e
  , else :: SourceToken
  , false :: Expr e
  }

type CaseOf e =
  { keyword :: SourceToken
  , head :: Separated (Expr e)
  , of :: SourceToken
  , branches :: NonEmptyArray (Tuple (Separated (Binder e)) (Guarded e))
  }

type LetIn e =
  { keyword :: SourceToken
  , bindings :: NonEmptyArray (LetBinding e)
  , in :: SourceToken
  , body :: Expr e
  }

newtype Where e = Where
  { expr :: Expr e
  , bindings :: Maybe (Tuple SourceToken (NonEmptyArray (LetBinding e)))
  }

derive instance newtypeWhere :: Newtype (Where e) _

data LetBinding e
  = LetBindingSignature (Labeled (Name Ident) (Type e))
  | LetBindingName (ValueBindingFields e)
  | LetBindingPattern (Binder e) SourceToken (Where e)
  | LetBindingError e

type DoBlock e =
  { keyword :: SourceToken
  , statements :: NonEmptyArray (DoStatement e)
  }

data DoStatement e
  = DoLet SourceToken (NonEmptyArray (LetBinding e))
  | DoDiscard (Expr e)
  | DoBind (Binder e) SourceToken (Expr e)
  | DoError e

type AdoBlock e =
  { keyword :: SourceToken
  , statements :: Array (DoStatement e)
  , in :: SourceToken
  , result :: Expr e
  }

data Binder e
  = BinderWildcard SourceToken
  | BinderVar (Name Ident)
  | BinderNamed (Name Ident) SourceToken (Binder e)
  | BinderConstructor (QualifiedName Proper) (Array (Binder e))
  | BinderBoolean SourceToken Boolean
  | BinderChar SourceToken Char
  | BinderString SourceToken String
  | BinderInt (Maybe SourceToken) SourceToken IntValue
  | BinderNumber (Maybe SourceToken) SourceToken Number
  | BinderArray (Delimited (Binder e))
  | BinderRecord (Delimited (RecordLabeled (Binder e)))
  | BinderParens (Wrapped (Binder e))
  | BinderTyped (Binder e) SourceToken (Type e)
  | BinderOp (Binder e) (NonEmptyArray (Tuple (QualifiedName Operator) (Binder e)))
  | BinderError e

-- utility
type EncodeOutput = F.Foreign -- { tag :: String, contents :: Array F.Foreign }

encodeDataArg0 :: String -> EncodeOutput
encodeDataArg0 tag = encode { tag, contents : [] :: Array Unit}

encodeDataArg1 :: forall a. Encode a => String -> a -> EncodeOutput
encodeDataArg1 tag value = encode
  { tag: tag
  , contents: [encode value]
  }

encodeDataArg2 :: forall a b. Encode a => Encode b => String -> a -> b -> EncodeOutput
encodeDataArg2 tag value1 value2 = encode
  { tag: tag
  , contents: [encode value1, encode value2]
  }

encodeDataArg3 :: forall a b c. Encode a => Encode b => Encode c => String -> a -> b -> c -> EncodeOutput
encodeDataArg3 tag value1 value2 value3 = encode
  { tag: tag
  , contents: [encode value1, encode value2, encode value3]
  }

encodeDataArg4 :: forall a b c d. Encode a => Encode b => Encode c => Encode d => String -> a -> b -> c -> d -> EncodeOutput
encodeDataArg4 tag value1 value2 value3 value4 = encode
  { tag: tag
  , contents: [encode value1, encode value2, encode value3, encode value4]
  }

--- encode --- 
instance Encode ModuleName where
  encode (ModuleName str) = encode { tag: "ModuleName", contents: { unModuleName: encode str } }

instance Encode Ident where
  encode (Ident str) = encode { tag: "Ident", contents: { unIdent: encode str } }

instance Encode Proper where
  encode (Proper str) = encode { tag: "Proper", contents: { unProper: encode str } }

instance Encode Label where
  encode (Label str) = encode { tag: "Label", contents: { unLabel: encode str } }

instance Encode Operator where
  encode (Operator str) = encode { tag: "Operator", contents: { unOperator: encode str } }

instance Encode a => Encode (Name a) where
  encode (Name { token, name }) = encode { tag: "Name", contents: { token: encode token, name: encode name } }

instance Encode a => Encode (QualifiedName a) where
  encode (QualifiedName { token, module : m, name }) = encode { tag: "QualifiedName", contents: { token: encode token, module: encode m, name: encode name } }

instance (Encode a) => Encode (Wrapped a) where
  encode (Wrapped { open, value, close }) = encode { tag: "Wrapped", contents: { open: encode open, value: encode value, close: encode close } }

instance (Encode a) => Encode (Separated a) where
  encode (Separated { head, tail }) = encode { tag: "Separated", contents: { head: encode head, tail: encode tail } }

instance (Encode a, Encode b) => Encode (Labeled a b) where
  encode (Labeled { label, separator, value }) = encode { tag: "Labeled", contents: { label: encode label, separator: encode separator, value: encode value } }

instance Encode a => Encode (Prefixed a) where
  encode (Prefixed { prefix, value }) = encode { tag: "Prefixed", contents: { prefix: encode prefix, value: encode value } }

instance Encode (Row e) where
  encode (Row { labels, tail }) = encode { tag: "Row", contents: { labels: encode labels, tail: encode tail } }

instance Encode (Module e) where
  encode (Module { header, body }) = encode { tag: "Module", contents: { header: encode header, body: encode body } }

instance Encode (ModuleHeader e) where
  encode (ModuleHeader { keyword, name, exports, where : w, imports }) = encode 
    { tag: "ModuleHeader", contents: { keyword: encode keyword, name: encode name, exports: encode exports, where: encode w, imports: encode imports } }

instance Encode (ModuleBody e) where
  encode (ModuleBody { decls, trailingComments, end }) = encode 
    { tag: "ModuleBody", contents: { decls: encode decls, trailingComments: encode trailingComments, end: encode end } }

instance Encode (Instance e) where
  encode (Instance { head, body }) = encode { tag: "Instance", contents: { head: encode head, body: encode body } }

instance Encode (GuardedExpr e) where
  encode (GuardedExpr { bar, patterns, separator, where : w }) = encode 
    { tag: "GuardedExpr", contents: { bar: encode bar, patterns: encode patterns, separator: encode separator, where: encode w } }

instance Encode (PatternGuard e) where
  encode (PatternGuard { binder, expr }) = encode { tag: "PatternGuard", contents: { binder: encode binder, expr: encode expr } }

instance Encode (ImportDecl e) where
  encode (ImportDecl { keyword, module : m, names, qualified : q }) = encode 
    { tag: "ImportDecl", contents: { keyword: encode keyword, module: encode m, names: encode names, qualified: encode q } }

instance Encode (DataCtor e) where
  encode (DataCtor { name, fields }) = encode { tag: "DataCtor", contents: { name: encode name, fields: encode fields } }

instance Encode (Where e) where
  encode (Where { expr, bindings }) = encode { tag: "Where", contents: { expr: encode expr, bindings: encode bindings } }

-- Encode instances for remaining `data` types

instance Encode LineFeed where
  encode = case _ of
    LF -> encodeDataArg0 "LF"
    CRLF -> encodeDataArg0 "CRLF"

instance Encode SourceStyle where
  encode = case _ of
    ASCII -> encodeDataArg0 "ASCII"
    Unicode -> encodeDataArg0 "Unicode"

instance Encode IntValue where
  encode = case _ of
    SmallInt int -> encodeDataArg1 "SmallInt" int
    BigInt str -> encodeDataArg1 "BigInt" str
    BigHex str -> encodeDataArg1 "BigHex" str

instance Encode Token where
  encode = case _ of
    TokLeftParen -> encodeDataArg0 "TokLeftParen"
    TokRightParen -> encodeDataArg0 "TokRightParen"
    TokLeftBrace -> encodeDataArg0 "TokLeftBrace"
    TokRightBrace -> encodeDataArg0 "TokRightBrace"
    TokLeftSquare -> encodeDataArg0 "TokLeftSquare"
    TokRightSquare -> encodeDataArg0 "TokRightSquare"
    TokLeftArrow style -> encodeDataArg1 "TokLeftArrow" style
    TokRightArrow style -> encodeDataArg1 "TokRightArrow" style
    TokRightFatArrow style -> encodeDataArg1 "TokRightFatArrow" style
    TokDoubleColon style -> encodeDataArg1 "TokDoubleColon" style
    TokForall style -> encodeDataArg1 "TokForall" style
    TokEquals -> encodeDataArg0 "TokEquals"
    TokPipe -> encodeDataArg0 "TokPipe"
    TokTick -> encodeDataArg0 "TokTick"
    TokDot -> encodeDataArg0 "TokDot"
    TokComma -> encodeDataArg0 "TokComma"
    TokUnderscore -> encodeDataArg0 "TokUnderscore"
    TokBackslash -> encodeDataArg0 "TokBackslash"
    TokAt -> encodeDataArg0 "TokAt"
    TokLowerName maybeModule str -> encodeDataArg2 "TokLowerName" maybeModule str
    TokUpperName maybeModule str -> encodeDataArg2 "TokUpperName" maybeModule str
    TokOperator maybeModule str -> encodeDataArg2 "TokOperator" maybeModule str
    TokSymbolName maybeModule str -> encodeDataArg2 "TokSymbolName" maybeModule str
    TokSymbolArrow style -> encodeDataArg1 "TokSymbolArrow" style
    TokHole str -> encodeDataArg1 "TokHole" str
    TokChar str ch -> encodeDataArg2 "TokChar" str ch
    TokString str1 str2 -> encodeDataArg2 "TokString" str1 str2
    TokRawString str -> encodeDataArg1 "TokRawString" str
    TokInt str intValue -> encodeDataArg2 "TokInt" str intValue
    TokNumber str num -> encodeDataArg2 "TokNumber" str num
    TokLayoutStart int -> encodeDataArg1 "TokLayoutStart" int
    TokLayoutSep int -> encodeDataArg1 "TokLayoutSep" int
    TokLayoutEnd int -> encodeDataArg1 "TokLayoutEnd" int

instance Encode l => Encode (Comment l) where
  encode = case _ of
    Comment str -> encodeDataArg1 "Comment" str
    Space int -> encodeDataArg1 "Space" int
    Line l int -> encodeDataArg2 "Line" l int

instance Encode a => Encode (OneOrDelimited a) where
  encode = case _ of
    One a -> encodeDataArg1 "One" a
    Many delimited -> encodeDataArg1 "Many" delimited

instance Encode (Type e) where
  encode = case _ of
    TypeVar name -> encodeDataArg1 "TypeVar" name
    TypeConstructor name -> encodeDataArg1 "TypeConstructor" name
    TypeWildcard tok -> encodeDataArg1 "TypeWildcard" tok
    TypeHole name -> encodeDataArg1 "TypeHole" name
    TypeString tok str -> encodeDataArg2 "TypeString" tok str
    TypeInt maybeTok tok intValue -> encodeDataArg3 "TypeInt" maybeTok tok intValue
    TypeRow wrappedRow -> encodeDataArg1 "TypeRow" wrappedRow
    TypeRecord wrappedRow -> encodeDataArg1 "TypeRecord" wrappedRow
    TypeForall tok vars tok2 ty -> encodeDataArg4 "TypeForall" tok vars tok2 ty
    TypeKinded ty tok ty2 -> encodeDataArg3 "TypeKinded" ty tok ty2
    TypeApp ty tys -> encodeDataArg2 "TypeApp" ty tys
    TypeOp ty opTys -> encodeDataArg2 "TypeOp" ty opTys
    TypeOpName opName -> encodeDataArg1 "TypeOpName" opName
    TypeArrow ty tok ty2 -> encodeDataArg3 "TypeArrow" ty tok ty2
    TypeArrowName tok -> encodeDataArg1 "TypeArrowName" tok
    TypeConstrained ty tok ty2 -> encodeDataArg3 "TypeConstrained" ty tok ty2
    TypeParens wrappedTy -> encodeDataArg1 "TypeParens" wrappedTy
    TypeError e -> encodeDataArg0 "TypeError"

instance Encode a => Encode (TypeVarBinding a e) where
  encode = case _ of
    TypeVarKinded wrappedLabeled -> encodeDataArg1 "TypeVarKinded" wrappedLabeled
    TypeVarName a -> encodeDataArg1 "TypeVarName" a

instance Encode (Export e) where
  encode = case _ of
    ExportValue name -> encodeDataArg1 "ExportValue" name
    ExportOp name -> encodeDataArg1 "ExportOp" name
    ExportType name maybeDataMembers -> encodeDataArg2 "ExportType" name maybeDataMembers
    ExportTypeOp tok name -> encodeDataArg2 "ExportTypeOp" tok name
    ExportClass tok name -> encodeDataArg2 "ExportClass" tok name
    ExportModule tok name -> encodeDataArg2 "ExportModule" tok name
    ExportError e -> encodeDataArg0 "ExportError"

instance Encode DataMembers where
  encode = case _ of
    DataAll tok -> encodeDataArg1 "DataAll" tok
    DataEnumerated delimited -> encodeDataArg1 "DataEnumerated" delimited

instance Encode Fixity where
  encode = case _ of
    Infix -> encode "Infix"
    Infixl -> encode "Infixl"
    Infixr -> encode "Infixr"

instance Encode FixityOp where
  encode = case _ of
    FixityValue qualifiedName tok name -> encodeDataArg3 "FixityValue" qualifiedName tok name
    FixityType tok qualifiedName tok2 name -> encodeDataArg4 "FixityType" tok qualifiedName tok2 name

instance Encode (DoStatement e) where
  encode = case _ of
    DoLet tok bindings -> encodeDataArg2 "DoLet" tok bindings
    DoDiscard expr -> encodeDataArg1 "DoDiscard" expr
    DoBind binder tok expr -> encodeDataArg3 "DoBind" binder tok expr
    DoError e -> encodeDataArg0 "DoError"

instance Encode (Declaration e) where
  encode = case _ of
    DeclData head maybeTuple -> encodeDataArg2 "DeclData" head maybeTuple
    DeclType head tok ty -> encodeDataArg3 "DeclType" head tok ty
    DeclNewtype head tok name ty -> encodeDataArg4 "DeclNewtype" head tok name ty
    DeclClass classHead maybeTuple -> encodeDataArg2 "DeclClass" classHead maybeTuple
    DeclInstanceChain separated -> encodeDataArg1 "DeclInstanceChain" separated
    DeclDerive tok maybeTok instanceHead -> encodeDataArg3 "DeclDerive" tok maybeTok instanceHead
    DeclKindSignature tok labeled -> encodeDataArg2 "DeclKindSignature" tok labeled
    DeclSignature labeled -> encodeDataArg1 "DeclSignature" labeled
    DeclValue valueBindingFields -> encodeDataArg1 "DeclValue" valueBindingFields
    DeclFixity fixityFields -> encodeDataArg1 "DeclFixity" fixityFields
    DeclForeign tok tok2 f -> encodeDataArg3 "DeclForeign" tok tok2 f
    DeclRole tok tok2 name nonEmptyArray -> encodeDataArg4 "DeclRole" tok tok2 name nonEmptyArray
    DeclError e -> encodeDataArg0 "DeclError"

instance Encode (Expr e) where
  encode = case _ of
    ExprHole name -> encodeDataArg1 "ExprHole" name
    ExprSection tok -> encodeDataArg1 "ExprSection" tok
    ExprIdent qualName -> encodeDataArg1 "ExprIdent" qualName
    ExprConstructor qualName -> encodeDataArg1 "ExprConstructor" qualName
    ExprBoolean tok boolean -> encodeDataArg2 "ExprBoolean" tok boolean
    ExprChar tok char -> encodeDataArg2 "ExprChar" tok char
    ExprString tok str -> encodeDataArg2 "ExprString" tok str
    ExprInt tok intValue -> encodeDataArg2 "ExprInt" tok intValue
    ExprNumber tok number -> encodeDataArg2 "ExprNumber" tok number
    ExprArray delimited -> encodeDataArg1 "ExprArray" delimited
    ExprRecord delimited -> encodeDataArg1 "ExprRecord" delimited
    ExprParens wrapped -> encodeDataArg1 "ExprParens" wrapped
    ExprTyped expr tok type_ -> encodeDataArg3 "ExprTyped" expr tok type_
    ExprInfix expr nonEmptyArray -> encodeDataArg2 "ExprInfix" expr nonEmptyArray
    ExprOp expr nonEmptyArray -> encodeDataArg2 "ExprOp" expr nonEmptyArray
    ExprOpName qualName -> encodeDataArg1 "ExprOpName" qualName
    ExprNegate tok expr -> encodeDataArg2 "ExprNegate" tok expr
    ExprRecordAccessor recordAccessor -> encodeDataArg1 "ExprRecordAccessor" recordAccessor
    ExprRecordUpdate expr delimitedNonEmpty -> encodeDataArg2 "ExprRecordUpdate" expr delimitedNonEmpty
    ExprApp expr nonEmptyArray -> encodeDataArg2 "ExprApp" expr nonEmptyArray
    ExprLambda lambda -> encodeDataArg1 "ExprLambda" lambda
    ExprIf ifThenElse -> encodeDataArg1 "ExprIf" ifThenElse
    ExprCase caseOf -> encodeDataArg1 "ExprCase" caseOf
    ExprLet letIn -> encodeDataArg1 "ExprLet" letIn
    ExprDo doBlock -> encodeDataArg1 "ExprDo" doBlock
    ExprAdo adoBlock -> encodeDataArg1 "ExprAdo" adoBlock
    ExprError error -> encodeDataArg0 "ExprError"

instance Encode (LetBinding e) where
  encode = case _ of
    LetBindingSignature labeledType -> encodeDataArg1 "LetBindingSignature" labeledType
    LetBindingName valueBindingFields -> encodeDataArg1 "LetBindingName" valueBindingFields
    LetBindingPattern binder sourceToken whereExpr -> encodeDataArg3 "LetBindingPattern" binder sourceToken whereExpr
    LetBindingError e -> encodeDataArg0 "LetBindingError"

instance Encode (Binder e) where
  encode = case _ of
    BinderWildcard sourceToken -> encodeDataArg1 "BinderWildcard" sourceToken
    BinderVar nameIdent -> encodeDataArg1 "BinderVar" nameIdent
    BinderNamed nameIdent sourceToken binder -> encodeDataArg3 "BinderNamed" nameIdent sourceToken binder
    BinderConstructor qualifiedName binders -> encodeDataArg2 "BinderConstructor" qualifiedName binders
    BinderBoolean sourceToken boolean -> encodeDataArg2 "BinderBoolean" sourceToken boolean
    BinderChar sourceToken char -> encodeDataArg2 "BinderChar" sourceToken char
    BinderString sourceToken string -> encodeDataArg2 "BinderString" sourceToken string
    BinderInt maybeSourceToken sourceToken intValue -> encodeDataArg3 "BinderInt" maybeSourceToken sourceToken intValue
    BinderNumber maybeSourceToken sourceToken number -> encodeDataArg3 "BinderNumber" maybeSourceToken sourceToken number
    BinderArray delimitedBinder -> encodeDataArg1 "BinderArray" delimitedBinder
    BinderRecord delimitedRecordLabeledBinder -> encodeDataArg1 "BinderRecord" delimitedRecordLabeledBinder
    BinderParens wrappedBinder -> encodeDataArg1 "BinderParens" wrappedBinder
    BinderTyped binder sourceToken typeExpr -> encodeDataArg3 "BinderTyped" binder sourceToken typeExpr
    BinderOp binder nonEmptyTupleArray -> encodeDataArg2 "BinderOp" binder nonEmptyTupleArray
    BinderError e -> encodeDataArg0 "BinderError"

instance Encode (InstanceBinding e) where
  encode = case _ of
    InstanceBindingSignature labeledNameType -> encodeDataArg1 "InstanceBindingSignature" labeledNameType
    InstanceBindingName valueBindingFields -> encodeDataArg1 "InstanceBindingName" valueBindingFields

instance Encode (Import e) where
  encode = case _ of
    ImportValue nameIdent -> encodeDataArg1 "ImportValue" nameIdent
    ImportOp nameOperator -> encodeDataArg1 "ImportOp" nameOperator
    ImportType nameProper maybeDataMembers -> encodeDataArg2 "ImportType" nameProper maybeDataMembers
    ImportTypeOp sourceToken nameOperator -> encodeDataArg2 "ImportTypeOp" sourceToken nameOperator
    ImportClass sourceToken nameProper -> encodeDataArg2 "ImportClass" sourceToken nameProper
    ImportError e -> encodeDataArg0 "ImportError"

instance (Encode (f e)) => Encode (AppSpine f e) where
  encode = case _ of
    AppType sourceToken typeE -> encodeDataArg2 "AppType" sourceToken typeE
    AppTerm appTermFE -> encodeDataArg1 "AppTerm" appTermFE

instance Encode a => Encode (RecordLabeled a) where
  encode = case _ of
    RecordPun nameIdent -> encodeDataArg1 "RecordPun" nameIdent
    RecordField nameLabel sourceToken a -> encodeDataArg3 "RecordField" nameLabel sourceToken a

instance Encode (RecordUpdate e) where
  encode = case _ of
    RecordUpdateLeaf nameLabel sourceToken exprE -> encodeDataArg3 "RecordUpdateLeaf" nameLabel sourceToken exprE
    RecordUpdateBranch nameLabel delimitedNonEmptyRecordUpdateE -> encodeDataArg2 "RecordUpdateBranch" nameLabel delimitedNonEmptyRecordUpdateE

instance Encode ClassFundep where
  encode = case _ of
    FundepDetermined sourceToken names -> encodeDataArg2 "FundepDetermined" sourceToken names
    FundepDetermines names1 sourceToken names2 -> encodeDataArg3 "FundepDetermines" names1 sourceToken names2

instance Encode (Guarded e) where
  encode = case _ of
    Unconditional sourceToken whereExpr -> encodeDataArg2 "Unconditional" sourceToken whereExpr
    Guarded guardedExprs -> encodeDataArg1 "Guarded" guardedExprs

instance Encode (Foreign e) where
  encode = case _ of
    ForeignValue labeledType -> encodeDataArg1 "ForeignValue" labeledType
    ForeignData sourceToken labeledType -> encodeDataArg2 "ForeignData" sourceToken labeledType
    ForeignKind sourceToken nameProper -> encodeDataArg2 "ForeignKind" sourceToken nameProper

instance Encode Role where
  encode = case _ of
    Nominal -> encodeDataArg0 "Nominal"
    Representational -> encodeDataArg0 "Representational"
    Phantom -> encodeDataArg0 "Phantom"

