pub type Keyword {
  ClassKeyword
  ConstructorKeyword
  FunctionKeyword
  MethodKeyword
  FieldKeyword
  StaticKeyword
  VarKeyword
  IntKeyword
  CharKeyword
  BooleanKeyword
  VoidKeyword
  NullKeyword
  ThisKeyword
  TrueKeyword
  FalseKeyword
  IfKeyword
  ElseKeyword
  LetKeyword
  DoKeyword
  WhileKeyword
  ReturnKeyword
}

pub type Symbol {
  LParen
  RParen
  LBracket
  RBracket
  LBrace
  RBrace
  Dot
  Comma
  Plus
  Minus
  Multiply
  Divide
  Semicolon
  Colon
  And
  Or
  Lt
  Gt
  Eq
  Tilde
}

pub type Identifier =
  String

pub type IntegerConstant =
  Int

pub type StringConstant =
  String

pub type Token {
  Keyword(Keyword)
  Identifier(Identifier)
  IntegerConstant(IntegerConstant)
  Symbol(Symbol)
  StringConstant(StringConstant)
  Comment(String)
}
