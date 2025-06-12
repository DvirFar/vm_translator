// ========================================
// Imports
// ========================================
import gleam/option.{type Option}
import tokens.{
  type Identifier, type IntegerConstant, type StringConstant, type Symbol,
}

// ========================================
// Class and Subroutine Declarations
// ========================================

/// Represents a complete class in the AST.
pub type Class {
  Class(
    name: ClassName,
    class_var_decs: List(ClassVarDec),
    class_subroutine_decs: List(ClassSubroutineDec),
  )
}

/// The name of a class.
pub type ClassName =
  Identifier

/// The name of a subroutine (method/function/constructor).
pub type SubroutineName =
  Identifier

/// The name of a variable.
pub type VarName =
  Identifier

// ========================================
// Class Variable Declarations
// ========================================

/// Declaration of a class variable.
pub type ClassVarDec {
  ClassVarDec(kind: ClassVarKind, var_type: VarType, var_names: List(VarName))
}

/// The kind of class variable: static or field.
pub type ClassVarKind {
  StaticKind
  FieldKind
}

/// The type of a variable (primitive or class).
pub type VarType {
  VarInt
  VarChar
  VarBoolean
  ClassName(ClassName)
}

// ========================================
// Subroutine Declarations
// ========================================

/// Declaration of a class subroutine.
pub type ClassSubroutineDec {
  ClassSubroutineDec(
    kind: SubroutineKind,
    subroutine_ret_type: RetType,
    subroutine_name: SubroutineName,
    parameter_list: ParameterList,
    subroutine_body: SubroutineBody,
  )
}

/// The return type of a subroutine.
pub type RetType {
  VarType(VarType)
  VoidType
}

/// The kind of subroutine.
pub type SubroutineKind {
  KindConstructor
  KindFunction
  KindMethod
}

/// A type wrapper for identifiers.
pub type Type =
  Identifier

/// List of parameters for a subroutine.
pub type ParameterList {
  EmptyList
  ParameterList(first: Parameter, rest: List(CommaParam))
}

/// A single parameter in a parameter list.
pub type Parameter {
  Parameter(param_type: VarType, param_name: VarName)
}

pub type CommaParam {
  CommaParam(comma: Symbol, param: Parameter)
}

/// The body of a subroutine, including variable declarations and statements.
pub type SubroutineBody {
  SubroutineBody(var_decs: List(VarDec), statements: Statements)
}

/// Declaration of a local variable.
pub type VarDec {
  VarDec(var_type: VarType, var_names: List(VarName))
}

// ========================================
// Statements
// ========================================

/// A list of statements.
pub type Statements {
  Statements(statements: List(Statement))
}

/// A single statement.
pub type Statement {
  LetStatement(LetStatement)
  IfStatement(IfStatement)
  WhileStatement(WhileStatement)
  DoStatement(DoStatement)
  ReturnStatement(ReturnStatement)
}

/// A let statement (assignment).
pub type LetStatement {
  LetStmt(var_name: VarName, opt_expr: Option(Expression), eq_expr: Expression)
}

/// An if statement, possibly with an else branch.
pub type IfStatement {
  IfStmt(
    expr: Expression,
    statements: Statements,
    else_statement: Option(ElseStatement),
  )
}

/// The else branch of an if statement.
pub type ElseStatement {
  ElseStmt(statements: Statements)
}

/// A while statement.
pub type WhileStatement {
  WhileStmt(expr: Expression, statements: Statements)
}

/// A do statement (subroutine call).
pub type DoStatement {
  DoStmt(subroutine_call: SubroutineCall)
}

/// A return statement.
pub type ReturnStatement {
  ReturnStmt(expr: Option(Expression))
}

// ========================================
// Expressions and Terms
// ========================================

/// An expression, possibly with chained operations.
pub type Expression {
  Expr(term: Term, rest: Option(List(OpTerm)))
}

/// An operator and a term (for chained expressions).
pub type OpTerm {
  OpTerm(op: Op, term: Term)
}

/// A term in an expression.
pub type Term {
  IntegerConstant(IntegerConstant)
  StringConstant(StringConstant)
  KeywordConstant(KeywordConstant)
  VarName(VarName)
  VNameExpr(VarNameExpr)
  SubroutineCall(SubroutineCall)
  Expression(Expression)
  UnaryOpTerm(UnaryOpTerm)
}

// ========================================
// Subroutine Calls
// ========================================

/// A subroutine call (flat or nested).
pub type SubroutineCall {
  FSubroutineCall(FlatSubroutineCall)
  NSubroutineCall(NestedSubroutineCall)
}

/// A direct subroutine call.
pub type FlatSubroutineCall {
  FlatSubroutineCall(subroutine_name: SubroutineName, expr_list: ExprList)
}

/// A nested subroutine call (class or variable).
pub type NestedSubroutineCall {
  NestedSubroutineCall(name: Identifier, call: FlatSubroutineCall)
}

/// A list of expressions (arguments).
pub type ExprList =
  List(Expression)

// ========================================
// Operators and Constants
// ========================================

/// Binary operators.
pub type Op {
  Plus(Symbol)
  Minus(Symbol)
  Mult(Symbol)
  Div(Symbol)
  And(Symbol)
  Or(Symbol)
  Lt(Symbol)
  Gt(Symbol)
  Eq(Symbol)
}

/// Unary operators.
pub type UnaryOp {
  UMinus(Symbol)
  Neg(Symbol)
}

/// Keyword constants (true, false, null, this).
pub type KeywordConstant {
  True
  False
  Null
  This
}

// ========================================
// Additional Types
// ========================================

/// A variable name with an expression (for array access).
pub type VarNameExpr {
  VarNameExpr(var_name: VarName, expr: Expression)
}

/// A unary operation applied to a term.
pub type UnaryOpTerm {
  UOpTerm(op: UnaryOp, term: Term)
}
