import ast
import gleam/option.{None, Some}
import nibble.{do}
import tokens.{
  type Identifier, type IntegerConstant, type Keyword, type StringConstant,
  type Symbol, type Token, And, BooleanKeyword, CharKeyword, ClassKeyword, Comma,
  ConstructorKeyword, Divide, DoKeyword, Dot, ElseKeyword, Eq, FalseKeyword,
  FieldKeyword, FunctionKeyword, Gt, Identifier, IfKeyword, IntKeyword,
  IntegerConstant, Keyword, LBrace, LBracket, LParen, LetKeyword, Lt,
  MethodKeyword, Minus, Multiply, NullKeyword, Or, Plus, RBrace, RBracket,
  RParen, ReturnKeyword, Semicolon, StaticKeyword, StringConstant, Symbol,
  ThisKeyword, Tilde, TrueKeyword, VarKeyword, VoidKeyword, WhileKeyword,
}

// Primitive token parsers
fn keyword_parser(expected: Keyword) -> nibble.Parser(Nil, Token, Nil) {
  nibble.take_if("keyword", fn(tok) {
    case tok {
      Keyword(k) -> k == expected
      _ -> False
    }
  })
  |> nibble.replace(Nil)
}

fn symbol_parser(expected: Symbol) -> nibble.Parser(Nil, Token, Nil) {
  nibble.take_if("symbol", fn(tok) {
    case tok {
      Symbol(s) -> s == expected
      _ -> False
    }
  })
  |> nibble.replace(Nil)
}

fn identifier_parser() -> nibble.Parser(Identifier, Token, Nil) {
  nibble.take_map("identifier", fn(tok) {
    case tok {
      Identifier(name) -> Some(name)
      _ -> None
    }
  })
}

fn int_parser() -> nibble.Parser(IntegerConstant, Token, Nil) {
  nibble.take_map("int", fn(tok) {
    case tok {
      IntegerConstant(i) -> Some(i)
      _ -> None
    }
  })
}

fn string_parser() -> nibble.Parser(StringConstant, Token, Nil) {
  nibble.take_map("string", fn(tok) {
    case tok {
      StringConstant(s) -> Some(s)
      _ -> None
    }
  })
}

// AST Parsers

pub fn class_parser() -> nibble.Parser(ast.Class, Token, Nil) {
  use _ <- do(keyword_parser(ClassKeyword))
  use name <- do(identifier_parser())
  use _ <- do(symbol_parser(LBrace))
  use class_var_decs <- do(nibble.many(class_var_dec_parser()))
  use class_subroutine_decs <- do(nibble.many(subroutine_dec_parser()))
  use _ <- do(symbol_parser(RBrace))
  nibble.return(ast.Class(
    name: name,
    class_var_decs: class_var_decs,
    class_subroutine_decs: class_subroutine_decs,
  ))
}

fn class_var_dec_parser() -> nibble.Parser(ast.ClassVarDec, Token, Nil) {
  use kind <- do(
    nibble.one_of([
      nibble.map(keyword_parser(StaticKeyword), fn(_) { ast.StaticKind }),
      nibble.map(keyword_parser(FieldKeyword), fn(_) { ast.FieldKind }),
    ]),
  )
  use var_type <- do(var_type_parser())
  use first_var <- do(identifier_parser())
  use rest_vars <- do(nibble.many(
    symbol_parser(Comma)
    |> nibble.then(fn(_) { identifier_parser() }),
  ))
  use _ <- do(symbol_parser(Semicolon))
  nibble.return(
    ast.ClassVarDec(kind: kind, var_type: var_type, var_names: [
      first_var,
      ..rest_vars
    ]),
  )
}

fn var_type_parser() -> nibble.Parser(ast.VarType, Token, Nil) {
  nibble.one_of([
    nibble.map(keyword_parser(IntKeyword), fn(_) { ast.VarInt }),
    nibble.map(keyword_parser(CharKeyword), fn(_) { ast.VarChar }),
    nibble.map(keyword_parser(BooleanKeyword), fn(_) { ast.VarBoolean }),
    nibble.map(identifier_parser(), fn(name) { ast.ClassName(name) }),
  ])
}

fn subroutine_dec_parser() -> nibble.Parser(ast.ClassSubroutineDec, Token, Nil) {
  use kind <- do(
    nibble.one_of([
      nibble.map(keyword_parser(ConstructorKeyword), fn(_) {
        ast.KindConstructor
      }),
      nibble.map(keyword_parser(FunctionKeyword), fn(_) { ast.KindFunction }),
      nibble.map(keyword_parser(MethodKeyword), fn(_) { ast.KindMethod }),
    ]),
  )
  use ret_type <- do(
    nibble.one_of([
      nibble.map(keyword_parser(VoidKeyword), fn(_) { ast.VoidType }),
      nibble.map(var_type_parser(), fn(t) { ast.VarType(t) }),
    ]),
  )
  use name <- do(identifier_parser())
  use _ <- do(symbol_parser(LParen))
  use parameter_list <- do(parameter_list_parser())
  use _ <- do(symbol_parser(RParen))
  use body <- do(subroutine_body_parser())
  nibble.return(ast.ClassSubroutineDec(
    kind: kind,
    subroutine_ret_type: ret_type,
    subroutine_name: name,
    parameter_list: parameter_list,
    subroutine_body: body,
  ))
}

fn parameter_list_parser() -> nibble.Parser(ast.ParameterList, Token, Nil) {
  nibble.one_of([
    nibble.map(
      var_type_parser()
        |> nibble.then(fn(t) {
          identifier_parser()
          |> nibble.map(fn(n) { ast.Parameter(param_type: t, param_name: n) })
        })
        |> nibble.then(fn(first_param) {
          nibble.many(
            symbol_parser(Comma)
            |> nibble.then(fn(_) {
              var_type_parser()
              |> nibble.then(fn(t) {
                identifier_parser()
                |> nibble.map(fn(n) {
                  // Return both the comma and the parameter
                  ast.CommaParam(
                    Comma,
                    ast.Parameter(param_type: t, param_name: n),
                  )
                })
              })
            }),
          )
          |> nibble.map(fn(rest) {
            // Flatten into a list of (maybe with comma tokens)
            ast.ParameterList(first_param, rest)
          })
        }),
      fn(params) { params },
    ),
    nibble.return(ast.EmptyList),
  ])
}

fn subroutine_body_parser() -> nibble.Parser(ast.SubroutineBody, Token, Nil) {
  use _ <- do(symbol_parser(LBrace))
  use var_decs <- do(nibble.many(var_dec_parser()))
  use stmts <- do(statements_parser())
  use _ <- do(symbol_parser(RBrace))
  nibble.return(ast.SubroutineBody(var_decs: var_decs, statements: stmts))
}

fn var_dec_parser() -> nibble.Parser(ast.VarDec, Token, Nil) {
  use _ <- do(keyword_parser(VarKeyword))
  use var_type <- do(var_type_parser())
  use first_var <- do(identifier_parser())
  use rest_vars <- do(nibble.many(
    symbol_parser(Comma)
    |> nibble.then(fn(_) { identifier_parser() }),
  ))
  use _ <- do(symbol_parser(Semicolon))
  nibble.return(
    ast.VarDec(var_type: var_type, var_names: [first_var, ..rest_vars]),
  )
}

fn statements_parser() -> nibble.Parser(ast.Statements, Token, Nil) {
  use stmts <- do(nibble.many(statement_parser()))
  nibble.succeed(ast.Statements(statements: stmts))
}

fn statement_parser() -> nibble.Parser(ast.Statement, Token, Nil) {
  nibble.one_of([
    nibble.map(let_statement_parser(), fn(s) { ast.LetStatement(s) }),
    nibble.map(if_statement_parser(), fn(s) { ast.IfStatement(s) }),
    nibble.map(while_statement_parser(), fn(s) { ast.WhileStatement(s) }),
    nibble.map(do_statement_parser(), fn(s) { ast.DoStatement(s) }),
    nibble.map(return_statement_parser(), fn(s) { ast.ReturnStatement(s) }),
  ])
}

fn let_statement_parser() -> nibble.Parser(ast.LetStatement, Token, Nil) {
  use _ <- do(keyword_parser(LetKeyword))
  use var_name <- do(identifier_parser())
  use opt_expr <- do(nibble.optional(
    symbol_parser(LBracket)
    |> nibble.then(fn(_) { expression_parser() })
    |> nibble.then(fn(expr) {
      symbol_parser(RBracket)
      |> nibble.replace(expr)
    }),
  ))
  use _ <- do(symbol_parser(Eq))
  use expr <- do(expression_parser())
  use _ <- do(symbol_parser(Semicolon))
  nibble.return(ast.LetStmt(
    var_name: var_name,
    opt_expr: opt_expr,
    eq_expr: expr,
  ))
}

fn if_statement_parser() -> nibble.Parser(ast.IfStatement, Token, Nil) {
  use _ <- do(keyword_parser(IfKeyword))
  use _ <- do(symbol_parser(LParen))
  use expr <- do(expression_parser())
  use _ <- do(symbol_parser(RParen))
  use _ <- do(symbol_parser(LBrace))
  use stmts <- do(statements_parser())
  use _ <- do(symbol_parser(RBrace))
  use else_stmt <- do(nibble.optional(
    keyword_parser(ElseKeyword)
    |> nibble.then(fn(_) {
      symbol_parser(LBrace)
      |> nibble.then(fn(_) {
        statements_parser()
        |> nibble.then(fn(stmts) {
          symbol_parser(RBrace)
          |> nibble.replace(ast.ElseStmt(statements: stmts))
        })
      })
    }),
  ))
  nibble.return(ast.IfStmt(
    expr: expr,
    statements: stmts,
    else_statement: else_stmt,
  ))
}

fn while_statement_parser() -> nibble.Parser(ast.WhileStatement, Token, Nil) {
  use _ <- do(keyword_parser(WhileKeyword))
  use _ <- do(symbol_parser(LParen))
  use expr <- do(expression_parser())
  use _ <- do(symbol_parser(RParen))
  use _ <- do(symbol_parser(LBrace))
  use stmts <- do(statements_parser())
  use _ <- do(symbol_parser(RBrace))
  nibble.return(ast.WhileStmt(expr: expr, statements: stmts))
}

fn do_statement_parser() -> nibble.Parser(ast.DoStatement, Token, Nil) {
  use _ <- do(keyword_parser(DoKeyword))
  use call <- do(subroutine_call_parser())
  use _ <- do(symbol_parser(Semicolon))
  nibble.return(ast.DoStmt(subroutine_call: call))
}

fn return_statement_parser() -> nibble.Parser(ast.ReturnStatement, Token, Nil) {
  use _ <- do(keyword_parser(ReturnKeyword))
  use expr <- do(nibble.optional(expression_parser()))
  use _ <- do(symbol_parser(Semicolon))
  nibble.return(ast.ReturnStmt(expr: expr))
}

// Expressions and Terms

fn expression_parser() -> nibble.Parser(ast.Expression, Token, Nil) {
  use term <- do(term_parser())
  use rest <- do(
    nibble.optional(nibble.many(
      op_parser()
      |> nibble.then(fn(op) {
        term_parser()
        |> nibble.map(fn(t) { ast.OpTerm(op: op, term: t) })
      }),
    )),
  )
  nibble.return(ast.Expr(term: term, rest: rest))
}

fn op_parser() -> nibble.Parser(ast.Op, Token, Nil) {
  nibble.one_of([
    nibble.map(symbol_parser(Plus), fn(_) { ast.Plus(Plus) }),
    nibble.map(symbol_parser(Minus), fn(_) { ast.Minus(Minus) }),
    nibble.map(symbol_parser(Multiply), fn(_) { ast.Mult(Multiply) }),
    nibble.map(symbol_parser(Divide), fn(_) { ast.Div(Divide) }),
    nibble.map(symbol_parser(And), fn(_) { ast.And(And) }),
    nibble.map(symbol_parser(Or), fn(_) { ast.Or(Or) }),
    nibble.map(symbol_parser(Lt), fn(_) { ast.Lt(Lt) }),
    nibble.map(symbol_parser(Gt), fn(_) { ast.Gt(Gt) }),
    nibble.map(symbol_parser(Eq), fn(_) { ast.Eq(Eq) }),
  ])
}

fn term_parser() -> nibble.Parser(ast.Term, Token, Nil) {
  nibble.one_of([
    nibble.map(int_parser(), fn(i) { ast.IntegerConstant(i) }),
    nibble.map(string_parser(), fn(s) { ast.StringConstant(s) }),
    nibble.map(
      nibble.one_of([
        nibble.map(keyword_parser(TrueKeyword), fn(_) { ast.True }),
        nibble.map(keyword_parser(FalseKeyword), fn(_) { ast.False }),
        nibble.map(keyword_parser(NullKeyword), fn(_) { ast.Null }),
        nibble.map(keyword_parser(ThisKeyword), fn(_) { ast.This }),
      ]),
      fn(k) { ast.KeywordConstant(k) },
    ),
    // varName[expr]
    nibble.map(
      identifier_parser()
        |> nibble.then(fn(name) {
          symbol_parser(LBracket)
          |> nibble.then(fn(_) {
            expression_parser()
            |> nibble.then(fn(expr) {
              symbol_parser(RBracket)
              |> nibble.replace(
                ast.VNameExpr(ast.VarNameExpr(var_name: name, expr: expr)),
              )
            })
          })
        }),
      fn(v) { v },
    ),
    // subroutine call
    nibble.map(subroutine_call_parser(), fn(call) { ast.SubroutineCall(call) }),
    // (expression)
    nibble.map(
      symbol_parser(LParen)
        |> nibble.then(fn(_) { expression_parser() })
        |> nibble.then(fn(expr) {
          symbol_parser(RParen)
          |> nibble.replace(expr)
        }),
      fn(expr) { ast.Expression(expr) },
    ),
    // unaryOp term
    nibble.map(
      nibble.one_of([
        nibble.map(symbol_parser(Minus), fn(_) { ast.UMinus(Minus) }),
        nibble.map(symbol_parser(Tilde), fn(_) { ast.Neg(Tilde) }),
      ])
        |> nibble.then(fn(op) {
          term_parser()
          |> nibble.map(fn(term) {
            ast.UnaryOpTerm(ast.UOpTerm(op: op, term: term))
          })
        }),
      fn(u) { u },
    ),
    // varName
    nibble.map(identifier_parser(), fn(name) { ast.VarName(name) }),
  ])
}

fn subroutine_call_parser() -> nibble.Parser(ast.SubroutineCall, Token, Nil) {
  nibble.one_of([
    // flat: subroutineName '(' exprList ')'
    nibble.map(
      identifier_parser()
        |> nibble.then(fn(name) {
          symbol_parser(LParen)
          |> nibble.then(fn(_) {
            expr_list_parser()
            |> nibble.then(fn(exprs) {
              symbol_parser(RParen)
              |> nibble.replace(ast.FlatSubroutineCall(
                subroutine_name: name,
                expr_list: exprs,
              ))
            })
          })
        }),
      fn(call) { ast.FSubroutineCall(call) },
    ),
    // nested: (className | varName) '.' subroutineName '(' exprList ')'
    nibble.map(
      identifier_parser()
        |> nibble.then(fn(obj) {
          symbol_parser(Dot)
          |> nibble.then(fn(_) {
            identifier_parser()
            |> nibble.then(fn(sub) {
              symbol_parser(LParen)
              |> nibble.then(fn(_) {
                expr_list_parser()
                |> nibble.then(fn(exprs) {
                  symbol_parser(RParen)
                  |> nibble.replace(
                    ast.NSubroutineCall(ast.NestedSubroutineCall(
                      name: obj,
                      call: ast.FlatSubroutineCall(
                        subroutine_name: sub,
                        expr_list: exprs,
                      ),
                    )),
                  )
                })
              })
            })
          })
        }),
      fn(call) { call },
    ),
  ])
}

fn expr_list_parser() -> nibble.Parser(ast.ExprList, Token, Nil) {
  nibble.one_of([
    nibble.map(
      nibble.sequence(expression_parser(), symbol_parser(Comma)),
      fn(exprs) { exprs },
    ),
    nibble.return([]),
  ])
}
