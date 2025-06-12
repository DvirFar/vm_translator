// author: Dvir Farkash 329398911 150060.3.5785.42

import argv
import ast
import gleam/int
import gleam/list
import gleam/option.{Some}
import gleam/result
import gleam/set
import gleam/string
import nibble
import nibble/lexer
import parser.{class_parser}
import simplifile
import tokens.{
  type Token, And, BooleanKeyword, CharKeyword, ClassKeyword, Colon, Comma,
  Comment, ConstructorKeyword, Divide, DoKeyword, Dot, ElseKeyword, Eq,
  FalseKeyword, FieldKeyword, FunctionKeyword, Gt, Identifier, IfKeyword,
  IntKeyword, IntegerConstant, Keyword, LBrace, LBracket, LParen, LetKeyword, Lt,
  MethodKeyword, Minus, Multiply, NullKeyword, Or, Plus, RBrace, RBracket,
  RParen, ReturnKeyword, Semicolon, StaticKeyword, StringConstant, Symbol,
  ThisKeyword, Tilde, TrueKeyword, VarKeyword, VoidKeyword, WhileKeyword,
}

type LexerState {
  LexerState
}

pub opaque type Lexer {
  Lexer(impl: lexer.Lexer(Token, LexerState))
}

pub type Error(ctx) {
  NoMatchingTokenError(row: Int, col: Int, lexeme: String)
}

pub fn new() -> Lexer {
  lexer.advanced(fn(_) {
    [
      // Comment handling
      multi_line_comment() |> lexer.ignore,
      single_line_comment() |> lexer.ignore,
      // Layout keywords
      lexer.keyword("class", "\\W+", Keyword(ClassKeyword)),
      lexer.keyword("constructor", "\\W+", Keyword(ConstructorKeyword)),
      lexer.keyword("function", "\\W+", Keyword(FunctionKeyword)),
      lexer.keyword("method", "\\W+", Keyword(MethodKeyword)),
      lexer.keyword("field", "\\W+", Keyword(FieldKeyword)),
      lexer.keyword("static", "\\W+", Keyword(StaticKeyword)),
      lexer.keyword("var", "\\W+", Keyword(VarKeyword)),
      lexer.keyword("int", "\\W+", Keyword(IntKeyword)),
      lexer.keyword("char", "\\W+", Keyword(CharKeyword)),
      lexer.keyword("boolean", "\\W+", Keyword(BooleanKeyword)),
      lexer.keyword("void", "\\W+", Keyword(VoidKeyword)),
      lexer.keyword("null", "\\W+", Keyword(NullKeyword)),
      lexer.keyword("this", "\\W+", Keyword(ThisKeyword)),
      lexer.keyword("true", "\\W+", Keyword(TrueKeyword)),
      lexer.keyword("false", "\\W+", Keyword(FalseKeyword)),
      lexer.keyword("if", "\\W+", Keyword(IfKeyword)),
      lexer.keyword("else", "\\W+", Keyword(ElseKeyword)),
      lexer.keyword("let", "\\W+", Keyword(LetKeyword)),
      lexer.keyword("do", "\\W+", Keyword(DoKeyword)),
      lexer.keyword("while", "\\W+", Keyword(WhileKeyword)),
      lexer.keyword("return", "\\W+", Keyword(ReturnKeyword)),
      // Operators (single-character)
      lexer.token("+", Symbol(Plus)),
      lexer.token("-", Symbol(Minus)),
      lexer.symbol("*", "\\s+", Symbol(Multiply)),
      lexer.symbol("/", "\\s+", Symbol(Divide)),
      lexer.token("<", Symbol(Lt)),
      lexer.token(">", Symbol(Gt)),
      lexer.token("=", Symbol(Eq)),
      lexer.token("(", Symbol(LParen)),
      lexer.token(")", Symbol(RParen)),
      lexer.token("[", Symbol(LBracket)),
      lexer.token("]", Symbol(RBracket)),
      lexer.token("{", Symbol(LBrace)),
      lexer.token("}", Symbol(RBrace)),
      lexer.token(";", Symbol(Semicolon)),
      lexer.token(":", Symbol(Colon)),
      lexer.token(".", Symbol(Dot)),
      lexer.token(",", Symbol(Comma)),
      lexer.token("&", Symbol(And)),
      lexer.token("|", Symbol(Or)),
      lexer.token("~", Symbol(Tilde)),
      // Identifiers and literals
      lexer.identifier("[a-zA-Z_]", "[A-Za-z0-9_]", set.new(), Identifier),
      // Number literals
      lexer.int(IntegerConstant),
      // TODO: Properly handle string, esp. multi-line strings.
      // String literals
      lexer.string("\"", StringConstant),
      // Whitespace handling
      lexer.whitespace(Nil)
        |> lexer.ignore,
    ]
  })
  |> Lexer
}

pub fn run(
  lx: Lexer,
  source: String,
) -> Result(List(lexer.Token(Token)), lexer.Error) {
  source
  |> lexer.run_advanced(LexerState, lx.impl)
}

pub fn single_line_comment() -> lexer.Matcher(Token, mode) {
  let start = "//"
  use mode, lexeme, lookahead <- lexer.custom()

  case string.starts_with(lexeme, start), lookahead {
    True, "\r\n" | True, "\n" ->
      lexeme
      |> string.drop_start(string.length(start))
      |> Comment
      |> lexer.Keep(mode)
    True, _ -> lexer.Skip
    False, _ -> lexer.NoMatch
  }
}

pub fn multi_line_comment() -> lexer.Matcher(Token, mode) {
  let start = "/*"
  let end = "*/"
  use mode, lexeme, lookahead <- lexer.custom()

  case
    string.starts_with(lexeme, start),
    string.ends_with(lexeme, end),
    lexeme <> lookahead
  {
    True, True, _ ->
      lexeme
      |> string.drop_start(string.length(start))
      |> string.drop_end(string.length(end))
      |> Comment
      |> lexer.Keep(mode)
    True, _, _ -> lexer.Skip
    _, _, "/*" -> lexer.Skip
    False, _, _ -> lexer.NoMatch
  }
}

// Parse a string into an integer
pub fn parse_int(str: String) {
  let assert Ok(result) = int.parse(str)
  result
}

pub fn to_xml_t(tokens: List(lexer.Token(Token)), output_file: String) {
  let _ = simplifile.delete(output_file)
  let _ = simplifile.append(output_file, "<tokens>\n")

  list.each(tokens, fn(token) {
    case token.value {
      Keyword(_) ->
        simplifile.append(
          output_file,
          "<keyword> " <> token.lexeme <> " </keyword>\n",
        )
      Identifier(id) ->
        simplifile.append(
          output_file,
          "<identifier> " <> id <> " </identifier>\n",
        )
      IntegerConstant(num) ->
        simplifile.append(
          output_file,
          "<integerConstant> " <> int.to_string(num) <> " </integerConstant>\n",
        )
      Symbol(sym) -> {
        let _ = simplifile.append(output_file, "<symbol> ")
        let _ = case sym {
          Lt -> simplifile.append(output_file, "&lt;")
          Gt -> simplifile.append(output_file, "&gt;")
          And -> simplifile.append(output_file, "&amp;")
          _ -> simplifile.append(output_file, token.lexeme)
        }
        simplifile.append(output_file, " </symbol>\n")
      }
      StringConstant(str) ->
        simplifile.append(
          output_file,
          "<stringConstant> " <> str <> " </stringConstant>\n",
        )
      _ -> Ok(Nil)
    }
  })

  Ok(simplifile.append(output_file, "</tokens>\n"))
}

// This function assumes the XML is well-formed and trimmed.
pub fn xml_line_to_token(line: String) -> Result(Token, Nil) {
  case line {
    "<keyword> " <> rest -> {
      let assert [kw, _] = string.split(rest, " </keyword>")
      Ok(Keyword(parse_keyword(kw)))
    }
    "<identifier> " <> rest -> {
      let assert [id, _] = string.split(rest, " </identifier>")
      Ok(Identifier(id))
    }
    "<symbol> " <> rest -> {
      let assert [sym, _] = string.split(rest, " </symbol>")
      Ok(Symbol(parse_symbol(sym)))
    }
    "<integerConstant> " <> rest -> {
      let assert [num, _] = string.split(rest, " </integerConstant>")
      Ok(IntegerConstant(parse_int(num)))
    }
    "<stringConstant> " <> rest -> {
      let assert [str, _] = string.split(rest, " </stringConstant>")
      Ok(StringConstant(str))
    }
    _ -> Error(Nil)
  }
}

fn parse_keyword(kw: String) -> tokens.Keyword {
  case kw {
    "class" -> ClassKeyword
    "constructor" -> ConstructorKeyword
    "function" -> FunctionKeyword
    "method" -> MethodKeyword
    "field" -> FieldKeyword
    "static" -> StaticKeyword
    "var" -> VarKeyword
    "int" -> IntKeyword
    "char" -> CharKeyword
    "boolean" -> BooleanKeyword
    "void" -> VoidKeyword
    "null" -> NullKeyword
    "this" -> ThisKeyword
    "true" -> TrueKeyword
    "false" -> FalseKeyword
    "if" -> IfKeyword
    "else" -> ElseKeyword
    "let" -> LetKeyword
    "do" -> DoKeyword
    "while" -> WhileKeyword
    "return" -> ReturnKeyword
    _ -> panic as "No such keyword!"
  }
}

fn parse_symbol(sym: String) -> tokens.Symbol {
  case sym {
    "+" -> Plus
    "-" -> Minus
    "*" -> Multiply
    "/" -> Divide
    "<" | "&lt;" -> Lt
    ">" | "&gt;" -> Gt
    "=" -> Eq
    "(" -> LParen
    ")" -> RParen
    "[" -> LBracket
    "]" -> RBracket
    "{" -> LBrace
    "}" -> RBrace
    ";" -> Semicolon
    ":" -> Colon
    "." -> Dot
    "," -> Comma
    "&" | "&amp;" -> And
    "|" -> Or
    "~" -> Tilde
    _ -> panic as sym
  }
}

pub fn tokens_from_xml_file(path: String) -> List(lexer.Token(Token)) {
  let assert Ok(lines) = simplifile.read(path)
  let lines = string.split(lines, "\n")
  // Skip <tokens> and </tokens>
  let token_lines =
    list.drop(lines, 1) |> list.take_while(fn(l) { l != "</tokens>" })
  list.map(
    list.filter_map(token_lines, xml_line_to_token),
    fn(tok) -> lexer.Token(Token) {
      lexer.Token(lexer.Span(0, 0, 0, 0), "", tok)
    },
  )
}

pub fn to_ast(tokens_file: String) {
  let tokens = tokens_from_xml_file(tokens_file)
  case nibble.run(tokens, class_parser()) {
    Ok(ast) -> Ok(ast)
    Error(_) -> Error("Parse error")
  }
}

pub fn to_xml(tokens_file: String, output_file: String) {
  let assert Ok(ast) = to_ast(tokens_file)
  let _ = simplifile.delete(output_file)
  let _ = simplifile.append(output_file, "<class>\n")
  let _ = ast_to_xml(ast, output_file, 1)
  let _ = simplifile.append(output_file, "</class>\n")
}

// Helper to indent XML
fn indent(level: Int) -> String {
  string.repeat("  ", level)
}

// Main AST to XML dispatcher
fn ast_to_xml(ast, output_file: String, level: Int) {
  case ast {
    ast.Class(name, var_decs, subroutines) -> {
      let _ =
        simplifile.append(
          output_file,
          indent(level) <> "<keyword> class </keyword>\n",
        )
      let _ =
        simplifile.append(
          output_file,
          indent(level) <> "<identifier> " <> name <> " </identifier>\n",
        )
      let _ =
        simplifile.append(
          output_file,
          indent(level) <> "<symbol> { </symbol>\n",
        )
      list.each(var_decs, fn(v) { class_var_dec_to_xml(v, output_file, level) })
      list.each(subroutines, fn(s) {
        subroutine_dec_to_xml(s, output_file, level)
      })
      let _ =
        simplifile.append(
          output_file,
          indent(level) <> "<symbol> } </symbol>\n",
        )
    }
  }
}

// ClassVarDec(StaticKind, VarBoolean, ["test"])
fn class_var_dec_to_xml(var_dec, output_file: String, level: Int) {
  let _ = simplifile.append(output_file, indent(level) <> "<classVarDec>\n")
  let _ = case var_dec {
    ast.ClassVarDec(kind, typ, names) -> {
      let _ =
        simplifile.append(
          output_file,
          indent(level + 1)
            <> "<keyword> "
            <> class_var_kind_to_str(kind)
            <> " </keyword>\n",
        )
      let _ =
        simplifile.append(output_file, indent(level + 1) <> type_to_str(typ))
      case names {
        [] -> Nil
        [first, ..rest] -> {
          let _ =
            simplifile.append(
              output_file,
              indent(level + 1)
                <> "<identifier> "
                <> first
                <> " </identifier>\n",
            )
          list.fold(rest, 0, fn(idx, n) {
            let _ =
              simplifile.append(
                output_file,
                indent(level + 1) <> "<symbol> , </symbol>\n",
              )
            let _ =
              simplifile.append(
                output_file,
                indent(level + 1) <> "<identifier> " <> n <> " </identifier>\n",
              )
            idx + 1
          })
          Nil
        }
      }
      let _ =
        simplifile.append(
          output_file,
          indent(level + 1) <> "<symbol> ; </symbol>\n",
        )
    }
  }
  let _ = simplifile.append(output_file, indent(level) <> "</classVarDec>\n")
}

fn class_var_kind_to_str(kind) {
  case kind {
    ast.StaticKind -> "static"
    ast.FieldKind -> "field"
  }
}

fn type_to_str(typ) {
  case typ {
    ast.VarBoolean -> "<keyword> boolean </keyword>\n"
    ast.VarInt -> "<keyword> int </keyword>\n"
    ast.VarChar -> "<keyword> char </keyword>\n"
    ast.ClassName(n) -> "<identifier> " <> n <> " </identifier>\n"
  }
}

fn ret_type_to_str(rtyp) {
  case rtyp {
    ast.VarType(v) -> type_to_str(v)
    ast.VoidType -> "<keyword> void </keyword>\n"
  }
}

fn subroutine_dec_to_xml(sub, output_file: String, level: Int) {
  let _ = simplifile.append(output_file, indent(level) <> "<subroutineDec>\n")
  let _ = case sub {
    ast.ClassSubroutineDec(kind, typ, name, params, body) -> {
      let _ =
        simplifile.append(
          output_file,
          indent(level + 1)
            <> "<keyword> "
            <> subroutine_kind_to_str(kind)
            <> " </keyword>\n",
        )
      let _ =
        simplifile.append(
          output_file,
          indent(level + 1) <> ret_type_to_str(typ),
        )
      let _ =
        simplifile.append(
          output_file,
          indent(level + 1) <> "<identifier> " <> name <> " </identifier>\n",
        )
      let _ =
        simplifile.append(
          output_file,
          indent(level + 1) <> "<symbol> ( </symbol>\n",
        )
      let _ =
        simplifile.append(output_file, indent(level + 1) <> "<parameterList>\n")
      parameter_list_to_xml(params, output_file, level + 2)
      let _ =
        simplifile.append(
          output_file,
          indent(level + 1) <> "</parameterList>\n",
        )
      let _ =
        simplifile.append(
          output_file,
          indent(level + 1) <> "<symbol> ) </symbol>\n",
        )
      subroutine_body_to_xml(body, output_file, level + 1)
    }
  }
  let _ = simplifile.append(output_file, indent(level) <> "</subroutineDec>\n")
}

fn subroutine_kind_to_str(kind) {
  case kind {
    ast.KindFunction -> "function"
    ast.KindMethod -> "method"
    ast.KindConstructor -> "constructor"
  }
}

fn parameter_list_to_xml(params, output_file: String, level: Int) {
  // params: List(Parameter(Type, String))
  case params {
    ast.ParameterList(first, rest) -> {
      let _ = case first {
        ast.Parameter(typ, name) -> {
          let _ =
            simplifile.append(output_file, indent(level) <> type_to_str(typ))
          let _ =
            simplifile.append(
              output_file,
              indent(level) <> "<identifier> " <> name <> " </identifier>\n",
            )
        }
      }
      list.each(rest, fn(param) {
        case param {
          ast.CommaParam(_, p) -> {
            let _ =
              simplifile.append(
                output_file,
                indent(level) <> "<symbol> , </symbol>\n",
              )
            case p {
              ast.Parameter(typ, name) -> {
                let _ =
                  simplifile.append(
                    output_file,
                    indent(level) <> type_to_str(typ),
                  )
                let _ =
                  simplifile.append(
                    output_file,
                    indent(level)
                      <> "<identifier> "
                      <> name
                      <> " </identifier>\n",
                  )
              }
            }
          }
        }
      })
    }
    ast.EmptyList -> Nil
  }
}

fn subroutine_body_to_xml(body, output_file: String, level: Int) {
  let _ = simplifile.append(output_file, indent(level) <> "<subroutineBody>\n")
  let _ = case body {
    ast.SubroutineBody(var_decs, statements) -> {
      let _ =
        simplifile.append(
          output_file,
          indent(level + 1) <> "<symbol> { </symbol>\n",
        )
      list.each(var_decs, fn(v) { var_dec_to_xml(v, output_file, level + 1) })
      let _ = statements_to_xml(statements, output_file, level + 1)
      let _ =
        simplifile.append(
          output_file,
          indent(level + 1) <> "<symbol> } </symbol>\n",
        )
    }
  }
  let _ = simplifile.append(output_file, indent(level) <> "</subroutineBody>\n")
}

fn var_dec_to_xml(var_dec, output_file: String, level: Int) {
  let _ = simplifile.append(output_file, indent(level) <> "<varDec>\n")
  let _ = case var_dec {
    ast.VarDec(typ, names) -> {
      let _ =
        simplifile.append(
          output_file,
          indent(level + 1) <> "<keyword> var </keyword>\n",
        )
      let _ =
        simplifile.append(output_file, indent(level + 1) <> type_to_str(typ))
      let _ = case names {
        [] -> Nil
        [first, ..rest] -> {
          let _ =
            simplifile.append(
              output_file,
              indent(level + 1)
                <> "<identifier> "
                <> first
                <> " </identifier>\n",
            )
          list.fold(rest, 0, fn(idx, n) {
            let _ =
              simplifile.append(
                output_file,
                indent(level + 1) <> "<symbol> , </symbol>\n",
              )
            let _ =
              simplifile.append(
                output_file,
                indent(level + 1) <> "<identifier> " <> n <> " </identifier>\n",
              )
            idx + 1
          })
          Nil
        }
      }
      let _ =
        simplifile.append(
          output_file,
          indent(level + 1) <> "<symbol> ; </symbol>\n",
        )
    }
  }
  let _ = simplifile.append(output_file, indent(level) <> "</varDec>\n")
}

fn statements_to_xml(stmts, output_file: String, level: Int) {
  let _ = simplifile.append(output_file, indent(level) <> "<statements>\n")
  let _ = case stmts {
    ast.Statements(list) ->
      list.each(list, fn(s) { statement_to_xml(s, output_file, level + 1) })
  }
  let _ = simplifile.append(output_file, indent(level) <> "</statements>\n")
}

fn statement_to_xml(stmt, output_file: String, level: Int) {
  case stmt {
    ast.LetStatement(ast.LetStmt(var, idx, expr)) -> {
      let _ =
        simplifile.append(output_file, indent(level) <> "<letStatement>\n")
      let _ =
        simplifile.append(
          output_file,
          indent(level + 1) <> "<keyword> let </keyword>\n",
        )
      let _ =
        simplifile.append(
          output_file,
          indent(level + 1) <> "<identifier> " <> var <> " </identifier>\n",
        )
      let _ = case idx {
        Some(e) -> {
          let _ =
            simplifile.append(
              output_file,
              indent(level + 1) <> "<symbol> [ </symbol>\n",
            )
          let _ = expression_to_xml(e, output_file, level + 1)
          let _ =
            simplifile.append(
              output_file,
              indent(level + 1) <> "<symbol> ] </symbol>\n",
            )
        }
        _ -> Ok(Nil)
      }
      let _ =
        simplifile.append(
          output_file,
          indent(level + 1) <> "<symbol> = </symbol>\n",
        )
      let _ = expression_to_xml(expr, output_file, level + 1)
      let _ =
        simplifile.append(
          output_file,
          indent(level + 1) <> "<symbol> ; </symbol>\n",
        )
      let _ =
        simplifile.append(output_file, indent(level) <> "</letStatement>\n")
    }
    ast.DoStatement(ast.DoStmt(call)) -> {
      let _ = simplifile.append(output_file, indent(level) <> "<doStatement>\n")
      let _ =
        simplifile.append(
          output_file,
          indent(level + 1) <> "<keyword> do </keyword>\n",
        )
      let _ = subroutine_call_to_xml(call, output_file, level + 1)
      let _ =
        simplifile.append(
          output_file,
          indent(level + 1) <> "<symbol> ; </symbol>\n",
        )
      let _ =
        simplifile.append(output_file, indent(level) <> "</doStatement>\n")
    }
    ast.ReturnStatement(ast.ReturnStmt(expr_opt)) -> {
      let _ =
        simplifile.append(output_file, indent(level) <> "<returnStatement>\n")
      let _ =
        simplifile.append(
          output_file,
          indent(level + 1) <> "<keyword> return </keyword>\n",
        )
      let _ = case expr_opt {
        Some(expr) -> expression_to_xml(expr, output_file, level + 1)
        _ -> Ok(Nil)
      }
      let _ =
        simplifile.append(
          output_file,
          indent(level + 1) <> "<symbol> ; </symbol>\n",
        )
      let _ =
        simplifile.append(output_file, indent(level) <> "</returnStatement>\n")
    }
    ast.IfStatement(ast.IfStmt(cond, then_stmts, else_opt)) -> {
      let _ = simplifile.append(output_file, indent(level) <> "<ifStatement>\n")
      let _ =
        simplifile.append(
          output_file,
          indent(level + 1) <> "<keyword> if </keyword>\n",
        )
      let _ =
        simplifile.append(
          output_file,
          indent(level + 1) <> "<symbol> ( </symbol>\n",
        )
      let _ = expression_to_xml(cond, output_file, level + 1)
      let _ =
        simplifile.append(
          output_file,
          indent(level + 1) <> "<symbol> ) </symbol>\n",
        )
      let _ =
        simplifile.append(
          output_file,
          indent(level + 1) <> "<symbol> { </symbol>\n",
        )
      let _ = statements_to_xml(then_stmts, output_file, level + 1)
      let _ =
        simplifile.append(
          output_file,
          indent(level + 1) <> "<symbol> } </symbol>\n",
        )
      let _ = case else_opt {
        Some(ast.ElseStmt(else_stmts)) -> {
          let _ =
            simplifile.append(
              output_file,
              indent(level + 1) <> "<keyword> else </keyword>\n",
            )
          let _ =
            simplifile.append(
              output_file,
              indent(level + 1) <> "<symbol> { </symbol>\n",
            )
          let _ = statements_to_xml(else_stmts, output_file, level + 1)
          let _ =
            simplifile.append(
              output_file,
              indent(level + 1) <> "<symbol> } </symbol>\n",
            )
        }
        _ -> Ok(Nil)
      }
      let _ =
        simplifile.append(output_file, indent(level) <> "</ifStatement>\n")
    }
    ast.WhileStatement(ast.WhileStmt(cond, stmts)) -> {
      let _ =
        simplifile.append(output_file, indent(level) <> "<whileStatement>\n")
      let _ =
        simplifile.append(
          output_file,
          indent(level + 1) <> "<keyword> while </keyword>\n",
        )
      let _ =
        simplifile.append(
          output_file,
          indent(level + 1) <> "<symbol> ( </symbol>\n",
        )
      let _ = expression_to_xml(cond, output_file, level + 1)
      let _ =
        simplifile.append(
          output_file,
          indent(level + 1) <> "<symbol> ) </symbol>\n",
        )
      let _ =
        simplifile.append(
          output_file,
          indent(level + 1) <> "<symbol> { </symbol>\n",
        )
      let _ = statements_to_xml(stmts, output_file, level + 1)
      let _ =
        simplifile.append(
          output_file,
          indent(level + 1) <> "<symbol> } </symbol>\n",
        )
      let _ =
        simplifile.append(output_file, indent(level) <> "</whileStatement>\n")
    }
  }
}

fn expression_to_xml(expr, output_file: String, level: Int) {
  let _ = simplifile.append(output_file, indent(level) <> "<expression>\n")
  case expr {
    ast.Expr(term, ops_opt) -> {
      let _ = term_to_xml(term, output_file, level + 1)
      case ops_opt {
        Some(ops) ->
          list.each(ops, fn(p: ast.OpTerm) {
            let _ =
              simplifile.append(
                output_file,
                indent(level + 1)
                  <> "<symbol> "
                  <> op_to_str(p.op)
                  <> " </symbol>\n",
              )
            term_to_xml(p.term, output_file, level + 1)
          })
        _ -> Nil
      }
    }
  }
  let _ = simplifile.append(output_file, indent(level) <> "</expression>\n")
}

fn op_to_str(op) {
  case op {
    ast.Plus(_) -> "+"
    ast.Minus(_) -> "-"
    ast.Mult(_) -> "*"
    ast.Div(_) -> "/"
    ast.And(_) -> "&amp;"
    ast.Or(_) -> "|"
    ast.Lt(_) -> "&lt;"
    ast.Gt(_) -> "&gt;"
    ast.Eq(_) -> "="
  }
}

fn term_to_xml(term, output_file: String, level: Int) {
  let _ = simplifile.append(output_file, indent(level) <> "<term>\n")
  let _ = case term {
    ast.VarName(n) ->
      simplifile.append(
        output_file,
        indent(level + 1) <> "<identifier> " <> n <> " </identifier>\n",
      )
    ast.IntegerConstant(i) ->
      simplifile.append(
        output_file,
        indent(level + 1)
          <> "<integerConstant> "
          <> int.to_string(i)
          <> " </integerConstant>\n",
      )
    ast.StringConstant(s) ->
      simplifile.append(
        output_file,
        indent(level + 1) <> "<stringConstant> " <> s <> " </stringConstant>\n",
      )
    ast.KeywordConstant(k) ->
      simplifile.append(
        output_file,
        indent(level + 1)
          <> "<keyword> "
          <> keyword_to_xml(k)
          <> " </keyword>\n",
      )
    ast.SubroutineCall(call) ->
      subroutine_call_to_xml(call, output_file, level + 1)
    ast.Expression(e) -> {
      let _ =
        simplifile.append(
          output_file,
          indent(level + 1) <> "<symbol> ( </symbol>\n",
        )
      let _ = expression_to_xml(e, output_file, level + 1)
      let _ =
        simplifile.append(
          output_file,
          indent(level + 1) <> "<symbol> ) </symbol>\n",
        )
    }
    ast.VNameExpr(ast.VarNameExpr(n, e)) -> {
      let _ =
        simplifile.append(
          output_file,
          indent(level + 1) <> "<identifier> " <> n <> " </identifier>\n",
        )
      let _ =
        simplifile.append(
          output_file,
          indent(level + 1) <> "<symbol> [ </symbol>\n",
        )
      let _ = expression_to_xml(e, output_file, level + 1)
      let _ =
        simplifile.append(
          output_file,
          indent(level + 1) <> "<symbol> ] </symbol>\n",
        )
    }
    ast.UnaryOpTerm(ast.UOpTerm(op, t)) -> {
      let _ =
        simplifile.append(
          output_file,
          indent(level + 1)
            <> "<symbol> "
            <> unary_op_to_str(op)
            <> " </symbol>\n",
        )
      term_to_xml(t, output_file, level + 1)
    }
  }
  let _ = simplifile.append(output_file, indent(level) <> "</term>\n")
}

fn keyword_to_xml(k) {
  case k {
    ast.True -> "true"
    ast.False -> "false"
    ast.Null -> "null"
    ast.This -> "this"
  }
}

fn unary_op_to_str(op: ast.UnaryOp) {
  case op {
    ast.Neg(_) -> "~"
    ast.UMinus(_) -> "-"
  }
}

fn subroutine_call_to_xml(call, output_file: String, level: Int) {
  case call {
    ast.FSubroutineCall(ast.FlatSubroutineCall(name, exprs)) -> {
      let _ =
        simplifile.append(
          output_file,
          indent(level) <> "<identifier> " <> name <> " </identifier>\n",
        )
      let _ =
        simplifile.append(
          output_file,
          indent(level) <> "<symbol> ( </symbol>\n",
        )
      let _ = expression_list_to_xml(exprs, output_file, level)
      let _ =
        simplifile.append(
          output_file,
          indent(level) <> "<symbol> ) </symbol>\n",
        )
    }
    ast.NSubroutineCall(ast.NestedSubroutineCall(
      obj,
      ast.FlatSubroutineCall(name, exprs),
    )) -> {
      let _ =
        simplifile.append(
          output_file,
          indent(level) <> "<identifier> " <> obj <> " </identifier>\n",
        )
      let _ =
        simplifile.append(
          output_file,
          indent(level) <> "<symbol> . </symbol>\n",
        )
      let _ =
        simplifile.append(
          output_file,
          indent(level) <> "<identifier> " <> name <> " </identifier>\n",
        )
      let _ =
        simplifile.append(
          output_file,
          indent(level) <> "<symbol> ( </symbol>\n",
        )
      let _ = expression_list_to_xml(exprs, output_file, level)
      let _ =
        simplifile.append(
          output_file,
          indent(level) <> "<symbol> ) </symbol>\n",
        )
    }
  }
}

fn expression_list_to_xml(exprs, output_file: String, level: Int) {
  let _ = simplifile.append(output_file, indent(level) <> "<expressionList>\n")
  case exprs {
    [] -> Nil
    [first, ..rest] -> {
      let _ = expression_to_xml(first, output_file, level + 1)
      list.each(rest, fn(e) {
        let _ =
          simplifile.append(
            output_file,
            indent(level + 1) <> "<symbol> , </symbol>\n",
          )
        expression_to_xml(e, output_file, level + 1)
      })
    }
  }
  let _ = simplifile.append(output_file, indent(level) <> "</expressionList>\n")
}

// Main function to process the input file and generate the output
pub fn main() {
  let assert Ok(dir_name) = list.first(argv.load().arguments)
  let assert Ok(files) = simplifile.get_files(dir_name)

  let jack_files =
    list.filter(files, fn(file) { string.ends_with(file, ".jack") })

  list.each(jack_files, fn(file) {
    let assert Ok(data) = simplifile.read(file)

    use tokens <- result.try(
      new()
      |> run(data)
      |> result.map_error(fn(e) {
        NoMatchingTokenError(row: e.row, col: e.col, lexeme: e.lexeme)
      }),
    )

    let assert Ok(output_file) = list.first(string.split(file, "."))
    let _ = to_xml_t(tokens, output_file <> "TTest.xml")
    let _ = to_xml(output_file <> "TTest.xml", output_file <> "Test.xml")

    Ok("")
  })
}
