// author: Dvir Farkash 329398911 150060.3.5785.42

import argv
import gleam/int
import gleam/list
import gleam/result
import gleam/set
import gleam/string
import nibble/lexer
import simplifile

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
  And
  Or
  Lt
  Gt
  Eq
  Tilde
}

pub type Token {
  Keyword(Keyword)
  Identifier(String)
  IntegerConstant(Int)
  Symbol(Symbol)
  StringConstant(String)
  Comment(String)
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
      lexer.keyword("class", "\\s+", Keyword(ClassKeyword)),
      lexer.keyword("constructor", "\\s+", Keyword(ConstructorKeyword)),
      lexer.keyword("function", "\\s+", Keyword(FunctionKeyword)),
      lexer.keyword("method", "\\s+", Keyword(MethodKeyword)),
      lexer.keyword("field", "\\s+", Keyword(FieldKeyword)),
      lexer.keyword("static", "\\s+", Keyword(StaticKeyword)),
      lexer.keyword("var", "\\s+", Keyword(VarKeyword)),
      lexer.keyword("int", "\\s+", Keyword(IntKeyword)),
      lexer.keyword("char", "\\s+", Keyword(CharKeyword)),
      lexer.keyword("boolean", "\\s+", Keyword(BooleanKeyword)),
      lexer.keyword("void", "\\s+", Keyword(VoidKeyword)),
      lexer.keyword("null", "\\s+", Keyword(NullKeyword)),
      lexer.keyword("this", "\\s+", Keyword(ThisKeyword)),
      lexer.keyword("true", "\\s+", Keyword(TrueKeyword)),
      lexer.keyword("false", "\\s+", Keyword(FalseKeyword)),
      lexer.keyword("if", "\\s+", Keyword(IfKeyword)),
      lexer.keyword("else", "\\s+", Keyword(ElseKeyword)),
      lexer.keyword("let", "\\s+", Keyword(LetKeyword)),
      lexer.keyword("do", "\\s+", Keyword(DoKeyword)),
      lexer.keyword("while", "\\s+", Keyword(WhileKeyword)),
      lexer.keyword("return", "\\s+", Keyword(ReturnKeyword)),
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
    _, _, "*/" -> lexer.Skip
    False, _, _ -> lexer.NoMatch
  }
}

pub fn to_xml_t(tokens: List(lexer.Token(Token)), output_file: String) {
  let _ = simplifile.delete(output_file)
  let _ = simplifile.append(output_file, "<tokens>\n")

  list.each(tokens, fn(token) {
    case token.value {
      Keyword(_) ->
        simplifile.append(
          output_file,
          "\t<keyword> " <> token.lexeme <> " </keyword>\n",
        )
      Identifier(id) ->
        simplifile.append(
          output_file,
          "\t<identifier> " <> id <> " </identifier>\n",
        )
      IntegerConstant(num) ->
        simplifile.append(
          output_file,
          "\t<integerConstant> "
            <> int.to_string(num)
            <> " </integerConstant>\n",
        )
      Symbol(sym) -> {
        let _ = simplifile.append(output_file, "\t<symbol> ")
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
          "\t<stringConstant> " <> str <> " </stringConstant>\n",
        )
      _ -> Ok(Nil)
    }
  })

  simplifile.append(output_file, "</tokens>")
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
    Ok(to_xml_t(tokens, output_file <> "T.xml"))
  })
}
