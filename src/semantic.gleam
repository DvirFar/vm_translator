import ast
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import simplifile
import symbol_table.{type SymbolTable}

pub fn build_class_symbol_table(class: ast.Class) -> SymbolTable {
  let table = symbol_table.new()
  // Add class vars
  let table =
    list.fold(class.class_var_decs, table, fn(tbl, dec) {
      let kind = case dec.kind {
        ast.StaticKind -> symbol_table.Static
        ast.FieldKind -> symbol_table.Field
      }
      list.fold(dec.var_names, tbl, fn(t, name) {
        symbol_table.define(t, name, type_to_string(dec.var_type), kind)
      })
    })
  table
}

pub fn build_subroutine_symbol_table(
  class_name: String,
  subroutine: ast.ClassSubroutineDec,
  class_table: SymbolTable,
) -> SymbolTable {
  // Start a new subroutine table (reset ARG/VAR indices)
  let table = symbol_table.start_subroutine(class_table)
  let kind = subroutine.kind
  let params = case subroutine.parameter_list {
    ast.EmptyList -> []
    ast.ParameterList(first, rest) -> [
      first,
      ..list.map(rest, fn(cp) { cp.param })
    ]
  }
  // For methods, add 'this' as the first argument
  let table = case kind {
    ast.KindMethod ->
      symbol_table.define(table, "this", class_name, symbol_table.Argument)
    _ -> table
  }
  // Add parameters (as arguments)
  let table =
    list.fold(params, table, fn(tbl, param) {
      symbol_table.define(
        tbl,
        param.param_name,
        type_to_string(param.param_type),
        symbol_table.Argument,
      )
    })
  // Add local variables (varDecs)
  let table =
    list.fold(subroutine.subroutine_body.var_decs, table, fn(tbl, dec) {
      list.fold(dec.var_names, tbl, fn(t, name) {
        symbol_table.define(
          t,
          name,
          type_to_string(dec.var_type),
          symbol_table.Var,
        )
      })
    })
  table
}

fn type_to_string(var_type: ast.VarType) -> String {
  // Convert ast.VarType to string for symbol table
  case var_type {
    ast.VarInt -> "int"
    ast.VarChar -> "char"
    ast.VarBoolean -> "boolean"
    ast.ClassName(name) -> name
  }
}

fn parameter_list_to_list(param_list: ast.ParameterList) {
  case param_list {
    ast.EmptyList -> []
    ast.ParameterList(first, rest) -> {
      list.fold(rest, [first], fn(par_list, param) { [param.param, ..par_list] })
      |> list.reverse()
    }
  }
}

fn insert_local_vars(sub_body: ast.SubroutineBody, table: SymbolTable) {
  list.fold(sub_body.var_decs, table, fn(tbl, var_dec) {
    let type_ = type_to_string(var_dec.var_type)
    list.fold(var_dec.var_names, tbl, fn(tbl, name) {
      symbol_table.define(tbl, name, type_, symbol_table.Var)
    })
  })
}

pub fn compile_class(class: ast.Class, output_file: String) {
  let _ = simplifile.delete(output_file)
  let ctx = CompileContext(0, class)
  let table = symbol_table.new()
  let table =
    list.fold(class.class_var_decs, table, fn(tbl, var_dec) {
      let type_ = type_to_string(var_dec.var_type)
      let kind = case var_dec.kind {
        ast.FieldKind -> symbol_table.Field
        ast.StaticKind -> symbol_table.Static
      }
      list.fold(var_dec.var_names, tbl, fn(tbl, name) {
        symbol_table.define(tbl, name, type_, kind)
      })
    })

  list.fold(class.class_subroutine_decs, ctx, fn(ctx_, sub_dec) {
    let table = symbol_table.start_subroutine(table)
    let tbl = case sub_dec.kind {
      ast.KindMethod ->
        symbol_table.define(table, "this", class.name, symbol_table.Argument)
      _ -> table
    }

    let table =
      list.fold(
        parameter_list_to_list(sub_dec.parameter_list),
        tbl,
        fn(tbl, param) {
          let type_ = type_to_string(param.param_type)
          symbol_table.define(
            tbl,
            param.param_name,
            type_,
            symbol_table.Argument,
          )
        },
      )

    let table = insert_local_vars(sub_dec.subroutine_body, table)
    let _ =
      simplifile.append(
        output_file,
        "function "
          <> class.name
          <> "."
          <> sub_dec.subroutine_name
          <> " "
          <> int.to_string(symbol_table.var_count(table, symbol_table.Var))
          <> "\n",
      )

    let _ = case sub_dec.kind {
      ast.KindConstructor ->
        simplifile.append(
          output_file,
          "\tpush constant "
            <> int.to_string(symbol_table.var_count(table, symbol_table.Field))
            <> "\n\tcall Memory.alloc 1\n\tpop pointer 0\n",
        )
      ast.KindMethod ->
        simplifile.append(output_file, "\tpush argument 0\n\tpop pointer 0\n")
      ast.KindFunction -> Ok(Nil)
    }

    compile_statements(
      sub_dec.subroutine_body.statements,
      table,
      output_file,
      ctx_,
    )
  })
}

pub fn compile_expression(
  exp: ast.Expression,
  table: SymbolTable,
  output_file: String,
  ctx: CompileContext,
) -> Nil {
  let _ = compile_term(exp.term, table, output_file, ctx)
  case exp.rest {
    None -> Nil
    Some(op_terms) ->
      list.each(op_terms, fn(op_term: ast.OpTerm) {
        compile_term(op_term.term, table, output_file, ctx)
        case op_term.op {
          ast.And(_) -> simplifile.append(output_file, "\tand\n")
          ast.Div(_) -> simplifile.append(output_file, "\tcall Math.divide 2\n")
          ast.Eq(_) -> simplifile.append(output_file, "\teq\n")
          ast.Gt(_) -> simplifile.append(output_file, "\tgt\n")
          ast.Lt(_) -> simplifile.append(output_file, "\tlt\n")
          ast.Minus(_) -> simplifile.append(output_file, "\tsub\n")
          ast.Mult(_) ->
            simplifile.append(output_file, "\tcall Math.multiply 2\n")
          ast.Or(_) -> simplifile.append(output_file, "\tor\n")
          ast.Plus(_) -> simplifile.append(output_file, "\tadd\n")
        }
      })
  }
}

pub fn compile_term(
  term: ast.Term,
  table: SymbolTable,
  output_file: String,
  ctx: CompileContext,
) -> Nil {
  case term {
    ast.Expression(exp) -> compile_expression(exp, table, output_file, ctx)
    ast.IntegerConstant(n) -> {
      let _ =
        simplifile.append(
          output_file,
          "\tpush constant " <> int.to_string(n) <> "\n",
        )
      Nil
    }
    ast.KeywordConstant(kw) -> {
      let _ = case kw {
        ast.False -> simplifile.append(output_file, "\tpush constant 0\n")
        ast.Null -> simplifile.append(output_file, "\tpush constant 0\n")
        ast.This -> simplifile.append(output_file, "\tpush pointer 0\n")
        ast.True -> simplifile.append(output_file, "\tpush constant 1\n\tneg\n")
      }
      Nil
    }
    ast.StringConstant(str) -> {
      let _ =
        simplifile.append(
          output_file,
          "\tpush constant "
            <> int.to_string(string.length(str))
            <> "\n\tcall String.new 1\n",
        )
      let _ =
        string.to_utf_codepoints(str)
        |> list.each(fn(ch) {
          let chint = string.utf_codepoint_to_int(ch)
          let _ =
            simplifile.append(
              output_file,
              "\tpush constant " <> int.to_string(chint) <> "\n",
            )
          let _ = simplifile.append(output_file, "\tcall String.appendChar 2\n")
        })
      Nil
    }
    ast.SubroutineCall(sub_call) -> {
      case sub_call {
        ast.FSubroutineCall(fs_call) -> {
          // Flat call: could be method, static, or constructor of current class
          // Determine if it's a method by checking symbol table for subroutine name
          // In Jack, if it's a method, we need to push pointer 0 (this)
          let is_method = is_method_in_class(ctx.class, fs_call.subroutine_name)
          let n_args = case is_method {
            True -> list.length(fs_call.expr_list) + 1
            False -> list.length(fs_call.expr_list)
          }
          let _ = case is_method {
            True -> simplifile.append(output_file, "\tpush pointer 0\n")
            False -> Ok(Nil)
          }
          let _ =
            list.each(fs_call.expr_list, fn(exp) {
              compile_expression(exp, table, output_file, ctx)
            })
          let _ =
            simplifile.append(
              output_file,
              "\tcall "
                <> ctx.class.name
                <> "."
                <> fs_call.subroutine_name
                <> " "
                <> int.to_string(n_args)
                <> "\n",
            )
          Nil
        }
        ast.NSubroutineCall(ns_call) -> {
          // Nested call: could be method or static/constructor of another class or variable
          let is_var = symbol_table.kind_of(table, ns_call.name) != None
          let n_args = case is_var {
            True -> list.length(ns_call.call.expr_list) + 1
            False -> list.length(ns_call.call.expr_list)
          }
          let _ = case is_var {
            True ->
              simplifile.append(
                output_file,
                "\tpush " <> name_to_vm(ns_call.name, table) <> "\n",
              )
            False -> Ok(Nil)
          }
          let _ =
            list.each(ns_call.call.expr_list, fn(exp) {
              compile_expression(exp, table, output_file, ctx)
            })
          let class_or_var = case is_var {
            True ->
              symbol_table.type_of(table, ns_call.name)
              |> option.unwrap(ns_call.name)
            False -> ns_call.name
          }
          let _ =
            simplifile.append(
              output_file,
              "\tcall "
                <> class_or_var
                <> "."
                <> ns_call.call.subroutine_name
                <> " "
                <> int.to_string(n_args)
                <> "\n",
            )
          Nil
        }
      }
    }
    ast.UnaryOpTerm(uop_term) -> {
      let _ = compile_term(uop_term.term, table, output_file, ctx)
      let _ = case uop_term.op {
        ast.Neg(_) -> simplifile.append(output_file, "\tnot\n")
        ast.UMinus(_) -> simplifile.append(output_file, "\tneg\n")
      }
      Nil
    }
    ast.VNameExpr(ast.VarNameExpr(name, expr)) -> {
      let _ =
        simplifile.append(
          output_file,
          "\tpush " <> name_to_vm(name, table) <> "\n",
        )
      let _ = compile_expression(expr, table, output_file, ctx)
      let _ =
        simplifile.append(
          output_file,
          "\tadd\n\tpop pointer 1\n\tpush that 0\n",
        )
      Nil
    }
    ast.VarName(name) -> {
      let _ =
        simplifile.append(
          output_file,
          "\tpush " <> name_to_vm(name, table) <> "\n",
        )
      Nil
    }
  }
}

fn is_method_in_class(class_ast: ast.Class, subroutine_name: String) -> Bool {
  list.any(class_ast.class_subroutine_decs, fn(sub) {
    sub.subroutine_name == subroutine_name && sub.kind == ast.KindMethod
  })
}

pub fn compile_let_stmt(
  stmt: ast.LetStatement,
  table: SymbolTable,
  output_file: String,
  ctx: CompileContext,
) {
  let _ = case stmt.opt_expr {
    None -> {
      let _ = compile_expression(stmt.eq_expr, table, output_file, ctx)
      let _ =
        simplifile.append(
          output_file,
          "\tpop " <> name_to_vm(stmt.var_name, table) <> "\n",
        )
      Nil
    }
    Some(expr) -> {
      let _ =
        simplifile.append(
          output_file,
          "\tpush " <> name_to_vm(stmt.var_name, table) <> "\n",
        )
      let _ = compile_expression(expr, table, output_file, ctx)
      let _ = simplifile.append(output_file, "\tadd\n")
      let _ = compile_expression(stmt.eq_expr, table, output_file, ctx)
      let _ =
        simplifile.append(
          output_file,
          "\tpop temp 0\n"
            <> "\tpop pointer 1\n"
            <> "\tpush temp 0\n"
            <> "\tpop that 0\n",
        )
      Nil
    }
  }
  ctx
}

pub fn compile_return_stmt(
  stmt: ast.ReturnStatement,
  table: SymbolTable,
  output_file: String,
  ctx: CompileContext,
) {
  let _ = case stmt.expr {
    Some(exp) -> compile_expression(exp, table, output_file, ctx)
    None -> {
      let _ = simplifile.append(output_file, "\tpush constant 0\n")
      Nil
    }
  }
  let _ = simplifile.append(output_file, "\treturn\n")
  ctx
}

pub fn compile_do_stmt(
  stmt: ast.DoStatement,
  table: SymbolTable,
  output_file: String,
  ctx: CompileContext,
) {
  let _ =
    compile_expression(
      ast.Expr(ast.SubroutineCall(stmt.subroutine_call), None),
      table,
      output_file,
      ctx,
    )
  let _ = simplifile.append(output_file, "\tpop temp 0\n")
  ctx
}

pub type CompileContext {
  CompileContext(label_counter: Int, class: ast.Class)
}

fn fresh_label(ctx: CompileContext, base: String) -> #(String, CompileContext) {
  let label = ctx.class.name <> "_" <> base <> int.to_string(ctx.label_counter)
  #(
    label,
    CompileContext(label_counter: ctx.label_counter + 1, class: ctx.class),
  )
}

pub fn compile_if_stmt(
  stmt: ast.IfStatement,
  table: SymbolTable,
  output_file: String,
  ctx: CompileContext,
) -> CompileContext {
  let #(label_true, ctx) = fresh_label(ctx, "IF_TRUE_")
  let #(label_false, ctx) = fresh_label(ctx, "IF_FALSE_")
  let #(label_end, ctx) = fresh_label(ctx, "IF_END_")
  let _ = compile_expression(stmt.expr, table, output_file, ctx)
  let _ = simplifile.append(output_file, "\tif-goto " <> label_true <> "\n")
  // else branch (if present)
  let _ = simplifile.append(output_file, "\tgoto " <> label_false <> "\n")
  let _ = simplifile.append(output_file, "label " <> label_true <> "\n")
  let ctx = compile_statements(stmt.statements, table, output_file, ctx)
  let _ = simplifile.append(output_file, "\tgoto " <> label_end <> "\n")
  let _ = simplifile.append(output_file, "label " <> label_false <> "\n")
  let ctx = case stmt.else_statement {
    Some(else_stmt) ->
      compile_statements(else_stmt.statements, table, output_file, ctx)
    None -> ctx
  }
  let _ = simplifile.append(output_file, "label " <> label_end <> "\n")
  ctx
}

pub fn compile_while_stmt(
  stmt: ast.WhileStatement,
  table: SymbolTable,
  output_file: String,
  ctx: CompileContext,
) -> CompileContext {
  let #(label_exp, ctx) = fresh_label(ctx, "WHILE_EXP_")
  let #(label_end, ctx) = fresh_label(ctx, "WHILE_END_")
  let _ = simplifile.append(output_file, "label " <> label_exp <> "\n")
  let _ = compile_expression(stmt.expr, table, output_file, ctx)
  let _ = simplifile.append(output_file, "\tnot\n")
  let _ = simplifile.append(output_file, "\tif-goto " <> label_end <> "\n")
  let ctx = compile_statements(stmt.statements, table, output_file, ctx)
  let _ = simplifile.append(output_file, "\tgoto " <> label_exp <> "\n")
  let _ = simplifile.append(output_file, "label " <> label_end <> "\n")
  ctx
}

// Helper to compile a list of statements with context
pub fn compile_statements(
  stmts: ast.Statements,
  table: SymbolTable,
  output_file: String,
  ctx: CompileContext,
) -> CompileContext {
  list.fold(stmts.statements, ctx, fn(ctx, stmt) {
    case stmt {
      ast.LetStatement(s) -> compile_let_stmt(s, table, output_file, ctx)
      ast.IfStatement(s) -> compile_if_stmt(s, table, output_file, ctx)
      ast.WhileStatement(s) -> compile_while_stmt(s, table, output_file, ctx)
      ast.DoStatement(s) -> compile_do_stmt(s, table, output_file, ctx)
      ast.ReturnStatement(s) -> compile_return_stmt(s, table, output_file, ctx)
    }
  })
}

fn name_to_vm(name: String, table: SymbolTable) {
  let kind = symbol_table.kind_of(table, name)
  let idx = symbol_table.index_of(table, name)
  case kind, idx {
    Some(symbol_table.Field), Some(num) -> "this " <> int.to_string(num)
    Some(symbol_table.Static), Some(num) -> "static " <> int.to_string(num)
    Some(symbol_table.Argument), Some(num) -> "argument " <> int.to_string(num)
    Some(symbol_table.Var), Some(num) -> "local " <> int.to_string(num)
    Some(symbol_table.Field), None -> "this 0"
    Some(symbol_table.Static), None -> "static 0"
    Some(symbol_table.Argument), None -> "argument 0"
    Some(symbol_table.Var), None -> "local 0"
    None, _ -> panic as "Invalid value in symbol table!"
  }
}
