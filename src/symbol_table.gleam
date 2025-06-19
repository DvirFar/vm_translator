import gleam/list
import gleam/option.{type Option}

pub type Kind {
  Static
  Field
  Argument
  Var
}

pub type SymbolInfo {
  SymbolInfo(name: String, type_: String, kind: Kind, index: Int)
}

pub type TableEntry {
  TableEntry(name: String, info: SymbolInfo)
}

pub type IndexEntry {
  IndexEntry(kind: Kind, index: Int)
}

pub type SymbolTable {
  SymbolTable(
    class_table: List(#(String, SymbolInfo)),
    subroutine_table: List(#(String, SymbolInfo)),
    indices: List(#(Kind, Int)),
  )
}

pub fn new() -> SymbolTable {
  SymbolTable(class_table: [], subroutine_table: [], indices: [
    #(Static, 0),
    #(Field, 0),
    #(Argument, 0),
    #(Var, 0),
  ])
}

fn merge(list1, list2) {
  case list2 {
    [#(name, info), ..rest] -> merge(list.key_set(list1, name, info), rest)
    [] -> list1
  }
}

pub fn start_subroutine(table: SymbolTable) -> SymbolTable {
  SymbolTable(
    class_table: table.class_table,
    subroutine_table: [],
    indices: merge(table.indices, [#(Argument, 0), #(Var, 0)]),
  )
}

pub fn define(
  table: SymbolTable,
  name: String,
  type_: String,
  kind: Kind,
) -> SymbolTable {
  let idx = case list.key_find(table.indices, kind) {
    Ok(n) -> n
    Error(Nil) -> 0
  }
  let info = SymbolInfo(name, type_, kind, idx)
  let indices = list.key_set(table.indices, kind, idx + 1)
  case kind {
    Static | Field ->
      SymbolTable(
        class_table: list.key_set(table.class_table, name, info),
        subroutine_table: table.subroutine_table,
        indices: indices,
      )
    Argument | Var ->
      SymbolTable(
        class_table: table.class_table,
        subroutine_table: list.key_set(table.subroutine_table, name, info),
        indices: indices,
      )
  }
}

pub fn var_count(table: SymbolTable, kind: Kind) -> Int {
  case list.key_find(table.indices, kind) {
    Ok(n) -> n
    Error(Nil) -> 0
  }
}

pub fn kind_of(table: SymbolTable, name: String) -> Option(Kind) {
  list.key_find(table.subroutine_table, name)
  |> option.from_result
  |> option.or(list.key_find(table.class_table, name) |> option.from_result)
  |> option.map(fn(info) { info.kind })
}

pub fn type_of(table: SymbolTable, name: String) -> Option(String) {
  list.key_find(table.subroutine_table, name)
  |> option.from_result
  |> option.or(list.key_find(table.class_table, name) |> option.from_result)
  |> option.map(fn(info) { info.type_ })
}

pub fn index_of(table: SymbolTable, name: String) -> Option(Int) {
  list.key_find(table.subroutine_table, name)
  |> option.from_result
  |> option.or(list.key_find(table.class_table, name) |> option.from_result)
  |> option.map(fn(info) { info.index })
}
