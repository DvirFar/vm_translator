// author: Dvir Farkash 329398911 150060.3.5785.42

import argv
import gleam/dict
import gleam/int
import gleam/list
import gleam/string
import simplifile

// Define a type to hold information about the current state of processing
pub type Info {
  Info(labels: Int, output_file: String, class_name: String)
}

// Define a type for computation instructions
pub type Comp {
  Add
  Sub
  Neg
  And
  Or
  Not
}

// Define a type for jump instructions
pub type Jump {
  Eq
  Gt
  Lt
}

pub type GotoI {
  Goto(label: String)
  IfGoto(label: String)
}

pub type Func {
  Function(name: String, args_num: Int)
  Return
  Call(name: String, args_num: Int)
}

// Define a type for all possible instructions
pub type Instruction {
  Comp(Comp)
  Jump(Jump)
  Push(segment: String, index: Int)
  Pop(segment: String, index: Int)
  Label(name: String)
  GotoI(GotoI)
  Func(Func)
  None
}

// Parse a string into an integer
pub fn parse_int(str: String) {
  let assert Ok(result) = int.parse(str)
  result
}

// Process a single line of input and convert it into an Instruction
pub fn process_line(line) -> Instruction {
  echo line
  case string.split(string.trim(line), " ") {
    [inst] if inst == "add" -> Comp(Add)
    [inst] if inst == "sub" -> Comp(Sub)
    [inst] if inst == "neg" -> Comp(Neg)
    [inst] if inst == "and" -> Comp(And)
    [inst] if inst == "or" -> Comp(Or)
    [inst] if inst == "not" -> Comp(Not)
    [inst] if inst == "eq" -> Jump(Eq)
    [inst] if inst == "gt" -> Jump(Gt)
    [inst] if inst == "lt" -> Jump(Lt)
    ["push", segment, index] -> Push(segment:, index: parse_int(index))
    ["pop", segment, index] -> Pop(segment:, index: parse_int(index))
    ["label", c] -> Label(name: c)
    ["goto", c] -> GotoI(Goto(label: c))
    ["if-goto", c] -> GotoI(IfGoto(label: c))
    ["call", g, n] -> Func(Call(name: g, args_num: parse_int(n)))
    [inst] if inst == "return" -> Func(Return)
    ["function", g, n] -> Func(Function(name: g, args_num: parse_int(n)))
    [""] -> None
    _ -> panic as "bad instruction"
  }
}

// Process an instruction and update the Info state
pub fn process_instruction(info: Info, instruction: Instruction) -> Info {
  case instruction {
    Comp(inst) -> handle_comp(info, inst)
    Jump(inst) -> handle_jump(info, inst)
    Push(segment, index) -> handle_push(info, segment, index)
    Pop(segment, index) -> handle_pop(info, segment, index)
    Label(inst) -> handle_label(info, inst)
    GotoI(inst) -> handle_gotoi(info, inst)
    Func(inst) -> handle_func(info, inst)
    None -> info
  }
}

// Constants for common assembly code snippets
pub const append_comp_un_start = "@SP\nA=M-1\n"

pub const append_comp_start = append_comp_un_start <> "D=M\nA=A-1\n"

pub const append_comp_end = "@SP\nM=M-1\n"

// Handle computation instructions
pub fn handle_comp(info: Info, a: Comp) -> Info {
  let _ = case a {
    Add ->
      simplifile.append(
        info.output_file,
        append_comp_start <> "M=D+M\n" <> append_comp_end,
      )
    Sub ->
      simplifile.append(
        info.output_file,
        append_comp_start <> "M=M-D\n" <> append_comp_end,
      )
    Neg -> simplifile.append(info.output_file, append_comp_un_start <> "M=-M\n")
    And ->
      simplifile.append(
        info.output_file,
        append_comp_start <> "M=D&M\n" <> append_comp_end,
      )
    Or ->
      simplifile.append(
        info.output_file,
        append_comp_start <> "M=D|M\n" <> append_comp_end,
      )
    Not -> simplifile.append(info.output_file, append_comp_un_start <> "M=!M\n")
  }
  info
}

// Constants for jump instructions
pub const append_jump_start = "@SP\nA=M-1\nD=M\nA=A-1\nD=M-D\n"

pub const append_jump_mid1 = "D=0\n@SP\nA=M-1\nA=A-1\nM=D\n"

pub const append_jump_mid2 = "0;JMP\n"

pub const append_jump_mid3 = "D=-1\n@SP\nA=M-1\nA=A-1\nM=D\n"

pub const append_jump_end = "@SP\nM=M-1\n"

// Handle jump instructions
pub fn handle_jump(info: Info, a: Jump) {
  let _ = case a {
    Eq ->
      simplifile.append(
        info.output_file,
        append_jump_start
          <> "@TRUE"
          <> int.to_string(info.labels)
          <> "\nD;JEQ\n"
          <> append_jump_mid1
          <> "@FALSE"
          <> int.to_string(info.labels)
          <> "\n"
          <> append_jump_mid2
          <> "(TRUE"
          <> int.to_string(info.labels)
          <> ")\n"
          <> append_jump_mid3
          <> "(FALSE"
          <> int.to_string(info.labels)
          <> ")\n"
          <> append_jump_end,
      )
    Gt ->
      simplifile.append(
        info.output_file,
        append_jump_start
          <> "@TRUE"
          <> int.to_string(info.labels)
          <> "\nD;JGT\n"
          <> append_jump_mid1
          <> "@FALSE"
          <> int.to_string(info.labels)
          <> "\n"
          <> append_jump_mid2
          <> "(TRUE"
          <> int.to_string(info.labels)
          <> ")\n"
          <> append_jump_mid3
          <> "(FALSE"
          <> int.to_string(info.labels)
          <> ")\n"
          <> append_jump_end,
      )
    Lt ->
      simplifile.append(
        info.output_file,
        append_jump_start
          <> "@TRUE"
          <> int.to_string(info.labels)
          <> "\nD;JLT\n"
          <> append_jump_mid1
          <> "@FALSE"
          <> int.to_string(info.labels)
          <> "\n"
          <> append_jump_mid2
          <> "(TRUE"
          <> int.to_string(info.labels)
          <> ")\n"
          <> append_jump_mid3
          <> "(FALSE"
          <> int.to_string(info.labels)
          <> ")\n"
          <> append_jump_end,
      )
  }
  Info(..info, labels: info.labels + 1)
}

// Handle push instructions
pub fn handle_push(info: Info, segment: String, index: Int) {
  let _ = case segment {
    seg
      if seg == "local" || seg == "argument" || seg == "this" || seg == "that"
    -> handle_push1(info, seg, index)
    seg if seg == "temp" -> handle_push2(info, index)
    seg if seg == "static" -> handle_push3(info, index)
    seg if seg == "pointer" -> handle_push4(info, index)
    seg if seg == "constant" -> handle_push5(info, index)
    _ -> panic as "Error reading push segment"
  }
  info
}

// Constants for push instructions
pub const append_push_end = "\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"

// Handle push for segments like local, argument, this, and that
pub fn handle_push1(info: Info, segment: String, index: Int) {
  let seg_dict =
    dict.from_list([
      #("local", "LCL"),
      #("argument", "ARG"),
      #("this", "THIS"),
      #("that", "THAT"),
    ])
  let assert Ok(seg_string) = dict.get(seg_dict, segment)
  let _ =
    simplifile.append(
      info.output_file,
      "@"
        <> int.to_string(index)
        <> "\nD=A\n"
        <> "@"
        <> seg_string
        <> "\nA=D+M\nD=M"
        <> append_push_end,
    )
}

// Handle push for the temp segment
pub fn handle_push2(info: Info, index: Int) {
  let _ =
    simplifile.append(
      info.output_file,
      "@" <> int.to_string(5 + index) <> "\nD=M" <> append_push_end,
    )
}

// Handle push for the static segment
pub fn handle_push3(info: Info, index: Int) {
  let _ =
    simplifile.append(
      info.output_file,
      "@"
        <> info.class_name
        <> "."
        <> int.to_string(index)
        <> "\nD=M"
        <> append_push_end,
    )
}

// Handle push for the pointer segment
pub fn handle_push4(info: Info, index: Int) {
  let _ = case index {
    0 -> simplifile.append(info.output_file, "@THIS\nD=M" <> append_push_end)
    1 -> simplifile.append(info.output_file, "@THAT\nD=M" <> append_push_end)
    _ -> panic as "Error in push pointer index"
  }
}

// Handle push for the constant segment
pub fn handle_push5(info: Info, index: Int) {
  let _ =
    simplifile.append(
      info.output_file,
      "@" <> int.to_string(index) <> "\nD=A" <> append_push_end,
    )
}

// Handle pop instructions
pub fn handle_pop(info: Info, segment: String, index: Int) {
  let _ = case segment {
    seg
      if seg == "local" || seg == "argument" || seg == "this" || seg == "that"
    -> handle_pop1(info, seg, index)
    seg if seg == "temp" -> handle_pop2(info, index)
    seg if seg == "static" -> handle_pop3(info, index)
    seg if seg == "pointer" -> handle_pop4(info, index)
    _ -> panic as "Error reading pop segment"
  }
  info
}

// Constants for pop instructions
pub const append_pop_start = "@SP\nA=M-1\nD=M\n"

pub const append_pop_end = "\nM=D\n@SP\nM=M-1\n"

// Handle pop for segments like local, argument, this, and that
pub fn handle_pop1(info: Info, segment: String, index: Int) {
  let seg_dict =
    dict.from_list([
      #("local", "LCL"),
      #("argument", "ARG"),
      #("this", "THIS"),
      #("that", "THAT"),
    ])
  let assert Ok(seg_string) = dict.get(seg_dict, segment)
  let _ =
    simplifile.append(
      info.output_file,
      append_pop_start
        <> "@"
        <> seg_string
        <> "\nA=M"
        <> string.repeat("\nA=A+1", index)
        <> append_pop_end,
    )
}

// Handle pop for the temp segment
pub fn handle_pop2(info: Info, index: Int) {
  let _ =
    simplifile.append(
      info.output_file,
      append_pop_start <> "@" <> int.to_string(5 + index) <> append_pop_end,
    )
}

// Handle pop for the static segment
pub fn handle_pop3(info: Info, index: Int) {
  let _ =
    simplifile.append(
      info.output_file,
      append_pop_start
        <> "@"
        <> info.class_name
        <> "."
        <> int.to_string(index)
        <> append_pop_end,
    )
}

// Handle pop for the pointer segment
pub fn handle_pop4(info: Info, index: Int) {
  let _ = case index {
    0 ->
      simplifile.append(
        info.output_file,
        append_pop_start <> "@THIS" <> append_pop_end,
      )
    1 ->
      simplifile.append(
        info.output_file,
        append_pop_start <> "@THAT" <> append_pop_end,
      )
    _ -> panic as "Error in pop pointer index"
  }
}

pub fn handle_label(info: Info, name: String) {
  let _ =
    simplifile.append(
      info.output_file,
      "(" <> info.class_name <> "." <> name <> ")\n",
    )
  info
}

pub fn handle_gotoi(info: Info, inst: GotoI) {
  let _ = case inst {
    Goto(label) -> handle_goto(info, label)
    IfGoto(label) -> handle_ifgoto(info, label)
  }
  info
}

pub fn handle_goto(info: Info, label: String) {
  let _ =
    simplifile.append(
      info.output_file,
      "@" <> info.class_name <> "." <> label <> "\n0;JMP\n",
    )
  info
}

pub fn handle_ifgoto(info: Info, label: String) {
  let _ =
    simplifile.append(
      info.output_file,
      "@SP\nM=M-1\nA=M\nD=M\n@"
        <> info.class_name
        <> "."
        <> label
        <> "\nD;JNE\n",
    )
  info
}

pub fn handle_func(info: Info, inst: Func) {
  case inst {
    Function(name, args_num) -> handle_function(info, name, args_num)
    Return -> handle_return(info)
    Call(name, args_num) -> handle_call(info, name, args_num)
  }
}

pub fn handle_function(info: Info, name: String, args_num: Int) {
  let func_label = name <> "."
  let _ =
    simplifile.append(
      info.output_file,
      "("
        <> name
        <> ")\n"
        <> "@"
        <> int.to_string(args_num)
        <> "\nD=A\n@"
        <> func_label
        <> "InitEnd\nD;JEQ\n("
        <> func_label
        <> "InitLoop)\n@SP\nA=M\nM=0\n@SP\nM=M+1\n@"
        <> func_label
        <> "InitLoop\nD=D-1;JNE\n("
        <> func_label
        <> "InitEnd)\n",
    )
  info
}

pub fn handle_return(info: Info) {
  // save return address in head of stack
  let _ =
    simplifile.append(info.output_file, "@LCL\nD=M\n@5\nA=D-A\nD=M\n@13\nM=D\n")

  // save return value in memory - RAM[13]
  let _ =
    simplifile.append(
      info.output_file,
      "@SP\nM=M-1\nA=M\nD=M\n@ARG\nA=M\nM=D\n",
    )

  // restore SP
  let _ = simplifile.append(info.output_file, "@ARG\nD=M\n@SP\nM=D+1\n")

  // restore pointers
  pop_vals(info, "THAT")
  pop_vals(info, "THIS")
  pop_vals(info, "ARG")
  pop_vals(info, "LCL")

  // goto return address
  let _ = simplifile.append(info.output_file, "@13\nA=M\n0;JMP\n")

  info
}

fn pop_vals(info: Info, val: String) {
  let _ =
    simplifile.append(
      info.output_file,
      "@LCL\nM=M-1\nA=M\nD=M\n@" <> val <> "\nM=D\n",
    )
  info
}

pub const append_push_vals_end = "@SP\nA=M\nM=D\n@SP\nM=M+1\n"

pub fn handle_call(info: Info, name: String, args_num: Int) {
  // push return address
  let _ =
    simplifile.append(
      info.output_file,
      "@"
        <> info.class_name
        <> "."
        <> int.to_string(info.labels)
        <> "ReturnAddress\nD=A\n"
        <> append_push_vals_end,
    )

  // push pointers
  let _ = push_vals(info, "LCL")
  let _ = push_vals(info, "ARG")
  let _ = push_vals(info, "THIS")
  let _ = push_vals(info, "THAT")

  // calc new ARG
  let _ =
    simplifile.append(
      info.output_file,
      "@SP\nD=M\n@" <> int.to_string(args_num + 5) <> "\nD=D-A\n@ARG\nM=D\n",
    )

  // calc new LCL
  let _ = simplifile.append(info.output_file, "@SP\nD=M\n@LCL\nM=D\n")

  // give control to func
  let _ = simplifile.append(info.output_file, "@" <> name <> "\n0;JMP\n")

  // define return address
  let _ = handle_label(info, int.to_string(info.labels) <> "ReturnAddress")

  echo info.labels
  Info(..info, labels: info.labels + 1)
}

fn push_vals(info: Info, val: String) {
  let _ =
    simplifile.append(
      info.output_file,
      "@" <> val <> "\nD=M\n" <> append_push_vals_end,
    )
  info
}

// Main function to process the input file and generate the output
pub fn main() {
  let assert Ok(dir_name) = list.first(argv.load().arguments)
  let assert Ok(files) = simplifile.get_files(dir_name)
  echo files

  let vm_files = list.filter(files, fn(file) { string.ends_with(file, ".vm") })
  let sorted_vm_files = case
    list.find(vm_files, fn(file) { string.ends_with(file, "Sys.vm") })
  {
    Ok(sys_file) -> [
      sys_file,
      ..list.filter(vm_files, fn(file) { file != sys_file })
    ]
    Error(Nil) -> vm_files
  }

  let output_file = dir_name <> "/output.asm"
  let assert Ok(class_name) = list.last(string.split(dir_name, "\\"))
  // Ensure the output file is empty
  let _ = simplifile.delete(output_file)
  let initial_info = Info(0, output_file, class_name)

  // Add sys.init() call if there is more than one file
  let info_with_init = case list.length(sorted_vm_files) > 1 {
    True -> {
      let _ = simplifile.append(output_file, "@256\nD=A\n@SP\nM=D\n")
      let _ = handle_call(initial_info, "Sys.init", 0)
      Info(..initial_info, labels: initial_info.labels + 1)
    }
    False -> {
      initial_info
    }
  }

  list.fold(sorted_vm_files, info_with_init, fn(info, file) {
    let assert Ok(temp_file) = list.first(string.split(file, "."))
    let assert Ok(class_name) = list.last(string.split(temp_file, "/"))
    let assert Ok(data) = simplifile.read(file)
    string.split(data, "\n")
    |> list.map(process_line)
    |> list.fold(Info(..info, class_name: class_name), process_instruction)
  })
}
