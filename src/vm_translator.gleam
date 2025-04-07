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

// Define a type for all possible instructions
pub type Instruction {
  Comp(Comp)
  Jump(Jump)
  Push(segment: String, index: Int)
  Pop(segment: String, index: Int)
  None
}

// Parse a string into an integer
pub fn parse_int(str: String) {
  let assert Ok(result) = int.parse(str)
  result
}

// Process a single line of input and convert it into an Instruction
pub fn process_line(line) -> Instruction {
  case string.split(string.drop_end(line, 1), " ") {
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

// Main function to process the input file and generate the output
pub fn main() {
  let assert Ok(file_name) = list.first(argv.load().arguments)
  let assert Ok(temp_file) = list.first(string.split(file_name, "."))
  let assert Ok(class_name) = list.last(string.split(temp_file, "\\"))
  let output_file = temp_file <> ".asm"
  // Ensure the output file is empty
  let _ = simplifile.delete(output_file)
  let assert Ok(data) = simplifile.read(file_name)
  string.split(data, "\n")
  |> list.map(process_line)
  |> list.fold(Info(0, output_file, class_name), process_instruction)
}
/// add = ["@sp", "A=M-1", "D=M", "A=A-1", "M=D+M", "@sp", "M=M-1"]
/// sub = ["@sp", "A=M-1", "D=M", "A=A-1", "M=M-D", "@sp", "M=M-1"]
/// neg = ["@sp", "A=M-1", "M=-M"]
/// and = ["@sp", "A=M-1", "D=M", "A=A-1", "M=D&M", "@sp", "M=M-1"]
/// or = ["@sp", "A=M-1", "D=M", "A=A-1", "M=D|M", "@sp", "M=M-1"]
/// not = ["@sp", "A=M-1", "M=!M"]
/// eq = ["@sp", "A=M-1", "D=M", "A=A-1", "D=M-D", "@TRUE0", "D;JEQ", "D=0", "@sp", "A=M-1", "A=A-1", "M=D", 
///                   "@FALSE0", "0;JMP", "(TRUE0)", "D=-1", "@sp", "A=M-1", "A=A-1", "M=D", "(FALSE0)", "@sp", "M=M-1"]
/// gt = ["@sp", "A=M-1", "D=M", "A=A-1", "D=M-D", "@TRUE0", "D;JGT", "D=0", "@sp", "A=M-1", "A=A-1", "M=D", 
///                   "@FALSE0", "0;JMP", "(TRUE0)", "D=-1", "@sp", "A=M-1", "A=A-1", "M=D", "(FALSE0)", "@sp", "M=M-1"]
/// lt = ["@sp", "A=M-1", "D=M", "A=A-1", "D=M-D", "@TRUE0", "D;JLT", "D=0", "@sp", "A=M-1", "A=A-1", "M=D", 
///                   "@FALSE0", "0;JMP", "(TRUE0)", "D=-1", "@sp", "A=M-1", "A=A-1", "M=D", "(FALSE0)", "@sp", "M=M-1"]
/// 
/// push constant x = ["@x", "D=A", "@sp", "A=M", "M=D", "@sp", "M=M+1"]
/// 
/// push local x = ["@x", "D=A", "@LCL", "A=D+M", "D=M", "@sp", "A=M", "M=D", "@sp", "M=M+1"]
/// pop local x = ["@sp", "A=M-1", "D=M", "@LCL", "A=M", "A=A+1"*x, "M=D", "@sp", "M=M-1"]
/// push argument x = ["@x", "D=A", "@ARG", "A=D+M", "D=M", "@sp", "A=M", "M=D", "@sp", "M=M+1"]
/// pop argument x = ["@sp", "A=M-1", "D=M", "@ARG", "A=M", "A=A+1"*x, "M=D", "@sp", "M=M-1"]
/// push this x = ["@x", "D=A", "@THIS", "A=D+M", "D=M", "@sp", "A=M", "M=D", "@sp", "M=M+1"]
/// pop this x = ["@sp", "A=M-1", "D=M", "@THIS", "A=M", "A=A+1"*x, "M=D", "@sp", "M=M-1"]
/// push that x = ["@x", "D=A", "@THAT", "A=D+M", "D=M", "@sp", "A=M", "M=D", "@sp", "M=M+1"]
/// pop that x = ["@sp", "A=M-1", "D=M", "@THAT", "A=M", "A=A+1"*x, "M=D", "@sp", "M=M-1"]
/// 
/// push temp x = ["@(5+x)", "D=M", "@sp", "A=M", "M=D", "@sp", "M=M+1"]
/// pop temp x = ["@sp", "A=M-1", "D=M", "@(5+x)", "M=D", "@sp", "M=M-1"]
/// 
/// push pointer 0 = ["@THIS", "D=M", "@sp", "A=M", "M=D", "@sp", "M=M+1"]
/// pop pointer 0 = ["@sp", "A=M-1", "D=M", "@THIS", "M=D", "@sp", "M=M-1"]
/// push pointer 1 = ["@THAT", "D=M", "@sp", "A=M", "M=D", "@sp", "M=M+1"]
/// pop pointer 1 = ["@sp", "A=M-1", "D=M", "@THAT", "M=D", "@sp", "M=M-1"]
/// 
/// push static x = ["@(name).x", "D=M", "@sp", "A=M", "M=D", "@sp", "M=M+1"]
/// pop static x = ["@sp", "A=M-1", "D=M", "@(name).x", "M=D", "@sp", "M=M-1"]
