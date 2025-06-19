import argv
import gleam/list
import gleam/result
import gleam/string
import jack_tokenizer
import semantic
import simplifile

// Main function to process the input file and generate the output
pub fn main() {
  let assert Ok(dir_name) = list.first(argv.load().arguments)
  let assert Ok(files) = simplifile.get_files(dir_name)

  let jack_files =
    list.filter(files, fn(file) { string.ends_with(file, ".jack") })

  list.each(jack_files, fn(file) {
    let assert Ok(data) = simplifile.read(file)

    use tokens <- result.try(
      jack_tokenizer.new()
      |> jack_tokenizer.run(data)
      |> result.map_error(fn(e) {
        jack_tokenizer.NoMatchingTokenError(
          row: e.row,
          col: e.col,
          lexeme: e.lexeme,
        )
      }),
    )

    let assert Ok(output_file) = list.first(string.split(file, "."))
    let _ = jack_tokenizer.to_xml_t(tokens, output_file <> "T.xml")
    let assert Ok(ast) = jack_tokenizer.to_ast(output_file <> "T.xml")
    let _ = semantic.compile_class(ast, output_file <> ".vm")
    let _ = simplifile.delete(output_file <> "T.xml")
    //let _ = jack_tokenizer.to_xml(output_file <> "T.xml", output_file <> ".xml")

    Ok("")
  })
}
