
fun read_commands_file filename =
  let 
    val instream = TextIO.openIn filename 

    fun read_lines () = 
      case TextIO.readLine instream of
        NONE => [] 
      | SOME line => line :: read_lines () 
  in
    read_lines
  end