
datatype warning =
    ParseWarning of (string * string list)
  | InvalidExt of string
  | InvalidFile of string
  | LexWarning of
       { filename : string
       , reason : string
       , pos : int
       , rest : char list
       }
  | GeneralWarning of
       { filename : string
       , reason : string
       , span : (int * int)
       }

datatype error =
    ParseError of (string * string list)
  | LexError of
       { reason : string
       , pos : int
       , rest : char list
       }

datatype signal =
    SigError of error
  | SigWarn of warning

signature ERROR =
  sig

    datatype warning = datatype warning
    datatype error = datatype error
    datatype signal = datatype signal

    exception Signal of signal

    val warn : ('a -> (warning -> 'a))

    val err : (error -> 'a)
  end
structure Error :> ERROR =
  struct
    structure TC = TerminalColors

    datatype warning = datatype warning
    datatype error = datatype error
    datatype signal = datatype signal

    exception Signal of signal

    fun specify_pos filename pos =
      let
        val instream = TextIO.openIn filename

        fun iterate line_num pos =
          case (TextIO.inputLine instream, pos) of
            (NONE, 0) => (line_num, 0)
          | (NONE, _) => raise Fail "error"
          | (SOME line, _) =>
              let
                val length = String.size line
              in
                if pos - length < 0 then
                  (line_num, pos)
                else
                  iterate (line_num + 1) (pos - length)
              end
        val (line_num, pos) = iterate 1 pos
      in
        ( TextIO.closeIn instream
        ; (line_num, pos)
        )
      end

    fun text color s = TC.foreground color ^ s ^ TC.reset
    fun orange s = text TC.orange s
    fun lightblue s = text TC.lightblue s

    fun source (filename, pos) =
      let
        val (line_num, pos) = specify_pos filename pos
      in
        (lightblue filename) ^ ":"
        ^ lightblue (Int.toString line_num) ^ ":"
        ^ lightblue (Int.toString pos) ^ ": "
      end

    fun warn x warning =
      ( print
        ( "=================================\n"
        ^ orange "Warning: "
        ^ ( case warning of
              ParseWarning (filename, rest) =>
                   "Parse error\n"
                 ^ lightblue filename ^ ": Failure to produce derived file, skipping...\n"
                 ^ "Remaining filestream: " ^ lightblue (String.substring (String.concatWith " " rest, 0, 25)) ^ "\n"
            | (InvalidExt filename) =>
                "Invalid extension\n"
                ^ lightblue filename ^ ": Expected .sml, .sig, .fun, or .cm extension instead.\n"

            | (InvalidFile filename) =>
                "Invalid file\n"
                ^ lightblue filename ^ ": Expected an extension to this file.\n"

            | (LexWarning {filename, reason, pos, rest}) =>
                "Lex error\n"
                ^ source (filename, pos) ^ reason ^ "\n"
                ^ "Remaining token stream: "
                  ^ String.concatWith " "  (List.map Char.toString (List.take (rest, 25))) ^ "\n"

            | GeneralWarning {filename, reason, span} =>
                "\n"
                ^ source (filename, #1 span) ^ reason ^ "\n"
          )
        ^ "---------------------------------\n\n"
        )
      ; x
      )

    fun err error = raise Signal (SigError error)
  end
