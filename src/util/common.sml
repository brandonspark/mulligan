
structure Common =
  struct
    fun println s = print (s ^ "\n")
    fun suspend x = fn () => x

    fun with_refval r v f =
      let
        val old_val = !r
        val () = r := v
        val res = f ()
        val () = r := old_val
      in
        res
      end

    fun with_file filename (contents : string) f =
      let
        val file =
          Posix.FileSys.createf
            ( filename
            , Posix.FileSys.O_RDWR
            , Posix.FileSys.O.flags []
            , Posix.FileSys.S.irwxu )

        val () = Posix.FileSys.ftruncate (file, 0)
        val () = Posix.FileSys.ftruncate (file, Position.fromInt 0)

        (* reset length of file in case it already existed *)
        val () = Posix.FileSys.ftruncate (file, 0)
        val _ = Posix.IO.writeVec
          (file,
          (Word8VectorSlice.slice (Byte.stringToBytes contents, 0, NONE))
          )

        val _ = Posix.IO.close file

        val res = f file
      in
        res
      end

  end