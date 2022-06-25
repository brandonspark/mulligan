signature FOO =
  sig
    structure A :
      sig
        type t = int
        type t2
      end

    structure B :
      sig
        type t2
        type t3
      end

    structure C :
      sig
        type t = int
        type t3
      end
    sharing A = B = C
  end

structure Foo : FOO =
  struct
    structure A =
      struct
        type t = int
        type t2 = string
      end

    structure B =
      struct
        type t2 = string
        type t3 = real

      end
    structure C =
      struct
        type t = int
        type t3 = real
      end
  end
