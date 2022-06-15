(** Copyright (c) 2020 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure TerminalColors:
sig

  type color
  val white: color
  val black: color
  val red: color
  val green: color
  val blue: color
  val lightblue: color
  val softred: color
  val orange: color
  val purple: color
  val yellow: color

  val text : color -> string -> string

  (* channel values 0 to 1 *)
  val rgb: {red: real, green: real, blue: real} -> color

  (* hue in [0, 360) and sat/value in [0,1] *)
  val hsv: {h: real, s: real, v: real} -> color

  val background: color -> string
  val foreground: color -> string
  val bold: string
  val italic: string
  val underline: string
  val reset: string

end =
struct

  type color = {red: real, green: real, blue: real}
  fun rgb x = x

  val white = {red=1.0, green=1.0, blue=1.0}
  val black = {red=0.0, green=0.0, blue=0.0}
  val red = {red=1.0, green=0.0, blue=0.0}
  val green = {red=0.0, green=1.0, blue=0.0}
  val blue = {red=0.0, green=0.0, blue=1.0}

  fun hsv {h, s, v} =
    let
      val H = h
      val S = s
      val V = v

      (* from https://en.wikipedia.org/wiki/HSL_and_HSV#HSV_to_RGB *)
      val C = V * S
      val H' = H / 60.0
      val X = C * (1.0 - Real.abs (Real.rem (H', 2.0) - 1.0))

      val (R1, G1, B1) =
        if H' < 1.0 then      (C,   X,   0.0)
        else if H' < 2.0 then (X,   C,   0.0)
        else if H' < 3.0 then (0.0, C,   X)
        else if H' < 4.0 then (0.0, X,   C)
        else if H' < 5.0 then (X,   0.0, C)
        else                  (C,   0.0, X)

      val m = V - C
    in
      {red = R1 + m, green = G1 + m, blue = B1 + m}
    end

  val lightblue = hsv {h = 199.0, s = 0.8, v = 0.99}
  val softred = hsv {h = 0.0, s = 0.66, v = 0.83}
  val orange = hsv {h = 35.0, s = 0.85, v = 0.84}
  val purple = hsv {h=283.0,s=0.87,v=0.87}
  val yellow = hsv {h =59.0, s=0.74, v = 0.95}

  fun to256 channel =
    Real.ceil (channel * 255.0)

  val esc = "\027["

  fun background {red, green, blue} =
    esc ^ "48;2;" ^
    Int.toString (to256 red) ^ ";" ^
    Int.toString (to256 green) ^ ";" ^
    Int.toString (to256 blue) ^
    "m"

  fun foreground {red, green, blue} =
    esc ^ "38;2;" ^
    Int.toString (to256 red) ^ ";" ^
    Int.toString (to256 green) ^ ";" ^
    Int.toString (to256 blue) ^
    "m"

  val bold = esc ^ "1m"

  val italic = esc ^ "3m"

  val underline = esc ^ "4m"

  val reset = esc ^ "0m"

  fun text color s = foreground color ^ s ^ reset

end
