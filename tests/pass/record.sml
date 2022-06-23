
val r = {b = "hi", a = false, c = 150}

val res1 = (#a r) orelse true
val res2 = (#b r) ^ " there"
val res3 = (#c r) div 50

val nested = {x = {a = true, b = "that"}, y = 150}

val res4 = "do " ^ #b (#x nested)
