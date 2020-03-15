infix &&
fun op && (reader1, reader2) input =
    case reader1 input of
        NONE => NONE
      | SOME (result1, rest) =>
        (case reader2 rest of
             NONE => NONE
           | SOME (result2, rest) => SOME ((result1, result2), rest))

fun main () =
    let
      val io = TextIO.stdIn
      val instream = TextIO.getInstream io
      val getc = TextIO.StreamIO.input1
      val intReader = Int.scan StringCvt.DEC getc
      val ((a, b), instream) =
          valOf ((intReader && intReader) instream)
          handle Option.Option => raise Fail "bug: invalid input number"
      val () = TextIO.setInstream (io, instream)
    in
      if a mod 2 = 0 orelse b mod 2 = 0 then print "Even" else print "Odd"
    end
val () = main ()
