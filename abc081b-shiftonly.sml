fun ** reader input =
    case reader input of
        NONE => NONE
      | SOME (result, rest) =>
        case ** reader rest of
            NONE => SOME ([result], rest)
          | SOME (results, rest2) => SOME (result :: results, rest2)

fun div2Count n inputs =
    if List.all (fn x => x mod 2 = 0) inputs
    then div2Count (n + 1) (map (fn x => x div 2) inputs)
    else n

fun main () =
    let
      val io = TextIO.stdIn
      val instream = TextIO.getInstream io
      val getc = TextIO.StreamIO.input1
      val intReader = Int.scan StringCvt.DEC getc
      val (num, instream) =
          valOf (intReader instream)
          handle Option.Option => raise Fail "bug: invalid input header"
      val (inputs, instream) =
          valOf ( ** intReader instream)
          handle Option.Option => raise Fail "bug: invalid inputs"
      val count = div2Count 0 inputs
      val () = TextIO.setInstream (io, instream)
    in
      print (Int.toString count)
    end
val () = main ()
