infix &&
fun op && (reader1, reader2) input =
    case reader1 input of
        NONE => NONE
      | SOME (result1, rest) =>
        (case reader2 rest of
             NONE => NONE
           | SOME (result2, rest) => SOME ((result1, result2), rest))

fun calcPattern total max500 max100 max50 =
    let
      fun calcPattern50 total =
          if total mod 50 = 0 andalso total div 50 <= max50 then 1 else 0

      fun calcPattern100 total max100 =
          let
            val rest = total mod 100
            val p = calcPattern50 rest
          in
            p + calcPattern100 (total - 100) (max100 - 1)
          end


      val lim500 = total div 500
      val num500 = min (max500, lim500)
      val rest = total - num500 * 500
    in
    end
fun main () =
    let
      val io = TextIO.stdIn
      val instream = TextIO.getInstream io
      val getc = TextIO.StreamIO.input1
      val intReader = Int.scan StringCvt.DEC getc
      val ((((num500, num100), num50), total), instream) =
          valOf ((intReader && intReader && intReader && intReader) instream)
          handle Option.Option => raise Fail "bug: invalid input header"
      val pattern = calcPattern total num500 num100 num50
      val () = TextIO.setInstream (io, instream)
    in
      print (Int.toString pattern)
    end
val () = main ()
