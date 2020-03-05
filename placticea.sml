infix >&&
fun op >&& (f, reader) input = reader (f input)
fun listReader nil input = SOME (nil, input)
  | listReader (reader :: readers) input =
    case reader input of
        NONE => NONE
      | SOME (result, rest) =>
        case listReader readers rest of
            NONE => NONE
          | SOME (results, rest2) => SOME (result :: results, rest2)
local
  fun makeResult chars input =
      if null chars then NONE else SOME (String.implode (rev chars), input)
  fun scanImpl acc charis getc input =
      case getc input of
          NONE => makeResult acc input
        | SOME (c, newInput) =>
          if charis c then scanImpl (c :: acc) charis getc newInput
          else makeResult acc input
in
fun scan charis getc input = scanImpl nil charis getc input
end

fun skip charis getc input =
    case scan charis getc input of
        NONE => input
      | SOME (_, rest) => rest

fun sum nil = 0
  | sum (h::t) = h + sum t

fun main () =
    let
      val io = TextIO.stdIn
      val instream = TextIO.getInstream io
      val getc = TextIO.StreamIO.input1
      val intReader = Int.scan StringCvt.DEC getc
      val wordReader = scan Char.isAlpha getc
      val skipWSReader = skip Char.isSpace getc
      val wordReader = skipWSReader >&& wordReader
      val (nums, instream) =
          valOf (listReader [intReader, intReader, intReader] instream)
          handle Option.Option => raise Fail "bug: invalid input number"
      val added = sum nums
      val (message, instream) = valOf (wordReader instream)
          handle Option.Option => raise Fail "bug: invalid input string"
      val () = TextIO.setInstream (io, instream)
    in
      print (Int.toString added); print " "; print message
    end
val () = main ()
