infix &&
fun op && (reader1, reader2) input =
    case reader1 input of
        NONE => NONE
      | SOME (result1, rest) =>
        (case reader2 rest of
             NONE => NONE
           | SOME (result2, rest) => SOME ((result1, result2), rest))

exception Invalid
fun changeGosenLoop papers man gosen sen =
    if papers < man + gosen + sen then raise Invalid
    else if gosen < 0 then raise Invalid
    else if papers = man + gosen + sen then (man, gosen, sen)
    else changeGosenLoop papers man (gosen - 1) (sen + 5)
fun changeLoop papers man gosen sen =
    if papers < man + gosen + sen then raise Invalid
    else if man < 0 then raise Invalid
    else if papers = man + gosen + sen then (man, gosen, sen)
    else changeGosenLoop papers man gosen sen
         handle Invalid => changeLoop papers (man - 1) (gosen + 2) sen
fun calcEachMaxPapers (papers, total) =
    let
      val initMan = total div 10
      val initGosen = (total mod 10) div 5
      val initSen = (total mod 10) mod 5
    in
      (initMan, initGosen, initSen)
    end
fun calcEachPapers (inputs as (papers, total)) =
    let val (man, gosen, sen) = calcEachMaxPapers inputs
    in  changeLoop papers man gosen sen end

fun main () =
    let
      val io = TextIO.stdIn
      val instream = TextIO.getInstream io
      val getc = TextIO.StreamIO.input1
      val intReader = Int.scan StringCvt.DEC getc
      val ((papers, total), instream) =
          valOf ((intReader && intReader) instream)
          handle Option.Option => raise Fail "bug: invalid input number"
      val (man, gosen, sen) = calcEachPapers (papers, total div 1000)
      val () = TextIO.setInstream (io, instream)
    in
      print (Int.toString man); print " ";
      print (Int.toString gosen); print " ";
      print (Int.toString sen)
    end
    handle Invalid => print "-1 -1 -1"
val () = main ()
