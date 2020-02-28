infix &&
fun op && (reader1, reader2) input =
    case reader1 input of
        NONE => NONE
      | SOME (result1, rest) =>
        (case reader2 rest of
             NONE => NONE
           | SOME (result2, rest) => SOME ((result1, result2), rest))

fun distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

fun checkTravel (start as ((startTime, startX), startY))
                (target as ((targetTime, targetX), targetY)) =
    let
      val dist = distance (startX, startY) (targetX, targetY)
      val time = targetTime - startTime
    in
      time >= dist andalso (time - dist) mod 2 = 0
    end

fun checkTravels reader instream (start as ((startTime, startX), startY)) =
    case reader instream of
        SOME (target as  ((targetTime, targetX), targetY), instream) =>
        if checkTravel start target
        then checkTravels reader instream target
        else (false, instream)
      | NONE => (true, instream)

fun main () =
    let
      val io = TextIO.stdIn
      val instream = TextIO.getInstream io
      val getc = TextIO.StreamIO.input1
      val intReader = Int.scan StringCvt.DEC getc
      val (n, instream) = valOf (intReader instream)
                          handle Option.Option => raise Fail "bug: invalid N"
      val lineReader = intReader && intReader && intReader
      val initState = ((0, 0), 0)
      val (checked, instream) = checkTravels lineReader instream initState
      val () = TextIO.setInstream (io, instream)
    in
      if checked then print "Yes" else print "No"
    end
val () = main ()
