fun main () =
    let
      val cOpts =
          [
            TextIO.input1 TextIO.stdIn,
            TextIO.input1 TextIO.stdIn,
            TextIO.input1 TextIO.stdIn
          ]
      val bits = map ((fn x => x - 48) o ord o valOf) cOpts
      val cells = foldl (op +) 0 bits
    in
      print (Int.toString cells)
    end
val () = main ()
