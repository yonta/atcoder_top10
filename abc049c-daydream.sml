fun trimHead nil y = SOME (#"," :: y)
  | trimHead _ nil = NONE
  | trimHead (x :: xs) (y :: ys) = if x = y then trimHead xs ys else NONE

fun deleteWord _ nil = nil
  | deleteWord word (target as c :: cs) =
   case trimHead word target of
        NONE => c :: deleteWord word cs
      | SOME rest => deleteWord word rest

fun allDeleted l = List.all (fn c => c = #"," orelse c = #"\n") l

fun check str =
    let
      val clist = String.explode str
      fun app (f, x) = f x
      val doDeletes =
          [
            deleteWord (String.explode "eraser"),
            deleteWord (String.explode "erase"),
            deleteWord (String.explode "dreamer"),
            deleteWord (String.explode "dream")
          ]
      val deleted = foldl app clist doDeletes
    in
      allDeleted deleted
    end

fun main () =
    let
      val input = TextIO.inputAll TextIO.stdIn
      val checked = check input
    in
      if checked then print "YES" else print "NO"
    end
val () = main ()
