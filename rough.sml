
fun readFile filename = 
    let val fd = TextIO.openIn filename
        fun readLine () = 
            case TextIO.inputLine fd of
                NONE => []
            |   SOME line => String.size line :: readLine ()
    in readLine () end


val a = readFile "test.mdt";;

