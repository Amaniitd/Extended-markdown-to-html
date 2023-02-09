signature dict =
sig
    type dict
    val create : unit -> dict
    val add : dict -> string * string -> dict
    val lookup : dict -> string -> string
end

structure Dict : dict =
struct
    type dict = (string * string) list
    fun create () = []
    fun add (d: dict) (s: string * string) = s :: d
    fun lookup (d: dict) (s: string) = 
        case List.find (fn (k, v) => k = s) d of
            NONE => raise Fail "Not found"
          | SOME (_, v) => v
end

val url_dict = ref (Dict.create ());


fun parse_url_def (s: string) = 
    if String.isPrefix "[" s  andalso String.isSubstring "]: " s then 
        let  
            val tokens = String.tokens (fn c => c = #"]") s
            val url_name = String.substring (hd tokens, 1, String.size (hd tokens) - 1)
            val url = String.substring((hd (tl tokens)), 2, String.size (hd (tl tokens)) - 2)
        in
            url_dict := Dict.add (!url_dict) (url_name, url)
        end
    else ();;

fun mdt2html(filename: string) = 
    let

        fun writeFile filename content =
            let val fd = TextIO.openOut filename
                val _ = TextIO.output (fd, content) handle e => (TextIO.closeOut fd; raise e)
                val _ = TextIO.closeOut fd
            in () end

        fun readFile filename = 
            let val fd = TextIO.openIn filename
                fun readLine () = 
                    case TextIO.inputLine fd of
                        NONE => []
                    |   SOME line => line :: readLine ()
            in readLine () end
             
        fun removeLineBreaks [] = []
            | removeLineBreaks (x::xs) = if String.isSuffix "\n" x then String.substring(x, 0, String.size x - 1) :: removeLineBreaks xs else x :: removeLineBreaks xs

        fun isEmptyLine "" = true
            | isEmptyLine _ = false
        


        val html_header = "<html>\n<head>\n</head>\n<body> \n"
        val html_footer = "</body>\n</html>"

        val output_filename = String.substring(filename, 0, String.size filename - 3) ^ "html"

        fun removeLeftSpace "" = ""
            | removeLeftSpace s = if String.substring(s, 0, 1) = " " then removeLeftSpace (String.substring(s, 1, String.size s - 1)) else s

        fun noMore (_, "") = true
            | noMore (0, s) = if String.substring(s, 0, 1) = "#" then false else true
            | noMore (n, s) = if String.substring(s, 0, 1) = "#" then noMore (n-1, String.substring(s, 1, String.size s - 1)) else true

        fun isHeader  "" = false
            | isHeader s = if String.substring(s, 0, 1) = "#" andalso noMore (6, s) then true else false
        
        fun isList "" = false
            | isList s = 
                let 
                    val charList = String.explode s
                    fun removeDigits [] = []
                        | removeDigits (x::xs) = if Char.isDigit x then removeDigits xs else x::xs
                    
                    fun checkDotAndSpace [] = false
                        | checkDotAndSpace (x::xs) = if x = #"." then if List.hd xs = #" " then true else false else false
                    
                    fun helper (x::xs) = if Char.isDigit x then checkDotAndSpace(removeDigits(xs)) else false
                in 
                    helper(charList)
            end 

        fun isHorizontalLine s = 
            let 
                val charList = String.explode s
                val count = ref 0
                fun helper [] = if !count < 3 then false else true 
                    | helper (x::xs) = 
                        if x = #"-" then 
                            (count := !count + 1; helper xs)
                        else 
                            if !count >= 3 then 
                                true
                            else 
                                false
            in 
                helper(charList)
            end

        fun updateHeader s = 
            let 
                val count_hash = ref 0
                val charList = String.explode s
                fun helper (x::xs) = 
                    if x = #"#" then 
                        (count_hash := !count_hash + 1; helper xs)
                    else 
                        if x = #" " then 
                            "<h" ^ Int.toString(!count_hash) ^ ">" ^ String.implode xs ^ "</h" ^ Int.toString(!count_hash) ^ ">"
                        else 
                            helper xs
            in 
                helper(charList)
            end
        
        val updated_content = ref []
        
        fun updateContent s_list = 
            let 
                val newParagraph = ref ""
                val newOrderedListItems = ref []
                val newUnorderedListItems = ref []
                fun helper [] = 
                    if !newParagraph <> "" then
                        (updated_content := (!updated_content)@["<p>"^(!newParagraph)^"</p>"];())
                    else ()
                    if !newOrderedListItems <> [] then
                        (updated_content := (!updated_content)@["<ol>"]@(!newOrderedListItems)@["</ol>"];())
                    else ()
                    if !newUnorderedListItems <> [] then
                        (updated_content := (!updated_content)@["<ul>"]@(!newUnorderedListItems)@["</ul>"];())
                    else ()
                    | helper (x::xs) = 
                        if isHeader x then 
                            if !newParagraph = "" then
                                (updated_content := (!updated_content)@[updateHeader x]; helper xs)
                            else 
                                (updated_content := (!updated_content)@["<p>"^(!newParagraph)^"</p>"]@[updateHeader x]; helper xs)
                        else if isHorizontalLine x then 
                            (updated_content := (!updated_content)@["<hr>"]; helper xs)
                        else if isEmptyLine x then
                            if !newParagraph = "" then
                                (updated_content := (!updated_content)@["<p>"^(!newParagraph)^"</p>"]; helper xs)
                            else 
                                helper xs
                        else 
                            (newParagraph := !newParagraph ^ "\n" ^ x; helper xs)
                            
            in 
                helper(s_list)
            end

    in
        updateContent(removeLineBreaks(readFile filename));
        updated_content
    end;;


mdt2html("test.mdt");