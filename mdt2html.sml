signature dict =
sig
    type dict
    val create : unit -> dict
    val add : dict -> string * string -> dict
    val lookup : dict -> string -> string
    val have : dict -> string -> bool
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
    fun have (d: dict) (s: string) =
        case List.find (fn (k, v) => k = s) d of
            NONE => false
          | SOME _ => true
end

val url_dict = ref (Dict.create ());


fun parse_url_def (s: string) = 
    if s = "" then 
        false
    else if String.isPrefix "[" s  andalso String.isSubstring "]: " s then 
        let  
            val tokens = String.tokens (fn c => c = #"]") s
            val url_name = String.substring (hd tokens, 1, String.size (hd tokens) - 1)
            val url = String.substring((hd (tl tokens)), 2, String.size (hd (tl tokens)) - 2)
        in
            (url_dict := Dict.add (!url_dict) (url_name, url); true)
        end
    else 
        false

fun mdt2html(filename: string) = 
    let

        fun writeFile (filename, content) =
            let val fd = TextIO.openOut filename
                val _ = TextIO.output (fd, content) handle e => (TextIO.closeOut fd; raise e)
                val _ = TextIO.closeOut fd
            in () end

        fun readFile filename = 
            let val fd = TextIO.openIn filename
                fun readLine () = 
                    case TextIO.inputLine fd of
                        NONE => ["\n"]
                    |   SOME line => line :: readLine ()
            in readLine () end
             
        fun removeLineBreaks [] = []
            | removeLineBreaks (x::xs) = if String.isSuffix "\n" x then String.substring(x, 0, String.size x - 1) :: removeLineBreaks xs else x :: removeLineBreaks xs
        
        val html_header = "<html>\n<head>\n</head>\n<body> \n"
        val html_footer = "</body>\n</html>"

        val output_filename = String.substring(filename, 0, String.size filename - 3) ^ "html"

        fun noMore (_, "") = true
            | noMore (0, s) = if String.substring(s, 0, 1) = "#" then false else true
            | noMore (n, s) = if String.substring(s, 0, 1) = "#" then noMore (n-1, String.substring(s, 1, String.size s - 1)) else true

        fun isHeader  "" = false
            | isHeader s = if String.substring(s, 0, 1) = "#" andalso noMore (6, s) then true else false
        

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
                        "<h" ^ Int.toString(!count_hash) ^ ">" ^ String.implode (x::xs) ^ "</h" ^ Int.toString(!count_hash) ^ ">"
                        
            in 
                helper(charList)
            end
        
        val updated_content = ref []

        fun countTabs (s: string): int = 
            let 
                val charList = String.explode s
                val count = ref 0
                fun helper [] = !count
                    | helper (x::xs) = 
                        if x = #" " then 
                            (count := !count + 1; helper xs)
                        else 
                            (!count) div 2
            in 
                helper(charList)
            end
        
        fun removeSpaces (s: string): string = 
            let 
                val charList = String.explode s
                fun helper [] = []
                    | helper (x::xs) = 
                        if x = #" " then 
                            helper xs
                        else 
                            x::xs
            in 
                String.implode(helper(charList))
            end
        
        fun isBlockQuote (s: string): bool = 
            let 
                val charList = String.explode (removeSpaces (s))
            in
                if List.length charList < 2 then false
                else if (List.nth(charList, 0) = #">" andalso List.nth(charList, 1) = #" ") then true 
                else false
            end

        fun ifContainOnlySpaces (s: string) = 
            let 
                val charList = String.explode s
                fun helper [] = true
                    | helper (x::xs) = 
                        if Char.isSpace x then 
                            helper xs
                        else 
                            false
            in 
                helper(charList)
            end
        fun isOrderedList "" = false
            | isOrderedList s = 
                let 
                    val charList = String.explode (removeSpaces(s))
                    fun removeDigits [] = []
                        | removeDigits (x::xs) = if Char.isDigit x then removeDigits xs else x::xs
                    
                    fun checkDotAndSpace [] = false
                        | checkDotAndSpace (x::xs) = if x = #"." then if List.hd xs = #" " then true else false else false
                    
                    fun helper [] = false
                        | helper (x::xs) = if Char.isDigit x then checkDotAndSpace(removeDigits(xs)) else false
                in 
                    helper(charList)
            end 
        fun isUnorderedList (s: string) = 
            let 
                val charList = String.explode (removeSpaces(s))
                fun helper [] = false
                    | helper (x::xs) = if x = #"-" andalso (hd xs) = #" " then true else false
            in 
                helper(charList)
            end

        val updated_content = ref []
        fun head [] = ""
            | head (x::xs) = x
        
        fun isCode (s: string): bool = 
            let 
                val charList = String.explode s
                val count = ref 0
                fun helper [] = if !count = 0 then false else true
                    | helper (x::xs) = 
                        if Char.isSpace x then 
                            (count := !count + 1;
                            if !count = 6 then true else helper xs)
                        else
                            false
            in
                if isOrderedList s orelse isUnorderedList s then false else helper(charList)
            end
        
        
        
        fun updateBold(s: string): string = 
            (* double asterisk *)
            let 
                val charList = String.explode s
                val result = ref []
                val count = ref 0
                fun helper [] = !result
                    | helper (x::xs) = 
                        if x = #"*" andalso ((hd xs) = #"*") then 
                            (if !count = 0 then (count := 1; result := !result@["<strong>"]; helper (tl xs)) else (count := 0; result := !result@["</strong>"]; helper (tl xs)))
                        else
                            (result := !result @ [String.str x]; helper xs)
                fun merge [] = ""
                    | merge (x::xs) = x ^ merge xs
            in
                merge(helper(charList))
            end

        fun updateItalic(s: string): string = 
            (* single asterisk *)
            let 
                val charList = String.explode s
                val result = ref []
                val count = ref 0
                fun helper [] = !result
                    | helper (x::xs) = 
                        if x = #"*" andalso ((hd xs) <> #"*") then 
                            (if !count = 0 then (count := 1; result := !result@["<em>"]; helper xs) else (count := 0; result := !result@["</em>"]; helper xs))
                        else
                            (result := !result @ [String.str x]; helper xs)
                fun merge [] = ""
                    | merge (x::xs) = x ^ merge xs
            in
                merge(helper(charList))
            end
        
         
        fun updateUnderline (s: string): string = 
        (* "_underlined text_" produces <u>underlined text</u> *)
            let 
                val charList = String.explode s
                val result = ref []
                val count = ref 0
                fun helper [] = !result
                    | helper (x::xs) = 
                        if x = #"_" then 
                            if !count = 0 then 
                                (count := 1; result := !result@["<u>"]; helper xs) 
                            else 
                                (count := 0; result := !result@["</u>"]; helper xs)
                        else 
                            (result := !result @ [String.str x]; helper xs)
                fun merge [] = ""
                    | merge (x::xs) = x ^ merge xs
            in
                merge(helper(charList))
            end

        
        fun removePrefixFromOlist (s: string): string = 
            let 
                val charList = String.explode s
                fun helper [] = []
                    | helper (x::xs) = if x = #"." then xs else helper xs
            in 
                String.implode(helper(charList))
            end
        
        fun removePrefixFromUlist (s: string): string =
            let 
                val charList = String.explode s
                fun helper [] = []
                    | helper (x::xs) = if x = #"-" then xs else helper xs
            in 
                String.implode(helper(charList))
            end
        
        fun update_href (s: string): string = 
        (* [url](www.google.com) = <a href="www.google.com">markdown</a> *)
            let 
                val charList = String.explode s
                val result = ref ""
                val url = ref ""
                val markdown = ref ""
                val isUrl = ref false
                val isMarkdown = ref false
                fun helper [] = 
                    if !isUrl then 
                        (result := !result ^ "<a href=\"" ^ !url ^ "\">" ^ !markdown ^ "</a>"; ())
                    else ()
                    | helper (x::xs) = 
                        if x = #"[" then 
                            (isMarkdown := true; helper xs)
                        else if x = #"]" then 
                            (isMarkdown := false; helper xs)
                        else if x = #"(" then 
                            (isUrl := true; helper xs)
                        else if x = #")" then 
                            (isUrl := false; result := !result ^ "<a href=\"" ^ !url ^ "\">" ^ !markdown ^ "</a>"; url := ""; markdown := ""; helper xs)
                        else 
                            if !isUrl then 
                                (url := !url ^ String.str x; helper xs)
                            else if !isMarkdown then 
                                (markdown := !markdown ^ String.str x; helper xs)
                            else 
                                (result := !result ^ String.str x; helper xs)
            in 
                helper(charList); !result
            end
        
        fun isDirectHrefStart (charList) = 
            if List.length charList < 7 then false
            else if (List.nth(charList, 0) = #"h" andalso List.nth(charList, 1) = #"t" andalso List.nth(charList, 2) = #"t" andalso List.nth(charList, 3) = #"p") then true 
            else false
            

        fun updateDirectHref (s: string): string = 
            let 
                val charList = String.explode s
                val result = ref ""
                val url = ref ""
                val isUrlStart = ref false
                fun helper [] = 
                    ()
                | helper (x::xs) = 
                    if x = #"<" andalso (isDirectHrefStart xs) then 
                        if !isUrlStart then 
                            url := !url ^ String.str x
                        else
                            (isUrlStart := true; helper xs)
                    else if x = #">" then
                        if !isUrlStart then 
                            (isUrlStart := false; result := !result ^ "<a href=\"" ^ !url ^ "\">" ^ !url ^ "</a>"; url := ""; helper xs)
                        else 
                            (result := !result ^ String.str x; helper xs)
                    else 
                        if !isUrlStart then 
                            (url := !url ^ String.str x; helper xs)
                        else 
                            (result := !result ^ String.str x; helper xs)
            in
                helper(charList); !result
            end
            



        fun updateSingleLine (s: string) = 
            updateItalic(updateBold(updateUnderline(updateDirectHref(update_href s))))

        fun mergeSingleLineBreak (s_list: string list): string list = 
            let 
                val paragraphContent = ref ""
                val result = ref []
                val isLastCode = ref false
                fun helper [] = 
                    if !paragraphContent = "" then ()
                    else 
                        (result := !result @ [updateSingleLine(!paragraphContent)]; paragraphContent := "")
                    | helper (x::xs) = 
                        if x = "" then 
                            if isCode (head(xs)) then 
                                (isLastCode:= true;
                                paragraphContent := !paragraphContent ^ "<pre><code>" ^ (hd xs) ^ "</code></pre>\n"; helper (tl xs)
                                )
                            else 
                                if !isLastCode then 
                                    (isLastCode := false; helper xs)
                                else 
                                    (if !paragraphContent = "" then helper xs else (result := !result @ [updateSingleLine(!paragraphContent)]; paragraphContent := ""; helper xs))
                        else if isHeader x orelse isHorizontalLine x then 
                            (if !paragraphContent = "" then (result := !result @ [updateSingleLine(x)]; helper xs) else (result := !result @ [updateSingleLine(!paragraphContent)]; paragraphContent := ""; result := !result @ [updateSingleLine(x)]; helper xs))
                        else if isOrderedList (x) orelse isUnorderedList (x) orelse isBlockQuote (x) then 
                            (if !paragraphContent = "" then (paragraphContent := x ^ "\n"; helper xs) else (result := !result @ [updateSingleLine(!paragraphContent)]; paragraphContent := x ^ "\n"; helper xs))
                        else
                            (paragraphContent := !paragraphContent ^ x ^ "\n"; helper xs)
            in 
                helper(s_list); !result
            end


        fun update_url_variable (s: string): string = 
            (* variable will be inclosed in [] and is stored in url_dict *)
            let 
                val charList = String.explode s
                val result = ref ""
                val variable = ref ""
                val isVariable = ref false
                fun helper [] = 
                    if !isVariable then 
                        (result := !result ^ "(" ^ (Dict.lookup (!url_dict) (!variable)) ^ ")"; ())
                    else ()
                    | helper (x::xs) = 
                        if x = #"[" then 
                            (isVariable := true; helper xs)
                        else if x = #"]" then 
                            (* check if variable is in url_dict *)
                            if Dict.have (!url_dict) (!variable) then 
                                (result := !result ^ "(" ^ (Dict.lookup (!url_dict) (!variable)) ^ ")"; variable := ""; isVariable := false; helper xs)
                            else 
                                (result := !result ^ "[" ^ !variable ^ "]"; variable := ""; isVariable := false; helper xs)
                        else 
                            if !isVariable then 
                                (variable := !variable ^ String.str x; helper xs)
                            else 
                                (result := !result ^ String.str x; helper xs)
            in
                helper(charList); !result
            end
        
            


        fun updateContent s_list = 
            let 
                val list_level = ref ~1
                val blockQuoteLevel = ref ~1
                val list_type = ref []
                fun closeLists (i: int, Ltype:string) = 
                    if i < 0 then ()
                    else if i = 0 then
                        if Ltype <> (hd (!list_type)) then 
                            if (hd (!list_type)) = "ol" then 
                                (updated_content := !updated_content @ ["</ol>"]@["<ul>"]; list_type := "ul"::(tl (!list_type));())
                            else 
                                (updated_content := !updated_content @ ["</ul>"]@["<ol>"]; list_type := "ol"::(tl (!list_type)); ())
                        else ()
                    else 
                        if (hd (!list_type)) = "ol" then 
                            (updated_content := !updated_content @ ["</ol>"]; list_type := tl (!list_type); closeLists (i-1, Ltype))
                        else 
                            (updated_content := !updated_content @ ["</ul>"]; list_type := tl (!list_type); closeLists (i-1, Ltype))
                
                

                fun openOlist (i: int) = 
                    if i <= 0 then ()
                    else 
                        (updated_content := !updated_content @ ["<ol>"]; list_type := "ol" :: (!list_type); openOlist (i-1))
                
                fun openUlist (i: int) =
                    if i <= 0 then ()
                    else 
                        (updated_content := !updated_content @ ["<ul>"]; list_type := "ul" :: (!list_type); openUlist (i-1))

                fun closeAllLists () =
                    if (!list_type) = [] then ()
                    else 
                        if (hd (!list_type)) = "ol" then 
                            (updated_content := !updated_content @ ["</ol>"]; list_type := tl (!list_type); closeAllLists ())
                        else 
                            (updated_content := !updated_content @ ["</ul>"]; list_type := tl (!list_type); closeAllLists ())
                
                fun closeAllBlockQuotes () = 
                    if !blockQuoteLevel = ~1 then ()
                    else 
                        (updated_content := !updated_content @ ["</blockquote>"]; blockQuoteLevel := !blockQuoteLevel - 1; closeAllBlockQuotes ())

                fun addBlockQuotes(count) = 
                    if count = 0 then ()
                    else 
                        (updated_content := !updated_content @ ["<blockquote>"]; addBlockQuotes (count-1))

                fun addCloseingBlockQuotes(count) = 
                    if count = 0 then ()
                    else 
                        (updated_content := !updated_content @ ["</blockquote>"]; addCloseingBlockQuotes (count-1))  

                fun removePrefixFromBlockQuote (s: string): string = 
                    let val s2 = removeSpaces s
                    in
                        String.substring (s2, 2, String.size s2 - 2)
                    end
                
                fun helper [] = (
                    closeAllLists ();
                    closeAllBlockQuotes ();())
                    | helper (x::xs) = 
                        if  x = "" then helper xs
                        else if isHeader x then 
                            (closeAllLists (); closeAllBlockQuotes();
                                list_level:= ~1; updated_content := !updated_content @ [updateHeader x]; helper xs)
                        else if isBlockQuote x then
                            let 
                                val count = countTabs x
                            in
                                closeAllLists ();
                                list_level := ~1;
                                if count > !blockQuoteLevel then 
                                    (addBlockQuotes (count - !blockQuoteLevel); blockQuoteLevel := count; updated_content := !updated_content @ ["<p>" ^ removePrefixFromBlockQuote x ^ "</p>"]; helper xs)
                                else if count < !blockQuoteLevel then 
                                    (addCloseingBlockQuotes (!blockQuoteLevel - count); blockQuoteLevel := count; updated_content := !updated_content @ ["<p>" ^ removePrefixFromBlockQuote x ^ "</p>"]; helper xs)
                                else 
                                    (updated_content := !updated_content @ ["<p>" ^ removePrefixFromBlockQuote x ^ "</p>"]; helper xs)                                    
                            end
                        else if isOrderedList (x) then 
                            let val count = countTabs x
                            in 
                                closeLists (!list_level - count, "ol");
                                openOlist (count - !list_level);
                                list_level := count;
                                updated_content := !updated_content @ ["<li><p>" ^ removePrefixFromOlist x ^ "</p></li>"];
                                helper xs
                            end    
                        else if isUnorderedList (x) then
                            let val count = countTabs x
                            in 
                                closeLists (!list_level - count, "ul");
                                openUlist (count - !list_level);
                                list_level := count;
                                updated_content := !updated_content @ ["<li><p>" ^ removePrefixFromUlist x ^ "</p></li>"];
                                helper xs
                            end
                        else if isHorizontalLine x then 
                            (closeAllLists (); closeAllBlockQuotes();
                                list_level:= ~1; 
                                updated_content := !updated_content @ ["<hr>"]; 
                                helper xs)
                            
                        else 
                            (closeAllLists(); closeAllBlockQuotes();
                                list_level:= ~1; 
                                updated_content := !updated_content @ ["<p>" ^ x ^ "</p>"]; 
                                helper xs
                            )
            in 
                helper(s_list)
            end

        fun mergeContent [] = ""
            | mergeContent (x::xs) = x ^ "\n" ^ mergeContent xs
        
        fun removeSpacesFromEmptyline [] = []
            | removeSpacesFromEmptyline (x::xs) = 
                let 
                    val empty = ""
                in
                    if(ifContainOnlySpaces x) then 
                        empty::(removeSpacesFromEmptyline(xs))
                    else 
                        x:: (removeSpacesFromEmptyline(xs))
                end


        fun handleURL (s_list : string list) = 
        
            let 
                val result = ref []
                val result2 = ref []
                fun helper [] = ()
                    | helper (x::xs) = 
                        if parse_url_def x then 
                            helper xs
                        else 
                            (result := !result @ [x]; helper xs)
                
                fun helper2 [] = ()
                    | helper2(x::xs) = 
                    (result2 := !result2 @ [update_url_variable x]; helper2 xs)


            in 
                helper(s_list);
                helper2(!result);
                !result2
            end
    
    in
        
        updateContent(mergeSingleLineBreak(removeSpacesFromEmptyline(removeLineBreaks(handleURL(readFile filename)))));
        writeFile (output_filename, html_header ^ mergeContent(!updated_content) ^ html_footer)
    end;;

