module DictionaryTrie
    
    type CTrie =
        | Leaf of char * string
        | Node of (char * string) * CTrie * CTrie * CTrie;;
    
    let empty() = Leaf (System.Char.MinValue, "")
    
    let char2num char = (int char - int 'a') + 1 

    // let trie2bool trie ch = 
    //     match trie with
    //     | Leaf (cha, _) -> if (cha = ch) then true else false
    //     | Node ((cha, _), _ , _ ,_) when (cha = ch) -> true 
    //     | Node ((cha, _), _ , _ ,_) when (cha > ch) -> true 
    //     | Node ((cha, _), _ , _ ,_) when (cha < ch) -> true 

    // insert function
    // example to use insert in the terminal
    // let ac = empty |> insert "dogs" |> insert "dogge";;
    let insert word root =
        let rec insertRec x currentCtrie =
            match currentCtrie with

            | Leaf (ch, str) when x = "" ->
                Leaf (ch, word)
            
            | Node ((ch, str), l,m,r) when x = "" ->
                Node((ch, word), l, m, r)
            
            // This will change an already existing leaf to a node and continue down the middle to the new leaf
            | Leaf (ch, str)   -> 
                match ch with
                | ch when ch = x.[0] -> Node((ch, str),  empty(), insertRec (x.[1..]) (empty()), empty())
                | ch when ch = System.Char.MinValue && String.length (x) = 1 -> Leaf (x.[0], word) //Node((ch, str),  empty, insertRec (x.[1..]) empty, empty)
                
                | _ -> insertRec (x) (Node((ch, str),  empty(), empty(), empty()))

                // old with error
                // | _ -> Node((x.[0], str),  empty, insertRec (x.[1..]) empty, empty)

            // base case for ch != System.Char.MinValue
            | Node ((ch, str), l, m, r) when ch = System.Char.MinValue -> Node ((x.[0], str), l, insertRec (x.[1..]) m, r)
            

            // This will continue down the left
            | Node ((ch, str), l, m, r) when char2num x.[0] < char2num ch   ->
                match l with 
                | l when l = empty() && String.length(x) = 1 -> Node((ch, str), Leaf(x.[0], word), m, r)
                // | l when l = empty && String.length(x) > 1 -> Node((ch, str), insertRec (x.[1..]) (Node((x.[0],""), empty, empty, empty)), m, r) // Maybe remove
                | l when String.length(x) >= 1 -> Node((ch, str), insertRec (x) l, m, r)
                

            // This will continue down the right
            | Node ((ch, str), l, m, r) when char2num x.[0] > char2num ch ->
                match r with 
                | r when r = empty() && String.length(x) = 1 -> Node((ch, str), Leaf(x.[0], word), m, r)
                //| r when r = empty && String.length(x) > 1 -> Node((ch, str), l, m, insertRec (x.[1..]) (Node((x.[0],""), empty, empty, empty))) //perhaps this should be Node((ch, str), l, m, insertRec (x) r) instead
                | r when String.length(x) >= 1 -> Node((ch, str), l, m, insertRec (x) r)

            // if middle child node is empty insert
            | Node ((ch, str), l,m,r) when m = empty() -> Node((ch,str), l, insertRec x.[1..] (Leaf(x.[0],"")), r)

            // This will continue down the middle
            | Node ((ch, str), l, m, r) when char2num x.[0] = char2num ch   ->
                match m, str with
                | m, _ when m = empty() && String.length(x) = 1 -> Node((ch, str), l, Leaf(x.[0], word), r)
                //| m, _ when m = empty && String.length(x) > 1 -> Node((ch, str), l, insertRec (x.[1..]) (Node((x.[0],""), empty, empty, empty)), r)
                | m, _ when String.length(x) >= 1 -> Node((ch, str), l, insertRec (x.[1..]) m, r)
                | _, str when str = "" && String.length(x) = 0 -> Node((ch, word), l, m, r) 
            
            
            | _ -> insertRec (x) (empty())
            
        let checkRoot r =
            match r with
            | Leaf (ch, str)  when ch = System.Char.MinValue -> Leaf ('m', str) 
            | _ -> r
        let root = checkRoot root
        insertRec word root
    
    // Lookup function
    // continue from our example before, use lookup like so:
    // lookup "dogge" ac;;
    // TODO: figure out .ToLower on the words / chars later
    let lookup word root =
        let rec lookupRec (x : string) currentTrie =
            match currentTrie with
            | Leaf (ch, str) when str = word -> true
            | Node ((ch, str), _, _, _) when str = word -> true
            | Node ((ch, str), l, _, _) when char2num x.[0] < char2num ch -> lookupRec x l // going left
            | Node ((ch, str), _, _, r) when char2num x.[0] > char2num ch -> lookupRec x r // going right
            | Node ((ch, str), _, m, _) when char2num x.[0] = char2num ch -> lookupRec (x.[1..]) m // found correct letter, going middle
            | _ -> false // No matching word found
            // | _, "" -> false // return true only if we are at a complete word node
        
        lookupRec word root
    

    // val step : char -> Dict -> (bool * Dict) option
    // that given a character c and a
    // dictionary d takes one step down the trie and returns a tuple (b, d') where b is
    // true if traversing c completed a word and false otherwise, and where d' is
    // the next level of the trie.
    // Ctrie |> step 'd' |> snd |> step 'o' |> snd |> step 'g'
    let step char root =
        let rec step_rec char node = 
            match node with
            | Leaf (ch, str) when str <> "" -> Some (true, Leaf(ch, str))

            //go right
            | Node ((ch, str), l, m, r) when char2num char > char2num ch  -> 
                match r with
                //base case
                | Node ((ch, str), l, m, r) when str = "" -> step_rec char ( Node ((ch, str), l, m, r)) //rec it

                | Node ((ch, str), l, m, r) when str <> "" -> Some (true, Node ((ch, str), l, m, r))
                
                | Leaf (ch, str) when str <> "" -> Some (true, Leaf(ch, str))
                | Leaf (ch, str) when str = "" -> Some (false, Leaf(ch, str))
            
            //go left
            | Node ((ch, str), l, m, r) when char2num char < char2num ch  -> 
                match l with
                //base case
                | Node ((ch, str), l, m, r) when str = "" -> step_rec char ( Node ((ch, str), l, m, r))
                | Node ((ch, str), l, m, r) when str <> "" -> Some (true, Node ((ch, str), l, m, r))
                
                | Leaf (ch, str) when str <> "" -> Some (true, Leaf(ch, str))
                | Leaf (ch, str) when str = "" -> Some (false, Leaf(ch, str))

            //go middle
            | Node ((ch, str), l, m, r) when char2num char = char2num ch  -> 
                match m with
                //base case
                | Node ((ch, str), l, m, r) when str = "" -> Some (false, Node ((ch, str), l, m, r))
                | Node ((ch, str), l, m, r) when str <> "" -> Some (true, Node ((ch, str), l, m, r))
                
                | Leaf (ch, str) when str <> "" -> Some (true, Leaf(ch, str))
                | Leaf (ch, str) when str = "" -> Some (false, Leaf(ch, str))
            
            | _ -> Some (false, empty())

        step_rec char root



    // for testing
    //let dc =  empty() |> insert "dog" //|> insert "dogge" |> insert "come" |> insert "big" |> insert "zip" |> insert "yoyo" |> insert "dad"|> insert "boy"|> insert "year" |> insert "copper" |> insert "bulgur" |> insert "vortex" |> insert "cannopy" |> insert "terrordome" |> insert "jesper"|> insert "abe"|> insert "bee" |> insert "dyslexia" |> insert "jens"|> insert "me"|> insert "hear" |> insert "moose"
    //let stepper =  dc |> step 'd'

    // let stepper2 =  
    //     match dc |> step 'd' with
    //     | Some (_, trie1) -> 
    //         match trie1 |> step 'o' with
    //         | Some (_, trie2) ->
    //             match trie2 |> step 'g' with
    //             | Some (true, _) -> true
    //             | _ -> false
    //         | _ -> false
    //     | _ -> false