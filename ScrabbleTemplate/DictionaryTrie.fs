module DictionaryTrie
    
    type CTrie =
        | Leaf of char * string
        | Node of (char * string) * CTrie * CTrie * CTrie;;
    
    let empty() = Leaf (System.Char.MinValue, "")
    
    let char2num char = (int char - int 'a') + 1 

    let insert word root =
        let rec insertRec x currentCtrie =
            match currentCtrie with

            // this is just for the compiler, it should never match with these two
            | Leaf (ch, str) when x = "" ->
                Leaf (ch, word)
            | Node ((ch, str), l,m,r) when x = "" ->
                Node((ch, word), l, m, r)
            
            // This will change an already existing leaf to a node and continue down the middle to the new leaf
            | Leaf (ch, str)   -> 
                match ch with
                | ch when ch = x.[0] && String.length (x) = 1 -> Leaf (ch, word)
                | ch when ch = x.[0] -> Node((ch, str),  empty(), insertRec (x.[1..]) (empty()), empty())
                | ch when ch = System.Char.MinValue && String.length (x) = 1 -> Leaf (x.[0], word) //Node((ch, str),  empty, insertRec (x.[1..]) empty, empty)
                
                | _ -> insertRec (x) (Node((ch, str),  empty(), empty(), empty()))

            // base case for ch != System.Char.MinValue
            | Node ((ch, str), l, m, r) when ch = System.Char.MinValue -> Node ((x.[0], str), l, insertRec (x.[1..]) m, r)
            
            // This will continue down the left
            | Node ((ch, str), l, m, r) when char2num x.[0] < char2num ch   ->
                match l with 
                | l when l = empty() && String.length(x) = 1 -> Node((ch, str), Leaf(x.[0], word), m, r)
                | l when String.length(x) >= 1 -> Node((ch, str), insertRec (x) l, m, r)
                

            // This will continue down the right
            | Node ((ch, str), l, m, r) when char2num x.[0] > char2num ch ->
                match r with 
                | r when r = empty() && String.length(x) = 1 -> Node((ch, str), l, m, Leaf(x.[0], word))
                | r when String.length(x) >= 1 -> Node((ch, str), l, m, insertRec (x) r)

            // This will continue down the middle
            | Node ((ch, str), l, m, r) when char2num x.[0] = char2num ch   ->
                match m, str with
                | m, _ when String.length(x) = 1 -> Node((ch, word), l,  m, r)
                | m, _ when String.length(x) > 1 -> Node((ch, str), l, insertRec (x.[1..]) m, r)
                
                | _, str when str = "" && String.length(x) = 0 -> Node((ch, word), l, m, r) 
            
            | _ -> insertRec (x) (empty())
            
        let checkRoot r =
            match r with
            | Leaf (ch, str)  when ch = System.Char.MinValue -> Leaf ('M', str) 
            | _ -> r
        let root = checkRoot root
        insertRec word root
    
    
    let step char root =
        let rec step_rec char node = 
            match node with
            | Leaf (ch, str) when str <> "" && char = ch -> Some (true, Leaf(ch, str))

            //go right
            | Node ((ch, str), l, m, r) when char2num char > char2num ch  -> 
                match r with
                | r -> step_rec char ( r ) //rec it

            //go left
            | Node ((ch, str), l, m, r) when char2num char < char2num ch  -> 
                match l with
                | l -> step_rec char ( l )

            //go middle
            | Node ((ch, str), l, m, r) when char2num char = char2num ch  -> 
                match node with 
                | Node ((ch, str), l, m, r) when str = "" -> Some (false, m)
                | Node ((ch, str), l, m, r)  when str <> "" -> Some (true, ( m))
            
            | _ -> Some (false, empty())

        step_rec char root