module DictionaryTrie
    
    type CTrie =
        | Leaf of char * string
        | Node of (char * string) * CTrie * CTrie * CTrie
    
    let empty = Leaf (System.Char.MinValue, "")

    let char2num char = (int char - int 'a') + 1 

    let trie2bool trie ch = 
        match trie with
        | Leaf (cha, _) -> if (cha = ch) then true else false
        | Node ((cha, _), _ , _ ,_) -> when (cha = ch) true 
        | Node ((cha, _), _ , _ ,_) -> when (cha > ch) true 
        | Node ((cha, _), _ , _ ,_) -> when (cha < ch) true 

    let lookup x = 
        let word = x
        let x = insertRec x
        match x with
        | Leaf (char, str) when word = str -> Leaf (char, str)
        | Node ((char, str), l, m, r) when word = str -> Node((char, str), l, m, r)

  
    let rec insert x =
        
        function
        | Node ((ch, str), l,m,r) when ch = System.Char.MinValue && x = "" ->
            Node((ch, str), l, m, r)
        
        | Leaf (ch, str) when ch = System.Char.MinValue && x = "" ->
            Leaf (ch, str)

        // This will change an already existing leaf to a node and continue down the middle to the new leaf 
        | Leaf (ch, str)   -> 
            Node((ch, x),  empty, insert (x) empty, empty)
        
        // This will continue down the left 
        | Node ((ch, str), l, m, r) when char2num ch < char2num x.[0]  ->
            match l with 
            | l when l = empty && String.length(x) = 1 -> Node((ch, str), Leaf(x.[0], word), m, r)
            | l when l = empty && String.length(x) > 1 -> Node((ch, str), insert (x) Node((x.[0],""),empty, empty, empty), m, r)
            | l when l != empty && String.length(x) >= 1 -> Node((ch, str), insert (x) l, m, r)
            //Node((ch, str), insert (x) l, m, r)

        // This will continue down the middle
        | Node ((ch, str), l, m, r) when char2num ch  = char2num x.[0]  ->
            match m, str with
            | m, _ when m = empty && String.length(x) = 1 -> Node((ch, str), l ,Leaf(x.[0], word), r)
            | m, _ when m = empty && String.length(x) > 1 -> Node((ch, str), l,insert (x) Node((x.[0],""), empty, empty, empty), r)
            | m, _ when m != empty && String.length(x) >= 1 -> Node((ch, str), l, insert (x) m, r)
            | _, str when str = "" && String.length(x[0]) = 0 -> Node((ch, word), l, m, r) 
        
        // This will continue down the right
        | Node ((ch, str), l, m, r) when char2num ch > char2num x.[0]  ->
            match r with 
            | r when r = empty && String.length(x) = 1 -> Node((ch, str), Leaf(x.[0], word), m, r)
            | r when r = empty && String.length(x) > 1 -> Node((ch, str), l, m, insert (x) Node((x.[0],""),empty, empty, empty))
            | r when r != empty && String.length(x) >= 1 -> Node((ch, str), l, m, insert (x) r)

    
    
    let rec lookup x =
        function
        | Leaf b when x = 0u -> b
        
        | Leaf _ -> false
        
        | Node (b, _, _) when x = 0u -> b
        
        | Node (_, l, _) when x % 2u = 0u ->
            lookup (x / 2u) l
        
        | Node (_, _, r) ->
            lookup (x / 2u) r

    // alternative

    // type CTrie =
    //     | Node of Map<char, CTrie> * bool
    
    // let empty = Node (Map.empty, false)
    
    // let rec insert word (ctrie: CTrie) =
    //     match ctrie, word with
    //     | Node (children, _), "" -> Node (children, true)  // Mark the end of a valid word
    //     | Node (children, isWord), _ ->
    //         let char = word.[0]
    //         let restWord = word.Substring(1)
    //         let childTrie =
    //             match Map.tryFind char children with
    //             | Some child -> insert restWord child  // Recurse down the child if it exists
    //             | None -> insert restWord (Node (Map.empty, false))  // Create a new child if it doesn't
    //         Node (Map.add char childTrie children, isWord)  // Reconstruct the node with the updated map

    // let rec lookup word (ctrie: CTrie) =
    //     match ctrie, word with
    //     | Node (_, true), "" -> true  // If it's the end of the word and the node is marked as a word, it's valid
    //     | Node (_, false), "" -> false  // If it's not marked as a word, it's invalid
    //     | Node (children, _), _ ->
    //         let char = word.[0]
    //         let restWord = word.Substring(1)
    //         match Map.tryFind char children with
    //         | Some child -> lookup restWord child  // Recurse down the child
    //         | None -> false  // If the path doesn't exist, the word is invalid