module DictionaryTrie
    
    type CTrie =
        | Leaf of char * string
        | Node of (char * string) * CTrie * CTrie * CTrie
    
    let empty = Leaf  (System.Char.MinValue, "")

    let char2num char = (int char - int 'a') + 1 

    let trie2bool trie ch = 
        match trie with
        | Leaf (cha, _) -> if (cha = ch) then true else false
        | Node ((cha, _), _ , _ ,_) -> if (cha = ch) then true else false 
  
    let rec insert x =
        function
        | Leaf (char, str) when x = str -> Leaf (char, str)

        | Node ((char, str), l, m, r) when x = str -> Node((char, str), l, m, r)

        // This will change an already existing leaf to a node and continue down the middle to the new leaf 
        | Leaf (ch, str)   -> 
            Node((ch, x),  empty, insert (x.[1..]) empty, empty)
        
        // This will continue down the left 
        | Node ((ch, str), l, m, r) when trie2bool l  x.[0]  ->
            Node((ch, str), insert (x.[1..]) l, m, r)

        // This will continue down the middle
        | Node ((ch, str), l, m, r) when trie2bool m  x.[0]  ->
            Node((ch, str),  l, insert (x.[1..]) m, r)
        
        // This will continue down the middle
        | Node ((ch, str), l, m, r) when trie2bool r  x.[0]  ->
            Node((ch, str),  l,  m, insert (x.[1..]) r)
    
    let rec lookup x =
        function
        | Leaf b when x = 0u -> b
        
        | Leaf _ -> false
        
        | Node (b, _, _) when x = 0u -> b
        
        | Node (_, l, _) when x % 2u = 0u ->
            lookup (x / 2u) l
        
        | Node (_, _, r) ->
            lookup (x / 2u) r
