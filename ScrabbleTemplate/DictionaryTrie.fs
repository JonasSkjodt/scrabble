module DictionaryTrie
    
    type CTrie =
        | Leaf of char * string
        | Node of (char * string) * CTrie * CTrie * CTrie
    
    let empty = Leaf (System.Char.MinValue, "")
    
    //let sDict = (System.Char.MinValue, ""), empty, empty, empty
    
    let char2num char = (int char - int 'a') + 1 

    let trie2bool trie ch = 
        match trie with
        | Leaf (cha, _) -> if (cha = ch) then true else false
        | Node ((cha, _), _ , _ ,_) when (cha = ch) -> true 
        | Node ((cha, _), _ , _ ,_) when (cha > ch) -> true 
        | Node ((cha, _), _ , _ ,_) when (cha < ch) -> true 

    // let lookup x = 
    //     let word = x
    //     let x = insertRec x
    //     match x with
    //     | Leaf (char, str) when word = str -> Leaf (char, str)
    //     | Node ((char, str), l, m, r) when word = str -> Node((char, str), l, m, r)

    //TODO make root ctrie -> ctrie
    //let root = Node ((System.Char.MinValue, ""), empty, empty, empty )
    // string -> (string -> Ctrie -> Ctrie)


    // let ac = empty |> insert "dogs" |> insert "dogge";;
    let insert word root =
        let rec insertRec x =
            function
            | Node ((ch, str), l,m,r) when x = "" ->
                Node((ch, word), l, m, r)
            
            | Leaf (ch, str) when x = "" ->
                Leaf (ch, word)
            
            // This will change an already existing leaf to a node and continue down the middle to the new leaf
            | Leaf (ch, str)   -> 
                match ch with
                | ch when ch = x.[0] -> Node((ch, str),  empty, insertRec (x.[1..]) empty, empty)
                | ch when ch = System.Char.MinValue && String.length (word) = 1 -> Leaf (x.[0], word) //Node((ch, str),  empty, insertRec (x.[1..]) empty, empty)
                | _ -> Node((x.[0], str),  empty, insertRec (x.[1..]) empty, empty)

            // This will continue down the left
            | Node ((ch, str), l, m, r) when char2num ch < char2num x.[0]  ->
                match l with 
                | l when l = empty && String.length(x) = 1 -> Node((ch, str), Leaf(x.[0], word), m, r)
                | l when l = empty && String.length(x) > 1 -> Node((ch, str), insertRec (x.[1..]) (Node((x.[0],""), empty, empty, empty)), m, r)
                | l when String.length(x) >= 1 -> Node((ch, str), insertRec (x) l, m, r)

            // This will continue down the right
            | Node ((ch, str), l, m, r) when char2num ch > char2num x.[0]  ->
                match r with 
                | r when r = empty && String.length(x) = 1 -> Node((ch, str), Leaf(x.[0], word), m, r)
                | r when r = empty && String.length(x) > 1 -> Node((ch, str), l, m, insertRec (x.[1..]) (Node((x.[0],""), empty, empty, empty)))
                | r when String.length(x) >= 1 -> Node((ch, str), l, m, insertRec (x) r)

            // if middle child node is empty insert
            | Node ((ch, str), l,m,r) when m = empty -> Node((ch,str), l, insertRec x.[1..] (Leaf(x.[0],"")), r)

            // This will continue down the middle
            | Node ((ch, str), l, m, r) when char2num ch  = char2num x.[0]  ->
                match m, str with
                | m, _ when m = empty && String.length(x) = 1 -> Node((ch, str), l, Leaf(x.[0], word), r)
                | m, _ when m = empty && String.length(x) > 1 -> Node((ch, str), l, insertRec (x.[1..]) (Node((x.[0],""), empty, empty, empty)), r)
                | m, _ when String.length(x) >= 1 -> Node((ch, str), l, insertRec (x.[1..]) m, r)
                | _, str when str = "" && String.length(x) = 0 -> Node((ch, word), l, m, r) 
            
            
            | _ -> insertRec (x) empty
        
        insertRec word root
    
    let step word2 = "not implemented"

    let lookup word =
        let rec lookupRec x =
            function
            | Leaf (ch, str) when str = word -> Leaf (ch, str)

            | Node ((ch, str), _, _, _) when str = word -> Leaf (ch, str)
            
            | Node ((ch, str), l, m, r) when char2num ch < char2num x.[0]  -> Node ((ch, str), lookupRec (x.[1..]) l, m, r)

            | Node ((ch, str), l, m, r) when char2num ch > char2num x.[0]  -> Node ((ch, str),  l, m, lookupRec (x.[1..]) r)

            | Node ((ch, str), l, m, r) when char2num ch = char2num x.[0] -> Node ((ch, str),  l, lookupRec (x.[1..]) m, r)
        
            | _ -> Leaf ('<', "No word found")
        
        lookupRec word
        
        
        
        
        // | Leaf b when x = 0u -> b
        
        // | Leaf _ -> false
        
        // | Node (b, _, _) when x = 0u -> b
        
        // | Node (_, l, _) when x % 2u = 0u ->
        //     lookup (x / 2u) l
        
        // | Node (_, _, r) ->
        //     lookup (x / 2u) r
    


    // //boom
    // let rec findAndPrintWord word (trie: CTrie) =
    //     match trie with
    //     | Leaf (ch, str) -> 
    //         if str = word then printfn "%s" str // Found the word, print it
    //     | Node ((ch, str), l, m, r) ->
    //         if word.StartsWith(str) then
    //             // If the current path string of the node matches the starting substring of the word,
    //             // continue searching down the middle path
    //             findAndPrintWord (word.Substring(str.Length)) m
    //         else
    //             // Otherwise, choose the left or right path based on the character comparison
    //             if ch < word.[0] then findAndPrintWord word r
    //             else findAndPrintWord word l
    //     | _ -> () // If we reach an empty branch, do nothing

    // let rec trieToString trie =
    //     match trie with
    //     | Leaf (ch, str) -> sprintf "Leaf(%c, \"%s\")" ch str
    //     | Node ((ch, str), l, m, r) ->
    //         let lStr = trieToString l
    //         let mStr = trieToString m
    //         let rStr = trieToString r
    //         sprintf "Node((%c, \"%s\"), %s, %s, %s)" ch str lStr mStr rStr

    // // Now you can use this function to get a string representation of your trie and print it
    // let printTrie trie =
    //     let str = trieToString trie
    //     printfn "%s" str;;



    // let rec lookup word (trie: CTrie) : bool =
    //     match trie, word with
    //     | Leaf (ch, str), "" -> str = word  // If we're at a leaf with an empty word, check if we found the word
    //     | Node ((ch, str), l, m, r), "" -> str = word  // Same as above but for nodes
    //     | Leaf _, _ -> false  // If we're at a leaf but haven't finished the word, it's not here
    //     // | Leaf _, _ -> false
    //     //| Node ((ch, _), _, _, _), _ when ch > word.[0] -> false  // If the current node's char is greater, the word is not here
    //     | Node ((ch, _), l, _, _), _ when char2num ch < char2num word.[0] -> lookup word l  // If the current node's char is less, go left
    //     | Node ((ch, _), _, _, r), _ when char2num ch > char2num word.[0] -> lookup word r  // If the current node's char is more, go right
    //     | Node ((ch, str), _, m, _), _ when ch = word.[0] ->
    //         if word.Length = 1 && (str = word || str = "") then
    //             true  // If we're at the right node and it's the last letter, check the word
    //         else
    //             lookup word.[1..] m  // If there are more letters, go down the middle
    //     | _, _ -> false





// type GaddagNode =
//     | Leaf
//     | Node of Map<char, GaddagNode>

// let emptyGaddagNode = Node Map.empty

// let addWord (word: string) (root: GaddagNode) : GaddagNode =
//     let rec addRevPrefix currNode revPrefix restOfWord =
//         match revPrefix, restOfWord with
//         | "", "" -> currNode
//         | _ ->
//             let Node children = currNode
//             let nextChar = if revPrefix = "" then '+' else revPrefix.[0]
//             let newRevPrefix = if revPrefix = "" then "" else revPrefix.[1..]
//             let newNode =
//                 match children |> Map.tryFind nextChar with
//                 | Some childNode -> addRevPrefix childNode newRevPrefix restOfWord
//                 | None -> addRevPrefix emptyGaddagNode newRevPrefix restOfWord
//             Node (children |> Map.add nextChar newNode)
    
//     let rec addBranches currNode i =
//         if i = -1 then
//             addRevPrefix currNode (word.[i..]) ""
//         else
//             let revPrefix = word.[0..i] |> String.rev
//             let restOfWord = word.[i+1..]
//             addRevPrefix (addBranches currNode (i - 1)) revPrefix restOfWord
    
//     addBranches root (word.Length - 1)

// // Usage example:
// let root = emptyGaddagNode
// let updatedRoot = addWord "word" root


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