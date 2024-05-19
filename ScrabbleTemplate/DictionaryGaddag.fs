module DictionaryGaddag 

    (* Assignment 4 - GADDAG Dictionary *)
    type Dict = 
    | Node of Map<char, bool*Dict>

    let empty () = Node Map.empty<char,bool*Dict>

    let insert (str : string) dict =
        let rec insertChars chars dict =
            match chars, dict with
            | [], _ -> dict
            
            |x::xs, Node n -> 
                match Map.tryFind x n with
                | Some (bool, dic) when xs = [] -> Node (Map.add x (true, dic) n)
                | Some (bool, dic) ->  Node (Map.add x (bool, (insertChars xs dic)) n)
                | None when xs = [] -> Node (Map.add x (true, empty ()) n)
                | None -> Node (Map.add x (false, (insertChars xs (empty ())) ) n)

        // Shift string with specialChar    
        let lstA = []
        let rec createSpecial lstA (lstB : list<char>) dict = // lstB is basically chars but gets shifted thus the new name
            let tupLst = 
                match lstB with
                | [] -> (lstA, [])
                | x::xs -> ([x] @ lstA, xs) 
            let lstA = fst tupLst
            let lstB = snd tupLst
            let str = lstA @ [(char 0)] @ lstB

            let dict = insertChars str dict
            match lstB with 
            | [] -> dict
            | _ -> createSpecial lstA lstB dict

        match createSpecial lstA (Seq.toList str) dict with 
        | Node n -> Node n

    let step char dict =  
        match dict with
        | Node n -> Map.tryFind char n 


    let reverse dict = step (char 0) dict 

    let lookup (str : string) dict = 
        let rec recLook str dict = 
            match str with
            | [] -> false
            | x::xs when xs = [] -> 
                match step x dict with
                | Some (b, _) -> b
                | None -> false
            | x::xs -> 
                match step x dict with
                | Some (b, dict) -> recLook xs dict
                | None -> false

        let lookupChar chars dict =
            match chars, dict with
            | x::xs, Node n when xs = [] -> 
                match (step x dict) with
                | Some (b, _) -> b
                | None -> false
            | x::xs, n -> 
                match step x n with 
                | Some (_, dict) ->  match reverse dict with 
                                            | Some (b, dict) -> recLook xs dict
                                            | None -> false
                | None -> false

        lookupChar (Seq.toList str) dict
