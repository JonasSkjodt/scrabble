// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet

    type MultiSet<'a when 'a : comparison> = R of Map<'a, uint32> // replace with your type

    let empty = R (Map.empty)

    let isEmpty (s : MultiSet<'a>) = 
        match s with
        | R(m) -> Map.isEmpty m

    let size (R (s)) = 
        match s with
        | m -> Map.fold (fun sumVal _ mapVal  -> sumVal + mapVal) 0u m

    let contains (a : 'a) (s : MultiSet<'a>) = 
        match s with
        | R(m) -> Map.containsKey a m

    let numItems (a : 'a) (s : MultiSet<'a>) = 
        match s with 
        | R(m) -> 
            match Map.tryFind a m with
            | Some elem -> elem
            | None -> 0u

    let add (a: 'a) (n : uint32) (s : MultiSet<'a>) : MultiSet<'a> = 
        match s with
        | R (m) -> R(m.Add (a, ((numItems a s) + n)))
        // (numItems a s) + n) get the current number of the key and adds n onto that number

    let addSingle (a : 'a) (s : MultiSet<'a>) : MultiSet<'a> = 
        match s with
        | R(m) -> add a 1u s
    
    let remove (a : 'a) (n : uint32) (s : MultiSet<'a>) : MultiSet<'a> = 
        let currentCount = numItems a s
        match s with
        | R (m) ->
            let newCount = currentCount - n
            if newCount > 0u then
                R (m.Add(a, newCount))
            else
                R (m.Remove a)

    let removeSingle (a : 'a) (s : MultiSet<'a>) : MultiSet<'a> = 
        match s with
        | R(m) -> remove a 1u s

    let fold (f : 'b -> 'a -> uint32 -> 'b) (x : 'b) (s : MultiSet<'a>) = 
        match s with
        | R (m) -> Map.fold f x m
    let foldBack (f : 'a -> uint32 -> 'b -> 'b) (s : MultiSet<'a>) (x : 'b) = 
        match s with
        | R (m) -> Map.foldBack f m x
    
    // yellow
    let rec ofList (lst : 'a list) : MultiSet<'a> = List.fold (fun acc elem -> addSingle elem acc) empty lst
    let toList (ms : MultiSet<'a>) : 'a list = foldBack (fun item count acc -> List.replicate (int count) item @ acc) ms []


    let map (_ : 'a -> 'b) (_ : MultiSet<'a>) : MultiSet<'b> = empty

    let union (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = empty
    let sum (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = empty
    
    let subtract (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = empty    
    let intersection (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = empty