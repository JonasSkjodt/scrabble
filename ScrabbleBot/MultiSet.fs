﻿// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet

    type MultiSet<'a when 'a : comparison> =
        MultiSet of Map<'a, uint32> // replace with your type

    //empty : MultiSet<'a> that creates an empty multiset.
    let empty =
        MultiSet Map.empty

    //isEmpty : MultiSet<'a> -> bool that given a multiset s, returns true if a s is
    //empty, and false otherwise.
    let isEmpty (MultiSet s) =
        Map.isEmpty s

    //size : MultiSet<'a> -> uint32 that given a multiset s, returns the size of s (the sum of the number of occurrences of all elements in s).
    //Map.values s extracts the counts from the map
    //https://fsharp.github.io/fsharp-core-docs/reference/fsharp-collections-seqmodule.html
    //Seq.sumBy Returns the sum of the results generated by applying the function to each element of the sequence.
    //(sum all the values as they are without any transformation)
    let size (MultiSet s) =
        Map.values s |> Seq.sumBy id
    
    //contains : 'a -> MultiSet<'a> -> bool that given an element a and a multiset s,
    //returns true if a is an element of s and false otherwise.
    let contains key (MultiSet(s)) =
        Map.containsKey key s

    //numItems : 'a -> MultiSet<'a> -> uint32 that given an element a and a multiset s, returns how many occurrences of a there are in s.
    //remember https://fsharp.github.io/fsharp-core-docs/reference/fsharp-core-optionmodule.html
    let numItems key (MultiSet(s)) =
        Map.tryFind key s |> Option.defaultValue (uint32 0)

    //add : 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a> that given an element a,
    //a number n, and a multiset s, returns the multiset that has a added to s n times.
    let add key n (MultiSet s) = 
        MultiSet (s.Add (key, (numItems key (MultiSet s)) + n ))

    //addSingle : 'a -> MultiSet<'a> -> MultiSet<'a> that given an element a and
    //a multiset s, returns s with a single instance of a added to it.
    //remember https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/basic-types
    //remember 1u is the 32-bit int with value of 1
    let addSingle key (MultiSet(s)) =
        add key 1u (MultiSet(s))
    
    //remove : 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a> that given an element a,
    //a number n, and a multiset s, returns the multiset with n occurrences of a from s
    //removed (remember that a multiset cannot have fewer than 0 entries of an element).
    //If s contains n or fewer occurrences of a then the result should contain
    //no occurrences of a.
    //remember https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/pattern-matching
    //Some(x) //x is the value
    let remove key n (MultiSet(s)) =
        match Map.tryFind key s with
        | Some(count) when count > n -> MultiSet (Map.add key (count - n) s)
        | _ -> MultiSet (Map.remove key s)

    //removeSingle : 'a -> MultiSet<'a> -> MultiSet<'a> that given an element a and
    //a multiset s returns the multiset where a single occurrence of a has been removed
    //from s. If s does not contain a then return s.
    let removeSingle key (MultiSet(s)) =
        remove key 1u (MultiSet(s))

    //fold : ('a -> 'b -> uint32 -> 'a) -> 'a -> MultiSet<'b> -> 'a that given a folding
    //function f, an accumulator acc and a multiset {(a1,x1), ..., (an,xn)},
    //returns f (... (f acc a1 x1) ...) an xn). Note that the folder function f takes
    //the number of occurrences of each element as an argument rather than calling f
    //that number of times. It is up to the user of the fold method to determine how
    //to handle multiple occurrences of the same element (this is better practice, and
    //it makes your life easier).
    //remember Map.fold https://stackoverflow.com/questions/65561733/need-f-map-fold-example
    let fold f acc  (MultiSet(s)) =
        Map.fold f acc s

    //foldBack : ('a -> uint32 -> 'b -> 'b) -> MultiSet<'a> -> 'b -> 'b that given a folding function f,
    //an accumulator a multiset {(a1,x1), ..., (an,xn)}, and an accumulator acc,
    //returns f a1 x1 (... (f an xn acc) ...). The same reasoning applies to foldBack as fold.
    let foldBack f (MultiSet(s)) acc  =
        Map.foldBack f s acc 
    
    //yellows
    //ofList : 'a list -> MultiSet<'a> that given a list lst returns a multiset containing exactly the elements of lst
    let ofList lst =
        List.fold (fun acc el -> addSingle el acc) empty lst

    //toList : MultiSet<'a> -> 'a list that given a multiset {(a1, x1), ..., (an, xn)} returns [a1; ..(x1 times).. a1; ...; an; ..(xn times)..; an]
    let toList s =
        foldBack (fun el count acc ->
            let rep = List.replicate (int32 count) el
            rep @ acc //concatenate the list with current acc, so the elemts are added to the end of acc
        ) s []


    //map : ('a -> 'b) -> MultiSet<'a> -> MultiSet<'b> that given a mapping function f and a multiset {(a1, x1), ..., (an, xn)} returns {(f a1, x1), ..., (f an, xn)}.
    //Note that the mapping function is only applied on the elements themselves, and not the number of times they occur. This is in line with how maps generally work - they change the payload, but not the size of a collection. Also note that this exercise is actually a bit trickier than usual as you cannot use the regular Map.map to solve this, as that map leaves the keys unchanged, and you are most likely using your keys to represent the elements of your multiset.
    let map (_ : 'a -> 'b) (_ : MultiSet<'a>) : MultiSet<'b> = empty

    let union (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = empty
    let sum (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = empty
    
    let subtract (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = empty
    
    let intersection (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = empty