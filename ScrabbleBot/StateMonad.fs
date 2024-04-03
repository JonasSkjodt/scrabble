﻿// Insert your StateMonad.fs from Assignment 6 here. All modules must be internal.


module internal StateMonad

    type Error = 
        | VarExists of string
        | VarNotFound of string
        | IndexOutOfBounds of int
        | DivisionByZero 
        | ReservedName of string          

    type Result<'a, 'b>  =
        | Success of 'a
        | Failure of 'b

    type State = { vars     : Map<string, int> list
                   word     : (char * int) list 
                   reserved : Set<string> }

    type SM<'a> = S of (State -> Result<'a * State, Error>)

    let mkState lst word reserved = 
           { vars = [Map.ofList lst];
             word = word;
             reserved = Set.ofList reserved }

    let evalSM (s : State) (S a : SM<'a>) : Result<'a, Error> =
        match a s with
        | Success (result, _) -> Success result
        | Failure error -> Failure error

    let bind (f : 'a -> SM<'b>) (S a : SM<'a>) : SM<'b> =
        S (fun s ->
              match a s with
              | Success (b, s') -> 
                match f b with 
                | S g -> g s'
              | Failure err     -> Failure err)


    let ret (v : 'a) : SM<'a> = S (fun s -> Success (v, s))
    let fail err     : SM<'a> = S (fun s -> Failure err)

    let (>>=)  x f = bind f x
    let (>>>=) x f = x >>= (fun () -> f)

    let push : SM<unit> = 
        S (fun s -> Success ((), {s with vars = Map.empty :: s.vars}))

    //6.1, the exercise is asking to either use program failure, exception or ignore the case
    //this will take the program failure
    //for the expecption we'd just use a | [] -> failwith (exception)
    //ignoring it (which we should probably never do) would be something like
    // | [] -> Success((), s)
    let pop : SM<unit> = //the StateMonad with an updateable <unit> value
        S (fun s ->
            match s.vars with
            //handles the empty stack, its value gives the custom error
            //should probably have been given a more specific name than divisionbyzero in type Error???
            //fail Error.DivisionByZero creates a Failure result with the DivisionByZero error from the Error type.
            //evalSM propagates this Failure result back to the calling code, and says its an empty stack error.
            | [] -> evalSM s (fail Error.DivisionByZero)
            //if s.vars is not empty, remove the top env from the stack
            //and update the state with the remaining stack, the return a success
            | env :: stack-> Success((), {s with vars = stack}))
    
    //remember
    //"Monads in F# are a way to structure computations that might succeed with a value,
    //fail with an error, or involve side effects, all wrapped in a uniform type."
    let wordLength : SM<int> =
        S (fun s ->
            match s.word with
            | [] -> Success(0, s) //word is empty, so its length is 0
            | _ -> Success(List.length s.word, s) //returns the length of the word
        )

    let characterValue (pos : int) : SM<char> =
        S (fun s ->
            match List.tryItem pos s.word with
            | Some (charValue, _) -> Success(charValue, s) //extract char from the tuple
            | None -> Failure(IndexOutOfBounds pos) // fail if position is invalid
        )

    let pointValue (pos : int) : SM<int> =
        S (fun s ->
            match List.tryItem pos s.word with
            | Some (_, pointValue) -> Success(pointValue, s) //extract int from the tuple
            | None -> Failure(IndexOutOfBounds pos) //fail if position is invalid
        )
    let lookup (x : string) : SM<int> = 
        let rec aux =
            function
            | []      -> None
            | m :: ms -> 
                match Map.tryFind x m with
                | Some v -> Some v
                | None   -> aux ms

        S (fun s -> 
              match aux (s.vars) with
              | Some v -> Success (v, s)
              | None   -> Failure (VarNotFound x))

    let declare (var : string) : SM<unit> = failwith "Not implemented"   
    let update (var : string) (value : int) : SM<unit> = failwith "Not implemented"      