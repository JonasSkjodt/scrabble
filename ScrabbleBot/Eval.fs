// Insert your updated Eval.fs file here from Assignment 7. All modules must be internal.

module internal Eval

    open StateMonad

    
    let arithEv (a: SM<int>) (b: SM<int>) (f: (int -> int -> int)) =
        a >>= (fun x -> b >>= (fun y -> ret (f x y)))

    let add a b = arithEv a b (fun a b -> a + b)    
    let div a (b : SM<int>) =
        a >>= (
            fun a -> b >>= (
                fun b ->
                    if b <= 0 then fail DivisionByZero
                    else ret (a / b)
                    )
                )
    let modulo a b = 
        a >>= (
            fun a -> b >>= (
                fun b ->
                if b <= 0 then fail DivisionByZero
                else ret (a % b)
                )
            )  
    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsConsonant of cExp  (* check for constant <---- look for this one letter*)
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let rec arithEval a : SM<int> =  
        match a with
        | N n -> ret n 
        | V v -> lookup v
        | WL -> wordLength
        | PV pv -> arithEval pv >>= (fun pv -> pointValue pv)
        | Add (a, b) -> add (arithEval a) (arithEval b) 
        | Sub (a,b) -> arithEv (arithEval a) (arithEval b) (fun a b -> a - b)
        | Mul (a,b) -> arithEv (arithEval a) (arithEval b) (fun a b -> a * b)
        | Div (a,b) -> div (arithEval a) (arithEval b) 
        | Mod (a,b) -> modulo (arithEval a) (arithEval b)
        | CharToInt cti -> (charEval cti) >>= (fun cti -> ret (int cti))
    and charEval c : SM<char> = 
        match c with
        | C c -> ret c
        | CV cv -> arithEval cv >>= (fun cv -> characterValue cv)
        | ToUpper c -> charEval c >>= (fun c -> ret (System.Char.ToUpper c))
        | ToLower c -> charEval c >>= (fun c -> ret (System.Char.ToLower c))
        | IntToChar itc -> arithEval itc >>= (fun itc -> ret (char itc))

    let numComp a b f =
        arithEval a >>= (fun x -> arithEval b >>= (fun y -> ret (f x y)))

    let vowel c = 
        match (System.Char.ToLower c) with
        | 'a' -> true
        | 'e' -> true
        | 'i' -> true
        | 'o' -> true
        | 'u' -> true
        | _ -> false

    let rec boolEval b : SM<bool> = 
        match b with
        | TT -> ret true
        | FF -> ret false
        | AEq (a, b) -> numComp a b (fun a b -> a = b)
        | ALt (a, b) -> numComp a b (fun a b -> a < b)
        | Not b -> boolEval b >>= (fun b -> ret (not b))
        | Conj (b1, b2) -> boolEval b1 >>= (fun x -> boolEval b2 
                                                    >>= (fun y -> ret (x && y)))
        | IsVowel c -> charEval c >>= (fun c -> ret (vowel c))
        | IsLetter c -> charEval c >>= (fun c -> ret (System.Char.IsLetter c))
        | IsDigit c -> charEval c >>= (fun c -> ret (System.Char.IsDigit c)) 

    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    let rec stmntEval stmnt : SM<unit> = failwith "Not implemented"

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>

    let stmntToSquareFun stm = failwith "Not implemented"


    type coord = int * int

    type boardFun = coord -> Result<squareFun option, Error> 

    let stmntToBoardFun stm m = failwith "Not implemented"

    type board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
    }

    let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"