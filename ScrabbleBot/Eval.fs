// Insert your updated Eval.fs file here from Assignment 7. All modules must be internal.

module internal Eval

    open StateMonad

    let add (sm1 : SM<int>) (sm2 : SM<int>) : SM<int> =
        sm1 >>= fun a ->
        sm2 >>= fun b ->
        ret (a + b)

    let div (sm1 : SM<int>) (sm2 : SM<int>) : SM<int> =
        sm1 >>= fun a ->
        sm2 >>= fun b ->
        match b with
        | 0 -> fail DivisionByZero // fails with DivisionByZero
        | _ -> ret (a / b)  // b is not zero and returns the division

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

    let rec arithEval (a : aExp) : SM<int> =
        match a with
        | N n -> ret n
        | V varName -> lookup varName
        | WL -> wordLength
        | PV expr -> arithEval expr >>= pointValue
        | Add (expr1, expr2) ->
            arithEval expr1 >>= fun val1 ->
            arithEval expr2 >>= fun val2 ->
            ret (val1 + val2)
        | Sub (expr1, expr2) ->
            arithEval expr1 >>= fun val1 ->
            arithEval expr2 >>= fun val2 ->
            ret (val1 - val2)
        | Mul (expr1, expr2) ->
            arithEval expr1 >>= fun val1 ->
            arithEval expr2 >>= fun val2 ->
            ret (val1 * val2)
        | Div (expr1, expr2) ->
            arithEval expr1 >>= fun val1 ->
            arithEval expr2 >>= fun val2 ->
            if val2 = 0 then fail DivisionByZero else ret (val1 / val2)
        | Mod (expr1, expr2) ->
            arithEval expr1 >>= fun val1 ->
            arithEval expr2 >>= fun val2 ->
            if val2 = 0 then fail DivisionByZero else ret (val1 % val2)
        | CharToInt cExpr -> charEval cExpr >>= fun c -> ret (int c)
    and charEval (c : cExp) : SM<char> =
        match c with
        | C ch -> ret ch
        | CV expr -> arithEval expr >>= characterValue
        | ToUpper cExpr -> charEval cExpr >>= fun ch -> ret (System.Char.ToUpper ch)
        | ToLower cExpr -> charEval cExpr >>= fun ch -> ret (System.Char.ToLower ch)
        | IntToChar expr -> arithEval expr >>= fun i -> ret (char i)
    and boolEval (b : bExp) : SM<bool> =
        match b with
        | TT -> ret true
        | FF -> ret false
        | AEq (expr1, expr2) ->
            arithEval expr1 >>= fun val1 ->
            arithEval expr2 >>= fun val2 ->
            ret (val1 = val2)
        | ALt (expr1, expr2) ->
            arithEval expr1 >>= fun val1 ->
            arithEval expr2 >>= fun val2 ->
            ret (val1 < val2)
        | Not bExpr ->
            boolEval bExpr >>= fun value ->
            ret (not value)
        | Conj (bExpr1, bExpr2) ->
            boolEval bExpr1 >>= fun val1 ->
            boolEval bExpr2 >>= fun val2 ->
            ret (val1 && val2)
        | IsVowel cExpr -> 
            charEval cExpr >>= fun ch ->
            let vowels = ['a'; 'e'; 'i'; 'o'; 'y'; 'u'; 'A'; 'E'; 'I'; 'O'; 'Y'; 'U']
            ret (List.contains ch vowels)
        | IsLetter cExpr -> 
            charEval cExpr >>= fun ch ->
            ret (System.Char.IsLetter ch)
        | IsDigit cExpr -> 
            charEval cExpr >>= fun ch ->
            ret (System.Char.IsDigit ch)

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