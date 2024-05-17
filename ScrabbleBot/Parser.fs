// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open StateMonad
    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    let pIntToChar  = pstring "intToChar" <?> "intToChar"
    let pPointValue = pstring "pointValue" <?> "pointValue"

    let pCharToInt  = pstring "charToInt" <?> "charToInt"
    let pToUpper    = pstring "toUpper" <?> "toUpper"
    let pToLower    = pstring "toLower" <?> "toLower"
    let pCharValue  = pstring "charValue" <?> "charValue"

    let pTrue       = pstring "true" <?> "true"
    let pFalse      = pstring "false" <?> "false"
    let pIsDigit    = pstring "isDigit" <?> "isDigit"
    let pIsLetter   = pstring "isLetter" <?> "isLetter"
    let pIsVowel   = pstring "isVowel" <?> "isVowel"

    let pif       = pstring "if" <?> "if"
    let pthen     = pstring "then" <?> "then"
    let pelse     = pstring "else" <?> "else"
    let pwhile    = pstring "while" <?> "while"
    let pdo       = pstring "do" <?> "do"
    let pdeclare  = pstring "declare" <?> "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

    let spaces         = many whitespaceChar <?> "space"
    let spaces1        = many1 whitespaceChar <?> "space1"

    let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2 
    let (.>*>) p1 p2  = p1 .>> spaces .>> p2
    let (>*>.) p1 p2  = p1 .>> spaces >>. p2

    let parenthesise p = pchar '(' >*>. p .>*> pchar ')'
    let curlyBrackets p = pchar '{' >*>. p .>*> pchar '}'

    let pid =
        pchar '_' <|> pletter .>>. many (palphanumeric <|> pchar '_')
        //The |>> operator takes the result of the parser on the left-hand side (which is a tuple containing the first character and the remaining characters) and feeds it as input to the function on the right-hand side.
        |>> fun (a, b) -> System.String.Concat(a::b)
    
    let unop op a = op >*>. a
    let binop op p1 p2 = p1 .>*> op .>*>. p2 
    
    //termparse (using ProdParse to parse the operands, handles addition and subtraction)
    let TermParse, tref = createParserForwardedToRef<aExp>()
    //prodparse (uses atomparse  to parse the operands, stuff like multiplication, division, and other product-like operations)
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    //atomparse (basic elements like literal values (like numbers or strings). Variables. Parenthesized expressions, since they evaluate to a single value.)
    let AtomParse, aref = createParserForwardedToRef<aExp>()

    //Addition and subtraction
    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]

    //Multiplication, division, and modulo
    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]

    //negation, point value, variables, and integer literals
    let NParse   = pint32 |>> N <?> "Int"
    let ParParse = parenthesise TermParse
    let PVParse = unop pPointValue AtomParse |>> PV <?> "PV"
    let NegParse = unop (pchar '-') AtomParse |>> (fun a -> Mul ((N -1), a)) <?> "Neg"
    let VParse = pid |>> V <?> "V"
    

    let AexpParse = TermParse 
    let CParse, cref = createParserForwardedToRef<cExp>()

    let charParse = between (pchar ''') (pchar ''') (palphanumeric <|> whitespaceChar) |>> C <?> "CParse"
    let toUpperParse = unop pToUpper (parenthesise CParse) |>> ToUpper <?> "ToUpper"
    let toLowerParse = unop pToLower (parenthesise CParse) |>> ToLower <?> "ToLower"
    let intToCharParse = unop pIntToChar (parenthesise AexpParse) |>> IntToChar <?> "IntToChar"
    let charValueParse = unop pCharValue (parenthesise AexpParse) |>> CV <?> "CV"
    //remember to follow the grammar's precedence rules
    //the order of the parsers matter as it enforces the correct interpretation
    //look at the way the grammar is set up for cref:
    (*C ::= 'c'
        | charValue ( A )
        | intToChar ( A )
        | toUpper ( C )
        | toLower ( C )*)
    //now watch how choice is set up:
    do
        cref
        := choice [ charValueParse
                    intToCharParse
                    toUpperParse
                    toLowerParse
                    charParse ]
    let CharToIntParse = unop pCharToInt (parenthesise CParse) |>> CharToInt <?> "CharToInt"
    do
        aref
        := choice [ CharToIntParse
                    NegParse
                    PVParse
                    VParse
                    NParse
                    ParParse]

    let CexpParse = CParse

    let BexpParse = pstring "not implemented"

    let stmParse = pstring "not implemented"

    (* The rest of your parser goes here *)
    //TODO: Refactor this part and add comments
    // moved from  
    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    
    type square = Map<int, squareFun>
    
    //let parseSquareProg (sqp:Map<int,string>) = sqp |> Map.map (fun _ p -> (stmntToSquareFun (getSuccess (run stmntParse p))))

    // let parseBoardProg (s:string) (sqs:Map<int, square>) : boardFun2 =
    //     //printf "Inside parseboardProg: %A \n" sqs
    //     stmntToBoardFun (getSuccess (run stmntParse s)) sqs



    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }
    
    // Default (unusable) board in case you are not implementing a parser for the DSL.
    let mkBoard : boardProg -> board = fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}
