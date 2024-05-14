namespace ScrabbleClient

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication
open System.IO


open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.
//type tile = Set of char * int
module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.


    type state = {
        board           : Parser.board
        dict            : ScrabbleUtil.Dictionary.Dict
        playerNumber    : uint32
        hand            : MultiSet.MultiSet<uint32>   
        playerTurn      : uint32
        numberOfPlayers : uint32
        letterPlacement : Map<coord, uint32 * (char * int)>
    }

    let mkState b d pn h pT nOP lP= {board = b; dict = d;  playerNumber = pn; hand = h; playerTurn = pT; numberOfPlayers = nOP; letterPlacement = lP}

    let board st            = st.board
    let dict st             = st.dict
    let playerNumber st     = st.playerNumber
    let hand st             = st.hand
    let playerTurn st       = st.playerTurn
    let numberOfPlayers st  = st.numberOfPlayers
    let letterPlacement st  = st.letterPlacement
    

    let rec removeTiles (move : list<coord * (uint32 * (char * int))>) hand =
                    match move with
                    | (_, (tileID, _)) :: tail when MultiSet.contains tileID hand ->
                        removeTiles tail (MultiSet.removeSingle tileID hand)
                    | [] -> hand
                    | _ -> failwith "Tile played could not be found in player hand"

    let rec addNewTiles newPieces hand =
                    match newPieces with
                    | newTile :: tail -> addNewTiles tail (MultiSet.addSingle (fst newTile) hand)
                    | [] -> hand

    let changeTurn pId numP= 
        match pId with
        | p when p = numP -> 1u
        | _ -> pId + 1u
        
    let private (|FoundValue|_|) key map = Map.tryFind key map

    let updateBoard tiles = 
        let updatePlacement tile coord st =
            match st.letterPlacement with
            | FoundValue coord _ ->  failwith "There is already a tile at the coord"  //map when Map.containsKey coord map -> failwith "There is already a tile at the coord"
            | _ -> {st with letterPlacement = Map.add coord tile st.letterPlacement} 

        Seq.foldBack (fun (coord, tile) acc -> updatePlacement tile coord acc) tiles 

    // let updateBoard tiles st = 
    //     let updatePlacement (coord, tile) map =
    //         match Map.tryFind coord map with
    //         | Some _ -> failwith "There is already a tile at the coord"
    //         | None -> Map.add coord tile map 
    //     updatePlacement tiles st.letterPlacement
    //     tiles
    //     |> Seq.fold (fun acc (coord, tile) -> updatePlacement (coord, tile) acc) st.letterPlacement
    //     |> fun updatedMap ->  updatedMap




module MudBot =
    open System.Threading

    let handToList (hand : MultiSet.MultiSet<uint32>) = hand |> MultiSet.toList // Map.toSeq |> List.ofSeq   
    
    // Allows for permutations even through async
    let rec createMove (letters : List<char>) dict =
        match letters with
        | [] -> false
        | x::xs -> 
            //let isValidMove = 
                match ScrabbleUtil.Dictionary.step x dict with
                | Some (valid, _) when valid = true -> valid
                | Some (_, node) -> createMove xs node
                | None -> false
            //isValidMove || createMove xs dict

    // let rec generatePermutations (input : List<char>) = 
    //     match input with
    //     | [] -> [[]]
    //     | _ ->
    //         input 
    //         |> List.collect (fun x -> 
    //             let remaining = List.filter (fun y -> y <> x) input
    //             generatePermutations remaining 
    //             |> List.map (fun sublist -> x :: sublist))

    // TODO: Change perm func to instead generate permutations in increasing order of length so first time is 2, then 3, then 4, etc
    // let rec permHand (hand : List<string>) (acc : List<string>) =
    //                     match hand with
    //                     | [] -> List.rev acc
    //                     | h::t -> 
    //                         let newAcc = List.map (fun x -> h + x) acc @ acc
    //                         permHand t newAcc
  
    let rec permutations (strings: string list) : string list =
        let rec insertAtAllPositions e l =
            match l with
            | [] -> [[e]]
            | h::t -> (e::l) :: (insertAtAllPositions e t |> List.map (fun x -> h::x))

        let rec aux acc xs =
            match xs with
            | [] -> acc
            | h::t ->
                let withH = acc |> List.collect (insertAtAllPositions h)
                aux (withH @ acc) t
                
        let allPerms = aux [[]] strings
        allPerms |> List.tail |> List.map (String.concat "") |> List.sort


    let rec check (perm:List<string>) dict =
        match perm with
        | [] -> ""
        | x::xs -> if createMove (Seq.toList x) dict then x else check (xs) dict

    

module Scrabble =
    open System.Threading

    // List of starting letter frequence from Wikipedia. Will help the bot with finding words easier.
    // https://en.wikipedia.org/wiki/Letter_frequency#Relative_frequencies_of_the_first_letters_of_a_word_in_English_language
    let startLetters = ['S' ; 'C' ; 'P' ; 'D' ; 'R' ; 'B' ; 'A' ; 'M' ; 'T' ; 'F' ; 'I' ; 'E' ; 'H' ; 'G' ; 'L' ; 'U' ; 'W' ; 'O' ; 'N' ; 'V' ; 'J' ; 'K' ; 'Q' ; 'Y' ; 'Z' ; 'X' ]

    // The pieces map is probably the single worst data type I have ever seen and used in my life. I would like to personally thank the person who made it :)

    let playGame cstream (pieces : Map<uint32,tile>) (st : State.state) =

        let rec aux (st : State.state) =
            if State.playerNumber st = State.playerTurn st then
                Thread.Sleep (1 * 500)
                Print.printHand pieces (State.hand st)
            //printfn "Updated hand: %A" (st.hand)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            //forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            // create empty move
            
            let move = "" //insert bot moves


            // TODO: Implement the while loop to keep creating new perms if the first one does not work (due to the board having pieces where it wants to play)
            // OR ASYNC
            if st.playerTurn = st.playerNumber then
                
                //change the console readline to find the necessary things for the bot
                //for instance, check the first letter, can the bot make a word from it? do it recursively
                //etc
                
                // old input reading
                // let input = System.Console.ReadLine()
                
                // Helper func to convert chars to alphanumeric values
                let char2num (char : char) = uint32((int char - int 'A') + 1)

                debugPrint "lol"

                // Check if board is empty and call this function with a unfinished word
                let move = 
                    if Map.isEmpty st.letterPlacement then  // if the board is empty
                            // Create permutations of the hand

                        // Convert alphanumeric hand to a list of strings
                        //System.Char.ConvertToUtf32
                        //https://stackoverflow.com/questions/5735698/how-to-convert-a-char-to-unicode-value-in-f
                        
                        let alphaNumericToStrings (hand : List<uint32>) = List.map (fun x -> string (fst ((Set.toList (Map.find x pieces)).Head))) hand

                        let startingHand = alphaNumericToStrings (MudBot.handToList st.hand)
                        //TODO: REMOVE THIS LATER
                        //let startingHand = ["D"; "D" ; "E" ; "E" ; "E" ; "E" ; "E"]

                        let result = MudBot.permutations startingHand 
                        
                        let moveString = MudBot.check result st.dict

                        // If moveString == "" then the bot should pass
                        if moveString = "" then
                            [(0,0),(0u,(System.Char.MinValue, 0))]
                        else

                        //let moveChar = List.map (fun x -> char (x)) (Seq.toList moveString)

                        let move =  List.map (fun x -> char2num x) (Seq.toList moveString)
                                

                        // Create a move going downwards
                        let rec createMove move acc : list<(int * int) * (uint32 * (char * int))>  = 
                            match move with
                            | [] -> []
                            | x::xs -> 
                                let letter = (Set.toList (Map.find x pieces)).Head
                                let tileID = x
                                let coord = (0, acc)
                                let newMove = (coord, (tileID, (letter)))
                                newMove :: createMove xs (acc+1)

                        createMove move 0                
                        
                    else
                    // TODO:
                    // FOR FINDING WORDS ON THE BOARD WHEN THE BOARD IS NOT EMPTY: 
                    // Keep a list of all perm (Based on the current tile that you grabbed from the board)
                    // Find the first word in the list and keep a int count of the index where the is
                    // If the word does not fit the board, find the next word in the list
                    // If you try all permutations for a tile on the board and none of them fit, then move to the next tile (generate new perm list)
                    
                    
                        let alphaNumericToStrings (hand : List<uint32>) = List.map (fun x -> string (fst ((Set.toList (Map.find x pieces)).Head))) hand
                    // Generate a move based on already existing tiles on the board
                        let startingHand = alphaNumericToStrings (MudBot.handToList st.hand)
                        //TODO: REMOVE AT THE END
                        //let startingHand = ["A"; "A" ; "E" ; "E" ; "E" ; "E" ; "E"]

                        // Convert letterPlacement to a list of coords and tiles
                        let letterPlacement = Map.toSeq st.letterPlacement |> List.ofSeq

                        // Convert starting hand to a list of strings
                        let startingHand = List.map (fun x -> string (char x) ) startingHand

                        
                        // Generate permutations of the hand for each letterPlacement
                        let rec perm (letterList : List<(coord * (uint32 * (char * int)))>) : string * coord = 
                            match letterList with
                            | [] -> "", (0, 0)
                            | (coord, (tileID, (c, p))) :: tail -> 
                                let c = string c 
                                let result = MudBot.permutations (startingHand @ [c])
                                let check = MudBot.check result st.dict
                                if check <> "" then 
                                    (check, coord)
                                else 
                                    perm tail
                        
                        // TODO: do async or while loop
                        let checkedValue = (perm letterPlacement)
                        let indexOfPlacedLetter = fst checkedValue |> Seq.toList |> List.findIndex (fun e -> e = fst (snd (Map.find (snd checkedValue) st.letterPlacement)))

                        if fst checkedValue <> "" then
                            let move = List.map (fun x -> char2num x) (Seq.toList (fst checkedValue))

                            // Decide if the move should be placed horizontally or vertically
                            let vertOrHor checkedValue = 
                                match checkedValue with
                                | (a, b) when (Map.containsKey (a,b-1) st.letterPlacement) && (Map.containsKey (a,b+1) st.letterPlacement) &&  (Map.containsKey (a-1,b) st.letterPlacement) && (Map.containsKey (a+1,b) st.letterPlacement) -> ("",0)
                                | (a,b) when not (Map.containsKey (a, b-1) st.letterPlacement) && not (Map.containsKey (a, b+1) st.letterPlacement) -> ("ver",a - indexOfPlacedLetter )
                                | (a,b) when not (Map.containsKey (a-1, b) st.letterPlacement) && not (Map.containsKey (a+1, b) st.letterPlacement) -> ("hor", b - indexOfPlacedLetter)
                            
                            
                            // let rec checkMoveOnBoard dir pivot_coord move start_coord =
                            //     match dir, move with
                            //     | _, [] -> Success "Valid move"
                            //     | "ver", _ -> if not Map.containsKey (start_coord, snd pivot_coord) st.letterPlacement then checkMoveOnBoard dir pivot_coord move (start_coord + 1) else Failure "Invalid move"
                            //     | "hor", _ -> if not Map.containsKey (fst pivot_coord, start_coord) st.letterPlacement then checkMoveOnBoard dir pivot_coord move (start_coord + 1) else Failure "Invalid move"

                                //   ABC(2,0)
                                //   B
                                //  ACT(0,2)


                                // Create a move going downwards
                                // list<(int * int) * (uint32 * (char * int))> 

                            // TODO: implement failure states for this function that then returns to the permutation part
                            let rec recursiveCheckerHor coord dir node =
                                match coord, dir with
                                | (a, b), "right" ->
                                    if Map.containsKey (a+1,b) st.letterPlacement then 
                                        match ScrabbleUtil.Dictionary.step (fst (snd (Map.find (a,b) st.letterPlacement))) node with
                                        | Some (_, node) -> recursiveCheckerHor (a+1,b) "right" node
                                        | None -> false
                                    else 
                                        match ScrabbleUtil.Dictionary.step (fst (snd (Map.find (a,b) st.letterPlacement))) node with
                                        | Some (bool, _) -> true
                                        | None -> false
                                | (a, b), "left" -> if Map.containsKey (a-1, b) st.letterPlacement then recursiveCheckerHor (a-1,b) "left" node else recursiveCheckerHor (a,b) "right" node


                            let rec recursiveCheckerVer coord dir node =
                                match coord, dir with
                                    | (a, b), "right" ->
                                        if Map.containsKey (a+1,b) st.letterPlacement then 
                                            match ScrabbleUtil.Dictionary.step (fst (snd (Map.find (a,b) st.letterPlacement))) node with
                                            | Some (_, node) -> recursiveCheckerHor (a+1,b) "right" node
                                            | None -> false
                                        else 
                                            match ScrabbleUtil.Dictionary.step (fst (snd (Map.find (a,b) st.letterPlacement))) node with
                                            | Some (bool, _) -> true
                                            | None -> false
                                    | (a, b), "left" -> if Map.containsKey (a-1, b) st.letterPlacement then recursiveCheckerHor (a-1,b) "left" node else recursiveCheckerHor (a,b) "right" node


                            // create move for vertical and horizontal
                            let rec createMove move acc dir coord pivotCoord : list<(int * int ) * (uint32 * (char * int))>  = 
                                match move, dir with
                                | [], _ -> []
                                | x::xs, _ when x = (match Map.tryFind pivotCoord st.letterPlacement with 
                                                            | Some (tileID, _) -> tileID 
                                                            | None -> 0u)
                                                            -> createMove xs (acc+1) dir coord pivotCoord
                                
                                | x::xs, "ver" -> 
                                    let letter = (Set.toList (Map.find x pieces)).Head
                                    let tileID = x
                                    let coord = (fst coord, acc)
                                    
                                    if Map.containsKey coord st.letterPlacement then [(0,0),(0u,(System.Char.MinValue, 0))]
                                    else
                                        let newMove = (coord, (tileID, (letter)))
                                        match (if Map.containsKey (fst coord-1, acc) st.letterPlacement && Map.containsKey (fst coord+1, acc) st.letterPlacement then recursiveCheckerHor coord "left" st.dict else true) with
                                        | true -> 
                                            newMove :: createMove xs (acc+1) dir coord pivotCoord
                                        | false -> 
                                            match (if Map.containsKey (fst coord-1, acc) st.letterPlacement then recursiveCheckerHor coord "left" st.dict else true) with
                                                    | true -> 
                                                        newMove :: createMove xs (acc+1) dir coord pivotCoord
                                                    | false -> match (if Map.containsKey (fst coord+1, acc) st.letterPlacement then recursiveCheckerHor coord "right" st.dict else true) with
                                                                | true ->
                                                                newMove :: createMove xs (acc+1) dir coord pivotCoord
                                                                | false -> [(0,0),(0u,(System.Char.MinValue, 0))]
                                            

                                | x::xs, "hor" ->
                                    let letter = (Set.toList (Map.find x pieces)).Head
                                    let tileID = x
                                    let coord = (acc, snd coord)
                                    if Map.containsKey coord st.letterPlacement then [(0,0),(0u,(System.Char.MinValue, 0))]  
                                    else
                                        let newMove = (coord, (tileID, (letter)))
                                        match (if Map.containsKey (acc, snd coord) st.letterPlacement then recursiveCheckerVer coord "up" st.dict else true) with
                                                    | true -> 
                                                        newMove :: createMove xs (acc+1) dir coord pivotCoord
                                                    | false -> [(0,0),(0u,(System.Char.MinValue, 0))]
                            if (fst (vertOrHor (snd checkedValue))) = "ver" then
                                createMove move ((snd (snd checkedValue))-1) "ver" (fst (snd checkedValue),snd (vertOrHor (snd checkedValue)))  (snd checkedValue)
                            elif (fst (vertOrHor (snd checkedValue))) = "hor" then
                                createMove move ((fst (snd checkedValue))-1) "hor" (snd (vertOrHor (snd checkedValue)), snd (snd checkedValue)) (snd checkedValue)
                            else
                                [(0,0),(0u,(System.Char.MinValue, 0))]

                        else 
                            [(0,0),(0u,(System.Char.MinValue, 0))]
                    
                
                let bool inp  = 
                    match inp with
                    | [(0,0), (0u,(System.Char.MinValue, 0))] -> false
                    | inp  -> true

                let boolReal = bool move
                
                let serverMsg move = 
                    match move with
                    | [(0,0),(0u,(System.Char.MinValue, 0))] -> SMPass
                    | _ -> SMPlay move

                debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                send cstream (serverMsg move)
            
                
            
            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.


            // pid = playerid
            // ms = letters on the board
            // points = points from a play
            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                // The message CMPlaySuccess is sent to the player who made the move

                // place letters on the board
                let boardWithLetters = State.updateBoard ms st

                debugPrint (sprintf "THIS MESSAGE IS A TEST: %A\n" boardWithLetters.letterPlacement)

                // create a new hand with old tiles removed and new tiles added
                let newHand = (State.removeTiles ms st.hand)
                let newHand' = State.addNewTiles newPieces newHand

                // update st with the new playernumber via changeTurn
                let newTurn = State.changeTurn st.playerNumber st.numberOfPlayers
                let st' = { st with hand = newHand' ; playerTurn = newTurn ; letterPlacement = boardWithLetters.letterPlacement}
                    
                aux st'

            // Successful play by you. Update your state (remove old tiles, add the new ones, etc.)
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)

                // place letters on the board
                let boardWithLetters = State.updateBoard ms st

                // update st with new playernumber and new points
                let newTurn = State.changeTurn pid st.numberOfPlayers
                let st' = {st with playerTurn = newTurn ; letterPlacement = boardWithLetters.letterPlacement}
                
                aux st'

            // Pass 
            | RCM (CMPassed (pid )) -> 
                  (* Passed play. Update your state *)

                let newTurn = State.changeTurn pid st.numberOfPlayers
                let st' = { st with playerTurn = newTurn }
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *) // this is seen from the other player

                let newTurn = State.changeTurn pid st.numberOfPlayers
                let st' = { st with playerTurn = newTurn } // This state needs to be updated

                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st


        aux st

    //tile
    //   type tile = Set
    //   property Count:  int
    //   property MinimumElement:  char * int
    //   property IsEmpty:  bool
    //   property MaximumElement:  char * int
    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet playerTurn numPlayers  Map.empty)
        
