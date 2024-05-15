//ASYNC GONE; TIMEOUT GONE; 
//think about life
//think about the future
//think about the past
//think about the present

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
        //board           : Parser.board
        dict            : ScrabbleUtil.Dictionary.Dict
        playerNumber    : uint32
        hand            : MultiSet.MultiSet<uint32>   
        playerTurn      : uint32
        numberOfPlayers : uint32
        players       : Map<uint32, bool>
        //letterPlacement : Map<coord, uint32 * (char * int)>
        letterPlacement : Map<coord, uint32>
        square_fun    : coord -> bool
    }

    let mkState d pn h pT nOP players lP square_fun= {dict = d;  playerNumber = pn; hand = h; playerTurn = pT; numberOfPlayers = nOP; players = players; letterPlacement = lP; square_fun = square_fun}

    //let board st            = st.board
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

    let rec addNewTiles (newPieces : (uint32 * uint32) list ) hand =
                    match newPieces with
                    | [] -> hand
                    //| newTile :: tail -> addNewTiles tail (MultiSet.addSingle (fst newTile) hand)
                    | newTile :: tail -> addNewTiles tail (MultiSet.add (fst newTile) (snd newTile) hand)
    
    let changeTurn pId numP= 
        match pId with
        | p when p = numP -> 1u
        | _ -> pId + 1u
        
    let private (|FoundValue|_|) key map = Map.tryFind key map

    // let updateBoard tiles = 
    //     let updatePlacement tile coord st =
    //         match st.letterPlacement with
    //         | FoundValue coord _ ->  failwith "There is already a tile at the coord"  //map when Map.containsKey coord map -> failwith "There is already a tile at the coord"
    //         | _ -> {st with letterPlacement = Map.add coord tile st.letterPlacement} 

    //     Seq.foldBack (fun (coord, tile) acc -> updatePlacement tile coord acc) tiles 
    
    



module MudBot =
    
    open System.Threading.Tasks

    let rec check_other_words (pieces : Map<uint32, tile>) (st : State.state) (pos : coord) dx dy (word : string) = 
        match ((st.letterPlacement.ContainsKey pos)) with
        | false -> word
        | true -> match dx+dy with
                    |  1 -> check_other_words pieces st (fst pos + dx, snd pos + dy) dx dy word+(string (fst (Set.toList (pieces.Item ((st.letterPlacement.Item pos)))).[0]))
                    | -1 -> check_other_words pieces st (fst pos + dx, snd pos + dy) dx dy (string (fst (Set.toList (pieces.Item ((st.letterPlacement.Item pos)))).[0]))+word

    let is_valid_word (pieces : Map<uint32, tile>) (st : State.state) (pos : coord) (direction : bool) (l : char) (rack : MultiSet.MultiSet<uint32>) =
        let word = match direction with // true is horizontal and false is vertical
                    | true -> check_other_words pieces st (fst pos - 1, snd pos) -1 0 (string l) |> (check_other_words pieces st (fst pos + 1, snd pos) +1 0)
                    | false   -> check_other_words pieces st (fst pos, snd pos - 1) 0 -1 (string l) |> (check_other_words pieces st (fst pos, snd pos + 1) 0 +1)
        if String.length word = 1 then true 
        else Dictionary.lookup word st.dict

    let rec gen (direction : bool) (st : State.state) (anchor : coord) (pos : int32)(rack : MultiSet.MultiSet<uint32>) (arc : Dictionary.Dict) (pieces : Map<uint32, tile>) (word : string) (word_moves : (coord*(uint32*(char*int)))list) =    
        let plays = []
        let pos_coords = 
            match direction with
            | true  -> coord(fst anchor + pos, snd anchor)
            | false -> coord(fst anchor, snd anchor + pos)
        if not (st.square_fun pos_coords) then []
        else 
            if st.letterPlacement.ContainsKey pos_coords then 
                go_on direction pos pieces (st.letterPlacement.Item pos_coords) (MultiSet.toList rack) word word_moves (Dictionary.step (fst (Set.toList (pieces.Item (st.letterPlacement.Item pos_coords))).[0]) arc) anchor st
            else 
                MultiSet.fold (fun plays letter _ -> go_on direction pos pieces letter (MultiSet.toList (MultiSet.remove letter 1u rack)) word word_moves (Dictionary.step (fst (Set.toList (pieces.Item letter)).[0]) arc) anchor st @ plays) plays rack

    and go_on (direction : bool) (pos : int32) (pieces : Map<uint32, Set<char * int>>) (l : uint32) (rack : uint32 list) (word : string) (word_moves : (coord*(uint32*(char*int)))list) (new_arc : (bool*Dictionary.Dict) option) (anchor : coord) (st : State.state)= 
        let plays = []
        let letter = fst (Set.toList (pieces.Item l)).[0]
        let pos_coords = 
            match direction with
            | true  -> coord(fst anchor + pos, snd anchor)
            | false -> coord(fst anchor, snd anchor + pos)
        if not (is_valid_word pieces st pos_coords (not direction) letter (MultiSet.ofList rack)) then plays 
        else if pos <= 0 then
            let next_pos_coords = 
                match direction with
                | true  -> coord(fst anchor + pos - 1, snd anchor)
                | false -> coord(fst anchor, snd anchor + pos - 1)
            let new_word = letter.ToString() + word
            let word_moves = if st.letterPlacement.ContainsKey pos_coords then word_moves else (pos_coords, (l, ((fst (Set.toList (pieces.Item l)).[0]), (snd (Set.toList (pieces.Item l)).[0])))) :: word_moves
            let plays = 
                if Dictionary.lookup new_word st.dict && (not (st.letterPlacement.ContainsKey next_pos_coords)) then 
                    if List.length word_moves > 0 then word_moves :: plays else plays
                else
                    plays

            match new_arc with
            | Some arc -> 
                let plays = (gen direction st anchor (pos-1) (MultiSet.ofList rack) (snd arc) pieces new_word word_moves) @ plays
                match Dictionary.step (char 0) (snd arc) with
                | Some arc -> 
                    if  (not (st.letterPlacement.ContainsKey next_pos_coords)) then 
                        (gen direction st anchor 1 (MultiSet.ofList rack) (snd arc) pieces new_word word_moves) @ plays
                    else plays
                | None -> plays
            | None -> plays

        else
            let next_pos_coords = 
                match direction with
                | true  -> coord(fst anchor + pos + 1, snd anchor)
                | false -> coord(fst anchor, snd anchor + pos + 1)
            let new_word = word + letter.ToString()
            let word_moves = if st.letterPlacement.ContainsKey pos_coords then word_moves else (pos_coords, (l, ((fst (Set.toList (pieces.Item l)).[0]), (snd (Set.toList (pieces.Item l)).[0])))) :: word_moves
            let plays = 
                if Dictionary.lookup new_word st.dict && (not (st.letterPlacement.ContainsKey next_pos_coords)) then 
                    if List.length word_moves > 0 then word_moves :: plays else plays
                else
                    plays
            match new_arc with
            | Some arc -> (gen direction st anchor (pos+1) (MultiSet.ofList rack) (snd arc) pieces new_word word_moves) @ plays
            | None -> plays

    let genStart (st : State.state) (pieces : Map<uint32, tile>) (anchor : coord)= 
        let pos = 0
        let rack = st.hand
        let initArc = st.dict
        let word = ""
        let word_moves = []
        let direction = true // True is horizontal and False is vertical

        let horizontal_words =  async { return if (not (st.letterPlacement.ContainsKey (coord(fst anchor + 1, snd anchor)))) then gen direction st anchor pos rack initArc pieces word word_moves else []}
        let vertical_words = async {return if (not (st.letterPlacement.ContainsKey (coord(fst anchor, snd anchor + 1)))) then (gen (not direction) st anchor pos rack initArc pieces word word_moves) else []}

        (Async.RunSynchronously horizontal_words) @ (Async.RunSynchronously vertical_words)

    let rec move_value (move : (coord*(uint32*(char*int)))list) = 
        match move with
        | []      -> 0 
        | x :: xs -> x |> snd |> snd |> snd |> (+) (move_value xs)

    let move pieces (st : State.state) : (coord*(uint32*(char*int)))list = 
        let playable_moves = if st.letterPlacement.ContainsKey (coord(0, 0)) then 
                                let tasks = (Map.fold (fun lst anchor letter  -> async {return genStart st pieces anchor} :: lst) [] st.letterPlacement)
                                Array.fold (fun moves move -> move @ moves) [] (tasks |> Async.Parallel |> Async.RunSynchronously)
                             else genStart st pieces (0, 0)
        List.fold (fun (best_move : (coord*(uint32*(char*int32))) list) (move : 'b list) -> if move_value move > move_value best_move then move else best_move) [] playable_moves



module Scrabble =

    open System.Threading

    let rec updateBoard (ms : (coord*(uint32*(char*int)))list) (board : Map<coord, uint32>) = 
        match ms with
        | [] -> board
        | x :: xs -> (updateBoard xs board).Add (fst x, fst(snd x)) 

    // let rec removePieces hand ms = 
    //     match ms with
    //     | [] -> hand
    //     | x :: xs -> removePieces (MultiSet.removeSingle (fst (snd x)) hand) xs 

    // let rec addPieces (newPieces : (uint32*uint32) list) (hand : MultiSet<uint32>)  = 
    //     match newPieces with
    //     | [] -> hand
    //     | x :: xs -> addPieces xs (MultiSet.add (fst x) (snd x) hand)

    // List of starting letter frequence from Wikipedia. Will help the bot with finding words easier.
    // https://en.wikipedia.org/wiki/Letter_frequency#Relative_frequencies_of_the_first_letters_of_a_word_in_English_language
    let startLetters = ['S' ; 'C' ; 'P' ; 'D' ; 'R' ; 'B' ; 'A' ; 'M' ; 'T' ; 'F' ; 'I' ; 'E' ; 'H' ; 'G' ; 'L' ; 'U' ; 'W' ; 'O' ; 'N' ; 'V' ; 'J' ; 'K' ; 'Q' ; 'Y' ; 'Z' ; 'X' ]

    // The pieces map is probably the single worst data type I have ever seen and used in my life. I would like to personally thank the person who made it :)

    let playGame cstream (pieces : Map<uint32,tile>) (st : State.state) =

        let rec aux (st : State.state) =
                
            //printfn "Updated hand: %A" (st.hand)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            //forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            // create empty move
            
            let move = "" //insert bot moves
            
            if (not (st.players.Item st.playerTurn)) then aux (State.mkState st.dict st.playerNumber st.hand (((st.playerTurn)%st.numberOfPlayers)+1u) st.numberOfPlayers st.players  st.letterPlacement st.square_fun)
            else 
                if st.playerTurn = st.playerNumber then
                    Thread.Sleep (1 * 500)
                    Print.printHand pieces (State.hand st)
                    let move = MudBot.move pieces st

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
                    let ms_seq = List.map (fun (coord, (tileID, _))  -> (coord, tileID)) ms |> List.collect (fun (coord, tileID) -> [(coord, tileID); (coord, tileID)]) |> List.toSeq
                    let boardWithLetters = updateBoard ms st.letterPlacement


                    // create a new hand with old tiles removed and new tiles added
                    let newHand = (State.removeTiles ms st.hand)
                    let newHand' = State.addNewTiles newPieces newHand

                    // update st with the new playernumber via changeTurn
                    let newTurn = State.changeTurn st.playerNumber st.numberOfPlayers
                    let st' = { st with hand = newHand' ; playerTurn = newTurn ; letterPlacement = boardWithLetters}
                        
                    aux st'

                // Successful play by you. Update your state (remove old tiles, add the new ones, etc.)
                | RCM (CMPlayed (pid, ms, points)) ->
                    (* Successful play by other player. Update your state *)
                    let ms_seq = List.map (fun (coord, (tileID, _))  -> (coord, tileID)) ms |> List.collect (fun (coord, tileID) -> [(coord, tileID); (coord, tileID)]) |> List.toSeq

                    // place letters on the board
                    let boardWithLetters = updateBoard ms st.letterPlacement

                    // update st with new playernumber and new points
                    let newTurn = State.changeTurn pid st.numberOfPlayers
                    let st' = {st with playerTurn = newTurn ; letterPlacement = boardWithLetters}
                    
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

        let dict = dictf true // Uncomment if using a gaddag for your dictionary
        //let dict = dictf false // Uncomment if using a trie for your dictionary
        
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        let board = ScrabbleLib.simpleBoardLangParser.parseSimpleBoardProg boardP
        let rec mkPlayers (n : uint32) =
            match n with
            | 0u -> Map.empty
            | n -> (mkPlayers (n-1u)).Add (n, true)

        fun () -> playGame cstream tiles (State.mkState dict playerNumber handSet playerTurn numPlayers (mkPlayers numPlayers)   Map.empty board)
        
